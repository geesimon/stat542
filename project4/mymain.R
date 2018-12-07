################ Load Environment ##################
# load necessary packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  #"catboost",
  "text2vec",
  "tokenizers",
  "glmnet",
  "randomForest",
  "xgboost",
  "kernlab"
)
library(catboost)

VOCAB_FILE = "myVocab.txt"
stop_words = c("i", "me", "my", "myself", 
               "we", "our", "ours", "ourselves", 
               "you", "your", "yours", 
               "their", "they", "his", "her", 
               "she", "he", "a", "an", "and",
               "is", "was", "are", "were", 
               "him", "himself", "has", "have", 
               "it", "its", "of", "one", "for", 
               "the", "us", "this")

# prep_fun = tolower
# tok_fun = tokenize_word_stems
# tok_fun = word_tokenizer

build_vocab <- function(review.data, stop.words = stop_words, word.count = 1000, tok.fun = word_tokenizer) {
  it.pos = itoken(review.data$review[review.data$sentiment == 1], 
                  preprocessor = tolower, 
                  tokenizer = tok.fun, 
                  ids = review.data$id,
                  progressbar = FALSE)
  pos.vocab = create_vocabulary(it.pos, stopwords = stop.words, ngram = c(1L, 4L))
  # pos.vocab = create_vocabulary(it.pos)
  pos.vocab = prune_vocabulary(pos.vocab, term_count_min = 5,
                               doc_proportion_max = 0.5,
                               doc_proportion_min = 0.001)
  
  it.neg = itoken(review.data$review[review.data$sentiment != 1], 
                  preprocessor = tolower, 
                  tokenizer = tok.fun, 
                  ids = review.data$id, 
                  progressbar = FALSE)
  neg.vocab = create_vocabulary(it.neg, stopwords = stop.words, ngram = c(1L, 4L))
  # neg.vocab = create_vocabulary(it.neg)
  neg.vocab = prune_vocabulary(neg.vocab, term_count_min = 5, 
                               doc_proportion_max = 0.5,
                               doc_proportion_min = 0.001)

  freq.all <- merge(pos.vocab[, c(1,2)], neg.vocab[, c(1,2)], by = 'term', all = T)
  freq.all$term_count.x[is.na(freq.all$term_count.x)] <- 0
  freq.all$term_count.y[is.na(freq.all$term_count.y)] <- 0
  freq.all$diff <- abs(freq.all$term_count.x - freq.all$term_count.y)
  
  alpha <- 2**7
  freq.all$ndsi <- abs(freq.all$term_count.x - freq.all$term_count.y)/(freq.all$term_count.x +
                                                                         freq.all$term_count.y + 
                                                                         2*alpha)
  freq.all <- freq.all[order(-freq.all$ndsi), ]
  
  return (freq.all$term[1:word.count])
}

glmnet_predict <- function(train.data, label, test.data){
  NFOLDS = 5
  my.cv = cv.glmnet(x = train.data, y = label, 
                    family = 'binomial', 
                    alpha = 0,
                    type.measure = "auc",
                    nfolds = NFOLDS)
  
  my.fit = glmnet(x = train.data, y = label, 
                  lambda = my.cv$lambda.min, family='binomial', alpha = 0)
  
  return (predict(my.fit, test.data, type = 'response')[,1])
}

xgboost_predict <- function(train.data, label, test.data) {
  xgb.model = xgboost(data = train.data, label=label,
                      objective = "binary:logistic", eval_metric = "auc",
                      eta = 0.09,
                      nrounds = 1200,
                      verbose = TRUE)
  
  return (predict(xgb.model, test.data, type="response"))
}

catboost_predict <- function(train.data, label, test.data) {
  fit_params <- list(iterations = 1233, #task_type = 'GPU',
                     #use_best_model = TRUE,
                     loss_function = 'Logloss',
                     #eval_metric = 'Logloss',
                     #logging_level = "Silent",
                     learning_rate = 0.09)
  learn_pool <- catboost.load_pool(train.data, label = label)
  cat.model <- catboost.train(learn_pool, params = fit_params)
  
  return (catboost.predict(cat.model, catboost.load_pool(test.data), 
                           prediction_type="Probability"))
}

make_prediction <- function(vocab, train.data, test.data, tok.fun = word_tokenizer){
  it_train = itoken(train.data$review, 
                    preprocessor = tolower, 
                    tokenizer = tok.fun, 
                    ids = train.data$id, 
                    progressbar = FALSE)
  
  # Note that most text2vec functions are pipe friendly!
  it_test = itoken(test.data$review, 
                   preprocessor = tolower, 
                   tokenizer = tok.fun, 
                   ids = test.data$id, 
                   progressbar = FALSE)
  
  vectorizer = vocab_vectorizer(create_vocabulary(vocab, ngram = c(1L, 4L)))
  
  dtm_train  = create_dtm(it_train, vectorizer)
  dtm_test = create_dtm(it_test, vectorizer)
  
  # define tfidf model
  tfidf = TfIdf$new()
  # fit model to train data and transform train data with fitted model
  dtm_train_tfidf = fit_transform(dtm_train, tfidf)
  # tfidf modified by fit_transform() call!
  # apply pre-trained tf-idf transformation to test data
  dtm_test_tfidf = transform(dtm_test, tfidf)
  
  
  #preds = glmnet_predict(dtm_train, train.data$sentiment, dtm_test)
  preds = xgboost_predict(dtm_train, train.data$sentiment, dtm_test)

  auc = glmnet:::auc(test.data$sentiment, preds)
  wrong.ids = test.data$new_id[(test.data$sentiment == 1) != (preds > 0.5)]
  
  return (list(AUC=auc, WrongIDs=wrong.ids))
}

#######################################
###### Train, Predict and Output ######
#######################################
f= file(VOCAB_FILE)
Sentiment.Vocab = readLines(f)
close(f)