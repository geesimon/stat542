################ Load Environment ##################
# load necessary packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "text2vec",
  #"tokenizers",
  "glmnet",
  "xgboost"
)

trim_vocab_by_lasso <- function(all.review, all.vocab, word.count = c(2000), tokenizer = word_tokenizer) {
  it.all = itoken(all.review$review, 
                  preprocessor = tolower, 
                  tokenizer = tokenizer, 
                  ids = all.review$id,
                  progressbar = FALSE)
  
  vectorizer = vocab_vectorizer(all.vocab)
  dtm.all  = create_dtm(it.all, vectorizer)
  
  NFOLDS = 5
  my.cv = cv.glmnet(x = dtm.all, y = all.review$sentiment, 
                    family = 'binomial', 
                    alpha = 1,
                    type.measure = "auc",
                    nfolds = NFOLDS)
  
  my.fit = glmnet(x = dtm.all, y = all.review$sentiment, 
                  lambda = my.cv$lambda.1se, family='binomial', alpha = 1)
  
  betas = coef(my.fit)[-1, 1] #Remove inception
  
  vocabs = list()
  term.order = order(abs(betas), decreasing=TRUE)
  for (w in 1:length(word.count)){
    vocabs[[w]]= all.vocab$term[term.order[1:word.count[w]]]
  }
  
  return (vocabs)
}

trim_vocab_by_boosting <- function(all.review, all.vocab, word.count = c(2000), tokenizer = word_tokenizer) {
  it.all = itoken(all.review$review, 
                  preprocessor = tolower, 
                  tokenizer = tokenizer, 
                  ids = all.review$id, 
                  progressbar = FALSE)
  vectorizer = vocab_vectorizer(all.vocab)
  X_train  = create_dtm(it.all, vectorizer)
  Y_train = all.review$sentiment
  
  xgb.model = xgboost(data = X_train, label=Y_train,
                      objective = "binary:logistic", eval_metric = "auc",
                      eta = 0.09,
                      nrounds = 1642,
                      verbose = TRUE)
  
  # cv <- xgb.cv(data = X_train, label = Y_train,
  #               objective = "binary:logistic", eval_metric = "auc",
  #               early_stopping_rounds = 10,
  #               # max_depth = 6,
  #               nfold = 5, nrounds = 2000,
  #               eta = 0.09,
  #               verbose = TRUE)
  
  feature.importance = xgb.importance(model= xgb.model)
  ordered.terms = feature.importance$Feature
  vocabs = list()
  
  for (w in 1:length(word.count)){
    vocabs[[w]]= ordered.terms[1:word.count[w]]
  }
  
  return (vocabs)
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
                      nrounds = 1642,
                      verbose = TRUE)
  
  return (predict(xgb.model, test.data, type="response"))
}

model_functions = list(
  glmnet = glmnet_predict
  # Boosting = xgboost_predict
)

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
  
  result_auc = list()
  for (f in 1:length(model_functions)) {
    func_name = names(model_functions[f])
    preds = model_functions[[f]](dtm_train, train.data$sentiment, dtm_test)
    
    result_auc[[func_name]] = list(yhat=preds, auc=glmnet:::auc(test.data$sentiment, preds))
  }
  
  return (result_auc)
}

#######################################
###### Train, Predict and Output ######
#######################################
main <- function(){
  all = read.table("data.tsv",stringsAsFactors = F,header = T)
  splits = read.table("splits.csv", header = T)
  s = 1
  
  # My Code
  f= file("myVocab.txt")
  vocab = readLines(f)
  close(f)
  
  train = all[-which(all$new_id%in%splits[,s]),]
  test = all[which(all$new_id%in%splits[,s]),]
  pred = make_prediction(vocab, train, test)[[1]]$yhat
  
  output.data = cbind(new_id = test$new_id, prob = round(pred,2))
  write.csv(output.data, "mysubmission.txt", row.names = FALSE, quote = FALSE) 
}

if(!exists("SKIPMAIN")) main()