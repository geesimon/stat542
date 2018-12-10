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