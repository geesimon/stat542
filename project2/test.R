DEV.STORES = c(1)
DEV.DEPARTMENTS = c(1, 3, 5, 10, 13, 21, 22)

train = train[train$Store %in% DEV.STORES & train$Dept %in% DEV.DEPARTMENTS, ]
test = test[test$Store %in% DEV.STORES & test$Dept %in% DEV.DEPARTMENTS, ]

for (t in 1:num_folds) {
  cat("Fold:", t, "\n")
  # *** THIS IS YOUR PREDICTION FUNCTION ***
  mypredict()
  
  # Load fold file 
  # You should add this to your training data in the next call 
  # to mypredict()
  fold_file <- paste0('fold_', t, '.csv')
  new_test <- readr::read_csv(fold_file)
  new_test = new_test[new_test$Store %in% DEV.STORES & new_test$Dept %in% DEV.DEPARTMENTS, ]
  
  # extract predictions matching up to the current fold
  scoring_tbl <- new_test %>% 
    left_join(test, by = c('Date', 'Store', 'Dept'))
  
  # compute WMAE
  actuals <- scoring_tbl$Weekly_Sales
  preds <- select(scoring_tbl, contains('Weekly_Pred'))
  preds[scoring_tbl$IsHoliday.x,] <- preds[scoring_tbl$IsHoliday.x,] * 1.1
  weights <- if_else(scoring_tbl$IsHoliday.x, 5, 1)
  wae[t, ] <- colSums(weights * abs(actuals - preds)) / sum(weights)
  
  # print((actuals - preds)[scoring_tbl$IsHoliday.x,1])
  # print(mean((actuals - preds)[scoring_tbl$IsHoliday.x,1]))
  print(wae[t,])
}
