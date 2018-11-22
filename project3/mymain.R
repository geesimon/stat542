################ Load Environment ##################
# clean workspace
#rm(list = ls())

# load necessary packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "xgboost",
  "kernlab"
)

#########################################################################
# log-loss function
logLoss = function(y, p){
  if (length(p) != length(y)){
    stop('Lengths of prediction and labels do not match.')
  }
  
  if (any(p < 0)){
    stop('Negative probability provided.')
  }
  
  p = pmax(pmin(p, 1 - 10^(-15)), 10^(-15))
  mean(ifelse(y == 1, -log(p), -log(1 - p)))
}

###### Pre-processing and Feature Engineering Functions ######
convert_label <- function(train.data, test.data){
  train.data$loan_status[train.data$loan_status == "Charged Off"] = "Default"
  train.data$loan_status = as.numeric(train.data$loan_status)
  train.data$loan_status[train.data$loan_status == 3] = 0
  train.data$loan_status[train.data$loan_status == 2] = 1
  
  list(train = train.data, test = test.data)
}

fill_NA_by_vast <- function(train.column, test.column) {
  stats = sort(summary(train.column), decreasing = TRUE)
  
  train.column[is.na(train.column)] = names(stats)[1]
  test.column[is.na(test.column)] = names(stats)[1]
  
  list(train = train.column, test = test.column)
}

fill_NA_by_others <- function(train.column, test.column){
  levels(train.column) = c(levels(train.column), "MyOthers")
  test.column = factor(test.column, levels = levels(train.column))
  
  train.column[is.na(train.column)] = "MyOthers"
  test.column[is.na(test.column)] = "MyOthers"
  
  list(train = train.column, test = test.column)
}

fill_NA_by_mean <- function(train.column, test.column){
  avg = mean(train.column[!is.na(train.column)])
  train.column[is.na(train.column)] = avg
  test.column[is.na(test.column)] = avg

  list(train = train.column, test = test.column)
}

fill_NA_by_zero <- function(train.column, test.column){
  train.column[is.na(train.column)] = 0
  test.column[is.na(test.column)] = 0
  
  list(train = train.column, test = test.column)
}

handle_missing <- function(train.data, test.data){
  # r = fill_NA_by_others(train.data[, "emp_title"], test.data[, "emp_title"])
  # train.data[, "emp_title"] = r$train
  # test.data[, "emp_title"] = r$test
  
  r = fill_NA_by_others(train.data[, "emp_length"], test.data[, "emp_length"])
  train.data[, "emp_length"] = r$train
  test.data[, "emp_length"] = r$test
  
  r = fill_NA_by_mean(train.data[, "revol_util"], test.data[, "revol_util"])
  train.data[, "revol_util"] = r$train
  test.data[, "revol_util"] = r$test
  
  r = fill_NA_by_mean(train.data[, "dti"], test.data[, "dti"])
  train.data[, "dti"] = r$train
  test.data[, "dti"] = r$test
  
  r = fill_NA_by_mean(train.data[, "mort_acc"], test.data[, "mort_acc"])
  train.data[, "mort_acc"] = r$train
  test.data[, "mort_acc"] = r$test
  
  r = fill_NA_by_zero(train.data[, "pub_rec_bankruptcies"], test.data[, "pub_rec_bankruptcies"])
  train.data[, "pub_rec_bankruptcies"] = r$train
  test.data[, "pub_rec_bankruptcies"] = r$test
  
  list(train = train.data, test = test.data)
}

remove_variables <- function(train.data, test.data){
  # train.name = c('id','sub_grade', 'emp_title', 'title',
  #                'zip_code', 'addr_state', 'earliest_cr_line')
  # test.name = c('sub_grade', 'emp_title', 'title',
  #              'zip_code', 'addr_state', 'earliest_cr_line')
  train.name = c('id','emp_title', 'title', 'zip_code', 'earliest_cr_line')
  test.name = c('emp_title', 'title', 'zip_code', 'earliest_cr_line')
  
  train.data = train.data[, !colnames(train.data) %in% train.name]
  test.data = test.data[, !colnames(test.data) %in% test.name]
  
  list(train = train.data, test = test.data)
}

group_level <- function(train.column, test.column, group_count) {
  groups = list()
  stats = sort(summary(train.column), decreasing = TRUE)
  level.names = names(stats)
  level.interval = floor(sum(stats) / group_count)
  
  acc = 0
  group_count = 1
  item_names = c()
  for (i in 1:length(stats)){
    item_names = c(item_names, level.names[i])
    acc = acc + stats[i]
    if( acc > level.interval){
      groups[[group_count]] = item_names
      group_count = group_count + 1
      item_names = c()
      acc = 0
    }
  }
  
  groups
}

add_features <- function(train.data, test.data){
  #Change "earliest_cr_line" as month till 2019-1-1
  till = as.Date("1-1-2019", "%d-%m-%Y")
  
  full_date = paste("1-",train.data$earliest_cr_line, sep = "")
  train.data$earliest_cr_line_mon = floor((till - as.Date(full_date, "%d-%b-%Y")) / 30)
  
  full_date = paste("1-",test.data$earliest_cr_line, sep = "")
  test.data$earliest_cr_line_mon = floor((till - as.Date(full_date, "%d-%b-%Y")) / 30)
  
  list(train = train.data, test = test.data)
}

preprocess_data <- function(train.data, test.data){
  r = convert_label(train.data, test.data)
  r = add_features(r$train, r$test)
  r = handle_missing(r$train, r$test)
  r = remove_variables(r$train, r$test)

  return (r)
}

logreg_predict = function(train.data, test.data){
  model.fit = glm(loan_status ~ ., data = train.data, family = "binomial")
  predict(model.fit, test.data, type="response")
}

svm_predict = function(train.data, test.data) {
  X_train = train.data[, colnames(train.data) != 'Sale_Price']
  X_train = model.matrix(~., X_train)[, -1]
  
  Y_train = train.data$Sale_Price
  
  cv.out = cv.glmnet(X_train, Y_train, alpha = 1)
  
  X_test = test.data[, colnames(test.data) != 'Sale_Price']
  X_test = model.matrix(~. -PID, X_test)[, -1]
  #print(cv.out$lambda.min)
  predict(cv.out, s = cv.out$lambda.min, newx = X_test)
}

lasso_predict = function(train.data, test.data) {
  X_train = train.data[, colnames(train.data) != 'Sale_Price']
  X_train = model.matrix(~., X_train)[, -1]
  
  Y_train = train.data$Sale_Price
  
  cv.out = cv.glmnet(X_train, Y_train, alpha = 1)
  
  X_test = test.data[, colnames(test.data) != 'Sale_Price']
  X_test = model.matrix(~. -PID, X_test)[, -1]
  #print(cv.out$lambda.min)
  predict(cv.out, s = cv.out$lambda.min, newx = X_test)
}

xgb_predict = function(train.data, test.data) {
  X_train = train.data[, colnames(train_data) != 'Sale_Price']
  X_train = model.matrix(~., X_train)[,-1]
  
  Y_train = train_data$Sale_Price
  
  xgb_model = xgboost(data = X_train, label=Y_train, max_depth = 6,
                      eta = 0.03, nrounds = 500,
                      colsample_bytree = 0.6,
                      subsample = 0.75,
                      verbose = FALSE)
  
  X_test = test.data[, colnames(test.data) != 'Sale_Price']
  X_test = model.matrix(~. - PID, X_test)[,-1]
  predict(xgb_model, X_test)
}

train_predict = function(train.data, test.data, label.data, model.func, output.filename){
  pred = model.func(train.data, test.data)
  
  output = cbind(test.data$id, round(pred, 2))
  colnames(output) = c("id", "prob")
  
  write.csv(output, output.filename, row.names = FALSE, quote = FALSE)
  
  if(!is.null(label.data)){
    return (logLoss(label.data$y, pred))
  } else {
    return (0)
  }
}

#######################################
###### Train, Predict and Output ######
#######################################
set.seed(6682)

if (!exists("TRAIN_FILE_NAME")) {
  TRAIN_FILE_NAME = "train.csv"
  TEST_FILE_NAME = "test.csv"
}
train.data = read.csv(TRAIN_FILE_NAME)
test.data = read.csv(TEST_FILE_NAME)

if (exists("LABEL_FILE_NAME")){
  label.data = read.csv(LABEL_FILE_NAME)
} else {
  label.data = NULL
}

output_filenames = c("mysubmission1.txt", "mysubmission2.txt", "mysubmission3.txt")

model_functions = list(
  LogRegression = logreg_predict
  # SVM = svm_predict,
  # Lasso = lasso_predict,
  # Xgboost = xgb_predict
)

r = preprocess_data(train.data, test.data)

for (f in 1:length(model_functions)) {
  logloss = train_predict(r$train, r$test, label.data, model_functions[[f]], output_filenames[f])
  cat("Model:", names(model_functions)[f], "\t LogLoss=", logloss, "\n")
}