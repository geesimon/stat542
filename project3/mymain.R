################ Load Environment ##################
# clean workspace
#rm(list = ls())

# load necessary packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "xgboost",
  "kernlab"
)

###### Pre-processing and Feature Engineering Functions ######
convert_label <- function(train.data, test.data){
  train.data$loan_status[train.data$loan_status == "Charged Off"] = "Default"
  train.data$loan_status = as.numeric(train.data$loan_status)
  train.data$loan_status[train.data$loan_status == 3] = 0
  train.data$loan_status[train.data$loan_status == 2] = 1
  
  list(train = train.data, test = test.data)
}

handle_missing <- function(train.data, test.data){
  
  
  list(train = train.data, test = test.data)
}

remove_variables <- function(train.data, test.data){
  dropVars = c('id','sub_grade', 'emp_title', 'emp_length', 'title', 
               'zip_code', 'addr_state', 'earliest_cr_line')
  
  train.data = train.data[, !colnames(train.data) %in% dropVars]
  test.data = test.data[, !colnames(test.data) %in% dropVars]
  
  list(train = train.data, test = test.data)
}

preprocess_data <- function(train.data, test.data){
  r = convert_label(train.data, test.data)
  r = remove_variables(r$train, r$test)
  r = handle_missing(r$train, r$test)

  r
}

logreg_predict = function(train.data, test.data){
  model.fit = glm(loan_status ~ ., data = train.data, family = "binomial")
  predict(model.fit, test.data)
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

train_predict = function(train.data, test.data, model.func, output.filename){
  r = preprocess_data(train.data, test.data)
  
  yhat.test = model.func(r$train, r$test)
  
  output = cbind(test.data$id, yhat_test)
  colnames(output) = c("id", "prob")
  
  write.csv(output, output.filename, row.names = FALSE)
}

#######################################
###### Train, Predict and Output ######
#######################################
set.seed(6682)

TRAIN_FILE_NAME = "train1.csv"
TEST_FILE_NAME = "test1.csv"
LABEL_FILE_NAME = "label1.csv"

train.data = read.csv(TRAIN_FILE_NAME)
test.data = read.csv(TEST_FILE_NAME)
label.data = read.csv(LABEL_FILE_NAME)

# output_filenames = c("mysubmission1.txt", "mysubmission2.txt", "mysubmission3.txt")
# 
# model_functions = list(
#   SVM = svm_predict,
#   Lasso = lasso_predict,
#   Xgboost = xgb_predict
# )
# 
# for (f in 1:length(model_functions)) {
#   train_predict(train_data, test_data, model_functions[[f]], output_filenames[f])
# }