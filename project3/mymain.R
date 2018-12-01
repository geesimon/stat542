################ Load Environment ##################
# load necessary packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  #"catboost",
  "glmnet",
  "randomForest",
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

Winsorization = function (data, filter_by, fraction=.05)
{
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim = quantile(data[,filter_by], probs=c(fraction, 1-fraction))
  
  data[data[,filter_by]< lim[1],filter_by] = lim[1]
  data[data[,filter_by] > lim[2], filter_by] = lim[2]
  
  data
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
  stats = sort(summary(train.column, maxsum = length(levels(train.column))), 
               decreasing = TRUE)
  
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
  
  r = fill_NA_by_vast(train.data[, "zip_code"], test.data[, "zip_code"])
  train.data[, "zip_code"] = r$train
  test.data[, "zip_code"] = r$test
  
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

remove_features <- function(train.data, test.data){
  train.name = c('id','emp_title', 'title', 'grade' ,'earliest_cr_line', 'fico_range_high', 'fico_range_low')
  test.name = c('emp_title', 'title', 'grade' ,'earliest_cr_line', 'fico_range_high', 'fico_range_low')

    
  train.data = train.data[, !colnames(train.data) %in% train.name]
  test.data = test.data[, !colnames(test.data) %in% test.name]
  
  list(train = train.data, test = test.data)
}

group_levels <- function(train.column, test.column, fraction, group_count) {
  stats = summary(train.column, maxsum = length(levels(train.column)))
  frac.stats = summary(train.column[fraction], maxsum = length(levels(train.column)))
  ratio = sort(frac.stats / stats, decreasing = TRUE)
  
  groups = rep("NL", length(ratio))
  level.names = names(ratio)
  names(groups) = level.names
  level.interval = (ratio[1] - ratio[length(ratio)]) / group_count
  
  current.ratio = Inf
  group_count = 1
  
  for (i in 1:length(stats)){
    if (ratio[i] < current.ratio - level.interval) {
      group_name = paste("Group", group_count, sep = "")
      current.ratio = ratio[i]
      group_count = group_count + 1
    }
    groups[i] = group_name
  }
  
  train.column = groups[as.numeric(factor(train.column, levels=level.names))]
  train.column = factor(train.column)
  test.column = groups[as.numeric(factor(test.column, levels=level.names))]
  test.column = factor(test.column, levels=levels(train.column))
  
  list(train = train.column, test = test.column)
}

group_feature_levels <- function(train.data, test.data) {
  fraction = (train.data$loan_status == 1)
  
  # r = group_levels(train.data[, "sub_grade"], test.data[, "sub_grade"],fraction, 10)
  # train.data[, "sub_grade"] = r$train
  # test.data[, "sub_grade"] = r$test
  # 
  # r = group_levels(train.data[, "addr_state"], test.data[, "addr_state"],fraction, 10)
  # train.data[, "addr_state"] = r$train
  # test.data[, "addr_state"] = r$test
  
  r = group_levels(train.data[, "zip_code"], test.data[, "zip_code"],fraction, 10)
  train.data[, "zip_code"] = r$train
  test.data[, "zip_code"] = r$test
  
  list(train = train.data, test = test.data)
}

add_features <- function(train.data, test.data){
  #Convert "earliest_cr_line" as month till 2019-1-1
  lct <- Sys.getlocale("LC_TIME");
  Sys.setlocale("LC_TIME", "C")
  
  till = as.Date("1-1-2019", "%d-%m-%Y")
  
  full_date = paste("1-",train.data$earliest_cr_line, sep = "")
  train.data$earliest_cr_line_mon = floor((till - as.Date(full_date, "%d-%b-%Y")) / 30)
  
  full_date = paste("1-",test.data$earliest_cr_line, sep = "")
  test.data$earliest_cr_line_mon = floor((till - as.Date(full_date, "%d-%b-%Y")) / 30)
  
  Sys.setlocale("LC_TIME", lct)
  
  train.data$fico_score = (train.data$fico_range_high + train.data$fico_range_low) / 2
  test.data$fico_score = (test.data$fico_range_high + test.data$fico_range_low) / 2

  train.data$annual_inc = log(train.data$annual_inc + 0.01)
  test.data$annual_inc = log(test.data$annual_inc + 0.01)
  
  list(train = train.data, test = test.data)
}

preprocess_data <- function(train.data, test.data){
  r = convert_label(train.data, test.data)
  r = add_features(r$train, r$test)
  r = group_feature_levels(r$train, r$test)
  r = handle_missing(r$train, r$test)
  r = remove_features(r$train, r$test)
  
  # r$train = Winsorization(r$train, "dti")
  
  return (r)
}

###### Build Models ######
dumb_predict = function(train.data, test.data){
  return (rep(0.2, nrow(test.data)))  
}

logreg_predict = function(train.data, test.data){
  model.fit = glm(loan_status ~ ., data = train.data, family = "binomial")
  predict(model.fit, test.data, type="response")
}

svm_predict = function(train.data, test.data) {
  model.fit = ksvm(loan_status ~ ., data = train.data, prob.model=TRUE)
  predict(model.fit, test.data, type="probabilities")
}

lasso_predict = function(train.data, test.data) {
  X_train = train.data[, colnames(train.data) != 'loan_status']
  X_train = model.matrix(~., X_train)[, -1]
  Y_train = train.data$loan_status
  
  cv.out = cv.glmnet(X_train, Y_train, family="binomial", alpha = 1)
  
  X_test = model.matrix(~. -id, test.data)[, -1]
  predict(cv.out, s = cv.out$lambda.min, newx = X_test, type="response")
}

xgb_predict = function(train.data, test.data) {
  X_train = train.data[, colnames(train.data) != 'loan_status']
  X_train = model.matrix(~., X_train)[, -1]
  Y_train = train.data$loan_status
  

  xgb.model = xgboost(data = X_train, label=Y_train,
                      objective = "binary:logistic", eval_metric = "logloss",
                      eta = 0.09,
                      nrounds = 1200,
                      # colsample_bytree = 0.6,
                      # subsample = 0.75,
                      verbose = TRUE)
  
  # dtrain <- xgb.DMatrix(X_train, label = Y_train)
  # cv <- xgb.cv(data = X_train, label = Y_train,
  #              objective = "binary:logistic", eval_metric = "logloss",
  #               early_stopping_rounds = 10,
  #               # max_depth = 6,
  #               nfold = 5, nrounds = 2000,
  #               eta = 0.03,
  #               # colsample_bytree = 0.6,
  #               # subsample = 0.75,
  #               verbose = TRUE)
  
  X_test = model.matrix(~. -id, test.data)[, -1]
  predict(xgb.model, X_test, type="response")
}

catboost_predict = function(train.data, test.data) {
  if(!require(catboost)){
    print("Switch to XGBoost")
    return (xgb_predict(train.data, test.data))
  }
  X_train = train.data[, colnames(train.data) != 'loan_status']
  X_train = model.matrix(~., X_train)[, -1]
  Y_train = train.data$loan_status
  
  fit_params <- list(iterations = 1233, #task_type = 'GPU',
                     #use_best_model = TRUE,
                     loss_function = 'Logloss',
                     #eval_metric = 'Logloss',
                     #logging_level = "Silent",
                     learning_rate = 0.09)
  test_ids = sample(1:nrow(X_train), nrow(X_train) * 0.2)
  
  # learn_pool = catboost.load_pool(X_train[-test_ids,], label = Y_train[-test_ids])
  # test_pool =  catboost.load_pool(X_train[test_ids,], label = Y_train[test_ids])
  # 
  # cat.model <- catboost.train(learn_pool, test_pool, params = fit_params)
  
  learn_pool = catboost.load_pool(X_train, label = Y_train)
  cat.model <- catboost.train(learn_pool, params = fit_params)
  
  
  X_test = model.matrix(~. -id, test.data)[, -1]
  
  catboost.predict(cat.model, catboost.load_pool(X_test), prediction_type="Probability")
}

rf_predict = function(train.data, test.data) {
  train.data$loan_status = as.factor(train.data$loan_status)
  rf.model = randomForest(loan_status ~ ., data = train.data,
                          do.trace = TRUE, ntree = 500);
  
  predict(rf.model, test.data, type="prob")[,2]
}

train_predict = function(train.data, test.data, label.data, model.func, output.filename){
  pred = model.func(train.data, test.data)
  pred = round(pred, 2)
  
  output = cbind(test.data$id, pred)
  colnames(output) = c("id", "prob")
  
  write.csv(output, output.filename, row.names = FALSE, quote = FALSE)
  
  if(!is.null(label.data)){
    return (logLoss(label.data$y, pred))
  } else {
    return ("Unknown")
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
  Dumb = dumb_predict,
  # Dumb = dumb_predict,
  LogisticRegression = logreg_predict,
  # SVM = svm_predict,
  #Lasso = lasso_predict,
  Boosting = catboost_predict
  # Xgboost = xgb_predict,
  #RandomForest = rf_predict,
  # Dumb = dumb_predict,
)

r = preprocess_data(train.data, test.data)
if(exists("LOGLOSS") && exists("TEST_NUM")){
  colnames(LOGLOSS) = rep(names(model_functions), 3)[1:(dim(LOGLOSS)[2])]
}

for (f in 1:length(model_functions)) {
  loss = train_predict(r$train, r$test, label.data, model_functions[[f]], output_filenames[f])
  cat("Model:", names(model_functions)[f], "\t LogLoss=", loss, "\n")
  if(exists("LOGLOSS") && exists("TEST_NUM")){
    LOGLOSS[TEST_NUM, f] = loss
  }
}