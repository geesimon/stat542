###### Build Model ######
one_step_lasso = function(r, x, lam){
  length(r)
  length(x)
  xx = sum(x^2)
  xr = sum(r*x)
  b = (abs(xr) -lam/2)/xx
  b = sign(xr)*ifelse(b>0, b, 0)
  return(b)
}

mylasso_myfit = function(X, y, lam, beta = NULL)
{
  # X: n-by-p design matrix without the intercept
  # y: n-by-1 response vector
  # lam: lambda value
  # n.iter: number of iterations
  n.iter = 2000
  p = dim(X)[2]
  
  #Remove column with zero variation and later will need to set the corresponsing beta to 0
  full_b = rep(0, p)
  for(i in 1:p) {
    if(length(unique(X[,i])) <= 1) full_b[i] = 1
  }
  p = p - sum(full_b)
  X = X[,full_b ==0]
  
  # Center and scale X and y.
  X = scale(X)
  y = scale(y)
  x_center = attr(X, "scaled:center")
  x_scale = attr(X, "scaled:scale")
  y_center = attr(y, "scaled:center")
  y_scale = attr(y, "scaled:scale")    
  
  # Initial values for residual and coefficient vector b
  if(is.null(beta)) b = rep(0, p)
  else b = beta
  r = y
  
  rss_saved = 0
  rss_changed = 1000
  
  for(step in 1:n.iter){
    if (rss_changed < 1e-08) break;
    for(j in 1:p){
      r = r + X[,j] * b[j]
      b[j] = one_step_lasso(r, X[, j], lam)
      r = r - X[, j] * b[j]
    }
    rss = sum(r^2)
    rss_changed = abs(rss_saved - rss)
    rss_saved = rss
  }
  cat("rss_changed",rss_changed, "step", step, "\n")
  
  # Scale back b and add intercept b0
  b = b * y_scale / x_scale
  b0 = y_center - sum(b * x_center)
  
  #Fill beta of invariate predictor with zero
  j = 1
  for(i in 1:length(full_b)){
    if(full_b[i] == 1) {
      full_b[i] = 0
      next;
    }
    full_b[i] = b[j]
    j = j + 1
  }
  b = full_b
  
  return(c(b0, b))
}

mylasso_mypredict = function(X, beta) {
  beta[1] + as.matrix(X) %*% beta[-1]
}

cv.mylasso = function(train_data, cv_fold = 5) {
  all_lambda = exp(seq(-10, 10, 0.2))
  #all_lambda = c(1, 10, 100)
  X_train = train_data[, !colnames(train_data) %in% c('PID','Sale_Price')]
  X_train = model.matrix(~., X_train)[, -1]
  Y_train = train_data$Sale_Price
  
  cv_rmse = rep(0, length(all_lambda))
  cv_beta = matrix(0, length(all_lambda), dim(X_train)[2] + 1)
  
  all_ids = sample(1:nrow(X_train), nrow(X_train))
  all_test_ids = list()
  size = ceiling(nrow(X_train) / cv_fold)
  start = 1
  for (i in 1:cv_fold){
    end = start + size - 1
    if(end > nrow(X_train)) end = nrow(X_train)
    
    all_test_ids[[i]] = all_ids[start:end]
    
    start = end + 1
  }
  
  for (lambda_i in 1:length(all_lambda)) {
    for (i in 1:cv_fold){
      X_train_split = X_train[-all_test_ids[[i]],]
      Y_train_split = Y_train[-all_test_ids[[i]]]
      X_test_split = X_train[all_test_ids[[i]],]
      Y_test_split = Y_train[all_test_ids[[i]]]
      
      # cv_beta[lambda_i,] = mylasso_myfit(X_train, Y_train, all_lambda[lambda_i], cv_beta[lambda_i,])
      # yhat_test = mylasso_mypredict(X_test_split, cv_beta[lambda_i,])
      beta = mylasso_myfit(X_train_split, Y_train_split, all_lambda[lambda_i])
      yhat_test = mylasso_mypredict(X_test_split, beta)
      cv_rmse[lambda_i] = cv_rmse[lambda_i] + get_RMSE(yhat_test, Y_test_split)
      #cat("lambda", all_lambda[lambda_i], "\trmse", cv_rmse[lambda_i], "\n")
    }
    cv_rmse[lambda_i] = cv_rmse[lambda_i] / cv_fold
    cat("lambda:", all_lambda[lambda_i], "\trmse:", cv_rmse[lambda_i], "\n")
  }
  
  min.lambda = all_lambda[which.min(cv_rmse)]
  print(min.lambda)
  mylasso_myfit(X_train, Y_train, min.lambda)
}

mylasso_predict = function(train_data, test_data) {
  X_train = train_data[, colnames(train_data) != 'Sale_Price']
  X_train = model.matrix(~., X_train)[, -1]
  
  Y_train = train_data$Sale_Price
  
  beta = mylasso_myfit(X_train, Y_train, 10)
  
  X_test = test_data[, colnames(test_data) != 'Sale_Price']
  X_test = model.matrix(~. -PID, X_test)[, -1]
  
  if(sum(names(X_train) != names(X_test)) > 0) stop("Unmatched train and test data")
  mylasso_mypredict(X_test, beta)
}
#######################################
###### Train, Predict and Output ######
#######################################
set.seed(6682)

TRAIN_FILE_NAME = 'train.csv'
TEST_FILE_NAME = 'test.csv'

train_data = read.csv(TRAIN_FILE_NAME)
test_data = read.csv(TEST_FILE_NAME)

train_mylasso = function(train_data, test_data, output_filename) {
  r = preprocess_data(train_data, test_data)
  
  X_train = r$train_data[, !colnames(r$train_data) %in% c('PID','Sale_Price')]
  X_train = model.matrix(~., X_train)[,-1]
  Y_train = r$train_data$Sale_Price
  
  X_test = r$test_data[, !colnames(r$test_data) %in% c('PID','Sale_Price')]
  X_test = model.matrix(~., X_test)[,-1]
  
  #best_beta = cv.mylasso(r$train_data)
  best_beta = mylasso_myfit(X_train, Y_train, 29.9641)
  
  yhat_test = exp(mylasso_mypredict(X_test, best_beta))
  
  if(!is.null(r$true_test_value)){
    rmse = log_RMSE(yhat_test, exp(r$true_test_value))
  } else {
    rmse = NULL
  }
  
  output = cbind(test_data$PID, yhat_test)
  colnames(output) = c("PID", "Sale_Price")
  
  write.csv(output, output_filename, row.names = FALSE)
  rmse
}

start_time = proc.time()
rmse = train_mylasso(train_data, test_data, "mysubmission3.txt")

rmse