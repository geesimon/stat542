################ Load Environment ##################
# clean workspace
#rm(list = ls())

# load necessary packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "lubridate",
  "forecast",
  "tidyverse"
)

# converts a Date x num_store forecast to a dataframe
# with Date, Store, value = Weekly_Price columns
flatten_forecast <- function(f_model) {
  f_model %>%
    # select(-IsHoliday) %>%
    gather(Store, value, -Date, convert = TRUE)
}

# Adds forecasts to the testing dataframe
update_forecast <- function(test_month, dept_preds, dept, num_model) {
  dept_preds = flatten_forecast(dept_preds)
  
  # pred.d <- test_month %>%
  #   filter(Dept == dept) %>%
  #   select('Store', 'Date') %>%
  #   left_join(dept_preds, by = c('Store', 'Date'))
  
  pred.d.idx <- test_month$Dept == dept
  pred.d <- test_month[pred.d.idx, c('Store', 'Date')] %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  # cat(length(pred.d$value), sum(pred.d.idx), "\n")
  
  if (num_model == 1) {
    test_month$Weekly_Pred1[pred.d.idx] <- pred.d$value
  } else if(num_model == 2) {
    test_month$Weekly_Pred2[pred.d.idx] <- pred.d$value
  } else {
    test_month$Weekly_Pred3[pred.d.idx] <- pred.d$value
  }
  
  test_month
}

# update forecasts in the global test dataframe
update_test <- function(test_month) {
  test <<- test %>%
    dplyr::left_join(test_month,
                     by = c('Date', 'Store', 'Dept', 'IsHoliday')) %>%
    mutate(Weekly_Pred1 = coalesce(Weekly_Pred1.y, Weekly_Pred1.x)) %>%
    mutate(Weekly_Pred2 = coalesce(Weekly_Pred2.y, Weekly_Pred2.x)) %>%
    mutate(Weekly_Pred3 = coalesce(Weekly_Pred3.y, Weekly_Pred3.x)) %>%
    select(-Weekly_Pred1.x, -Weekly_Pred1.y,
           -Weekly_Pred2.x, -Weekly_Pred2.y,
           -Weekly_Pred3.x, -Weekly_Pred3.y)
}


##### Model Building Functions #####

# Forecasts out the last observation in the training data
naive_model <- function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  
  # naive forecast per store
  for(j in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, j], frequency=52)
    test_ts[, j] <- naive(store_ts, num_forecasts)$mean
  }
  test_ts
}

snaive_model <- function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  
  # naive forecast per store
  for(j in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, j], frequency=52)
    test_ts[, j] <- snaive(store_ts, num_forecasts)$mean
  }
  test_ts
}

stlf_model <- function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts <- na.interp(train_ts)
  
  # naive forecast per store
  for(j in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, j], frequency=52)
    test_ts[, j] <- stlf(store_ts, num_forecasts)$mean
  }
  test_ts
}

handle_na <- function(train_ts){
  # if(length(train_ts) - sum(is.na(train_ts)) > 2){
  #   train_ts1 <-  na.interp(train_ts)
  # } 

  train_ts[is.na(train_ts)] <- 0

  return (train_ts)
}

naive_forecast <- function(train_dept, test_dept){
  # num_forecasts <- nrow(test_ts)
  # train_ts <- handle_na(train_ts)
  # 
  # return (naive(train_ts, num_forecasts)$mean)
  
  num_forecasts <- nrow(test_dept)
  
  for(j in 2:ncol(train_dept)){
    store_ts <- ts(train_dept[, j], frequency=52)
    store_ts <- handle_na(store_ts)
    test_dept[, j] <- naive(store_ts, num_forecasts)$mean
  }
  test_dept
}

snaive_forecast <- function(train_dept, test_dept){
  # num_forecasts <- nrow(test_ts)
  # train_ts <- handle_na(train_ts)
  # 
  # return (snaive(train_ts, num_forecasts)$mean)
  
  num_forecasts <- nrow(test_dept)
  
  for(j in 2:ncol(train_dept)){
    store_ts <- ts(train_dept[, j], frequency=52)
    store_ts <- handle_na(store_ts)
    test_dept[, j] <- snaive(store_ts, num_forecasts)$mean
  }
  test_dept
}

# nnetar_forecast <- function(train_ts, test_ts){
#   num_forecasts <- nrow(test_ts)
#   train_ts <- handle_na(train_ts)
#   
#   return (forecast(nnetar(train_ts), num_forecasts)$mean)
# }
# 
# tbats_forecast <- function(train_ts, test_ts){
#   num_forecasts <- nrow(test_ts)
#   train_ts <- handle_na(train_ts)
#   
#   return (forecast(tbats(train_ts, biasadj=TRUE), num_forecasts)$mean)
# }

regression_forecast <- function(train_dept, test_dept){
  num_forecasts <- nrow(test_dept)
  
  for(j in 2:ncol(train_dept)){
    train_ts = ts(train_dept[, j], frequency = 52)
    model <- tslm(train_ts ~ trend + season)
    
    # test_dept[, j] <- forecast(model, newdata = test_dept)$mean
    test_dept[, j] <- forecast(model, h = num_forecasts)$mean
  }
  
  test_dept
}

# Dimension reduction using SVD.
preprocess.svd = function(train, n.comp){
  train[is.na(train)] = 0
  z = svd(train[, 2:ncol(train)], nu=n.comp, nv=n.comp)
  s = diag(z$d[1:n.comp])
  train[, 2:ncol(train)] = z$u %*% s %*% t(z$v)
  train
}


##### Prediction Loop #####
#forecast.functions = c(naive_forecast, snaive_forecast)
#forecast.functions = c(naive_forecast, tbats_forecast)
#forecast.functions = c(snaive_forecast, nnetar_forecast, tbats_forecast)
forecast.functions = c(regression_forecast)

n.comp = 12

mypredict <- function() {
  ###### Create train and test time-series #######
  if (t > 1) {
    # append the previous periods test data to the current training data
    train <<- rbind(train, new_test)
  }
  
  # filter test data.frame for the month that needs predictions
  # backtesting starts during March 2011
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_month <- test %>%
    filter(Date >= start_date & Date < end_date)
  
  # Dates are not the same across months!
  test_dates <- unique(test_month$Date)
  num_test_dates <- length(test_dates)
  
  # Not all stores may need predictions either
  all_stores <- unique(test_month$Store)
  num_stores <- length(all_stores)
  
  # Most importantly not all departments need predictions
  test_depts <- unique(test_month$Dept)
  
  # Dateframe with (num_test_dates x num_stores) rows
  test_frame <- data.frame(
    Date=rep(test_dates, num_stores),
    Store=rep(all_stores, each=num_test_dates)
  )
  
  # Create the same dataframe for the training data
  # (num_train_dates x num_stores)
  train_dates <- unique(train$Date)
  num_train_dates <- length(train_dates)
  train_frame <- data.frame(
    Date=rep(train_dates, num_stores),
    Store=rep(all_stores, each=num_train_dates)
  )
  
  #### Perform a individual forecasts for each department
  pb <- txtProgressBar(min = 0, max = length(test_depts), style = 3)
  for (dept_i in 1:length(test_depts)) {
    dept = test_depts[dept_i]
    # filter for the particular department in the training data
    train_dept <- train %>%
      filter(Dept == dept) %>%
      #select(Store, Date, Weekly_Sales, IsHoliday)
      select(Store, Date, Weekly_Sales)
    
    # Reformat so that each column is a weekly time-series for that
    # store's department.
    # The dataframe has a shape (num_train_dates, num_stores)
    train_dept <- train_frame %>%
      left_join(train_dept, by = c('Date', 'Store')) %>%
      spread(Store, Weekly_Sales)
    
    # We create a similar dataframe to hold the forecasts on
    # the dates in the testing window
    test_dept <- test_frame %>%
      mutate(Weekly_Sales = 0) %>%
      spread(Store, Weekly_Sales)
    
    ###### Model Fitting / Forecasting ######
    # for (func.i in 1:length(forecast.functions)){
    #   for(store.index in 2:ncol(train_dept_ts)) {
    #     store_ts <- ts(train_dept_ts[, store.index], frequency=52)  
    #   
    #     test_dept_ts[, store.index] <-  forecast.functions[[func.i]](store_ts, test_dept_ts[, 1:2])
    #     
    #   }
    #   test_month <- update_forecast(test_month, test_dept_ts, dept, func.i)
    # }
    
    # apply SVD for tr.d
    tr.d = preprocess.svd(train_dept, n.comp)

    for (func.i in 1:length(forecast.functions)){
      pred <- forecast.functions[[func.i]](tr.d, test_dept)
      test_month <- update_forecast(test_month, pred, dept, func.i)
    }
    
    # # naive forecast
    # f_naive <- naive_model(train_dept_ts, test_dept_ts)
    # test_month <- update_forecast(test_month, f_naive, dept, 1)
    # 
    # # snaive forecast
    # f_snaive <- snaive_model(train_dept_ts, test_dept_ts)
    # test_month <- update_forecast(test_month, f_snaive, dept, 2)
    # 
    # # stlf forecast
    # f_stlf <- stlf_model(train_dept_ts, test_dept_ts)
    # test_month <- update_forecast(test_month, f_stlf, dept, 3)
    
    setTxtProgressBar(pb, dept_i)
  }
  
  # update global test dataframe
  update_test(test_month)
}