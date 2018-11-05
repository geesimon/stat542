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
update_forecast.old <- function(test_month, dept_preds, dept, num_model) {
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

handle_na <- function(train.ts.data, test.data){
  sales.na.idx = seq(1, length(train.ts.data[,1]))[is.na(train.ts.data[,1])]
  if(length(sales.na.idx) > 0){
    for (i in sales.na.idx) {
      #Use the value of previous season
      pre_i = i - 52
      if(pre_i > 0 & !is.na(train.ts.data[pre_i, 1])){
        cat("Fill with the value of previous session")
        train.ts.data[i, 1] <- train.ts.data[pre_i, 1]
      } else {
        #Use average of all value of previous season
      }
      train.ts.data[i, 1] <- 0
      
    }
  }
  
  # if(length(train_ts) - sum(is.na(train_ts)) > 2){
  #   #train_ts1 <-  na.interp(train_ts)
  # }
  # 
  # train_ts[is.na(train_ts)] <- 0

  return (list(train_data = train.ts.data, test_data = test.data))
}

naive_forecast <- function(train_data, test_data){
  if(nrow(test_data) < 8){
    cat(nrow(train_data), nrow(test_data), "\n")
  }
  num_forecasts <- nrow(test_data)
  
  ts_data <- ts(train_data$Weekly_Sales, frequency=52)
  ts_data <- handle_na(ts_data)
  naive(ts_data, num_forecasts)$mean
}

snaive_forecast <- function(train_data, test_data){
  num_forecasts <- nrow(test_data)
  
  ts_data <- ts(train_data$Weekly_Sales, frequency=52)
  ts_data <- handle_na(ts_data)
  snaive(ts_data, num_forecasts)$mean
}

regression_forecast <- function(train.data, test.data){
  num_forecasts <- nrow(test.data)
  
  train.ts.data <- ts(train.data %>% select(Weekly_Sales, IsHoliday), frequency = 52,
                start = c(year(train$Date[1]), week(train$Date[1])))
  data = handle_na(train.ts.data, test.data)
  
  # if(train_data$Store[1] == 36 & train_data$Dept == 5){
  #   cat("Store:", train_data$Store[1], "Dept:", train_data$Dept[1])
  # }
  
  model <- tslm(Weekly_Sales ~ trend + season, data = data$train_data)
  forecast(model, h=num_forecasts)$mean
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



##### Prediction Loop #####
#forecast.functions = c(naive_forecast, regression_forecast)
#forecast.functions = c(naive_forecast, tbats_forecast)
#forecast.functions = c(snaive_forecast, nnetar_forecast, tbats_forecast)
forecast.functions = c(regression_forecast)

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
      filter(Dept == dept)
    
    # Create a dataframe to hold the forecasts on
    # the dates in the training window
    train_dept <- train_frame %>%
      left_join(train_dept, by = c('Date', 'Store'))
    
    # Filter for the particular department in the test data
    test_month.idx <- test_month$Dept == dept
   
    # Create a similar dataframe to hold the forecasts on
    # the dates in the testing window
    test_dept <- test_frame %>%
      left_join(test_month[test_month.idx, ], by = c('Date', 'Store')) %>%
      select(-Weekly_Pred1, -Weekly_Pred2, -Weekly_Pred3) %>%
      mutate(Predict_Sales = 0)

    for (func.i in 1:length(forecast.functions)){
      all.predict = data.frame()
      
      for (store in all_stores){
        train.data <- train_dept %>% filter(Store == store)
        test.data <-  test_dept %>% filter(Store == store)
          
        test.data$Predict_Sales <- as.numeric(forecast.functions[[func.i]](train.data, test.data))
        all.predict = rbind(all.predict, test.data)
      }
      
      test_month[test_month.idx, 4 + func.i] <- (test_month[test_month.idx,] %>%
                                left_join(all.predict, by = c('Date', 'Store', 'Dept')))$Predict_Sales
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