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
fill_missing_holiday <- function(dept_data){
  na.idx = seq(1, nrow(dept_data))[is.na(dept_data$IsHoliday)]
  for (i in na.idx){
    same.date.holiday = dept_data$IsHoliday[dept_data$Date == dept_data$Date[i]]
    non.na.idx = !is.na(same.date.holiday)
    if (sum(non.na.idx) > 0){
      value = same.date.holiday[non.na.idx][1]
    } else {
      cat("Cannot find IsHoliday default value for ", dept_data$Date[i], "\n")
      value = FALSE
    }
    dept_data$IsHoliday[i] = value
  }
  
  dept_data$IsHoliday
}

handle_na <- function(train.data, test.data){
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
  num_forecasts <- nrow(test_data)
  
  ts_data <- ts(train_data$Weekly_Sales, frequency=52)
  naive(ts_data, num_forecasts)$mean
}

snaive_forecast <- function(train_data, test_data){
  num_forecasts <- nrow(test_data)
  
  ts_data <- ts(train_data$Weekly_Sales, frequency=52)
  snaive(ts_data, num_forecasts)$mean
}

regression_forecast <- function(train.data, test.data){
  num_forecasts <- nrow(test.data)
  
  train.ts.data <- ts(train.data %>% select(Weekly_Sales, IsHoliday), frequency = 52,
                start = c(year(train$Date[1]), week(train$Date[1])))
  
  model <- tslm(Weekly_Sales ~ trend + season, data = train.ts.data)
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
    dept <- test_depts[dept_i]
    # filter for the particular department in the training data
    train_dept <- train %>%
      filter(Dept == dept)
    
    # Create a dataframe to hold the forecasts on
    # the dates in the training window
    train_dept <- train_frame %>%
      left_join(train_dept, by = c('Date', 'Store'))
    
    # Handle missing value of Weekly_Sales in the training data
    # Set possible NA to the dept
    train_dept$Dept <- dept
    # Handle missing Weekly_Sales
    na.idx <- seq(1, nrow(train_dept))[is.na(train_dept$Weekly_Sales)]
    for (i in na.idx){
      # Try to find the value from last season/year
      pre_i <- i - 52
      if (pre_i > 0 && train_dept$Store[pre_i] == train_dept$Store[i] &&
          !is.na(train_dept$Weekly_Sales[pre_i])){
        train_dept$Weekly_Sales[i] = train_dept$Weekly_Sales[pre_i]
        #cat("Found value from last year")
      } else {
        # Use the average value of stores in the same department and date
        sales <- train_dept$Weekly_Sales[train_dept$Date == train_dept$Date[i]]
        sales <- sales[!is.na(sales)]
        if (length(sales) > 0){
          #cat("Average of all store values are used")
          train_dept$Weekly_Sales[i] <- mean(sales)
        } else {
          cat("Cannot calculate value for ", train_dept$Date[i], train_dept$Store[i], 
              train_dept$Dept[i], "\n")
          
          train_dept$Weekly_Sales[i] <- 0
        }
      }
    }
    # Handle missing IsHoliday
    train_dept$IsHoliday <- fill_missing_holiday(train_dept)
    
    # Filter for the particular department in the test data
    test_month.idx <- test_month$Dept == dept
   
    # Create a similar dataframe to hold the forecasts on
    # the dates in the testing window
    test_dept <- test_frame %>%
      left_join(test_month[test_month.idx, ], by = c('Date', 'Store')) %>%
      select(-Weekly_Pred1, -Weekly_Pred2, -Weekly_Pred3) %>%
      mutate(Predict_Sales = 0)
    
    # Handle missing value in test data
    # Set possible NA to the dept
    test_dept$Dept <- dept
    # Handle missing IsHoliday
    test_dept$IsHoliday <- fill_missing_holiday(test_dept)
    
    if (sum(is.na(train_dept)) > 0 | sum(is.na(test_dept))) {
      stop("NA found in the training or test data")
    }
    
    # Start training/prediction 
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

    setTxtProgressBar(pb, dept_i)
  }
  
  # update global test dataframe
  update_test(test_month)
}