library(xgboost)
library(Matrix)
require(dplyr)
require(Metrics)
#################### Training Function ####################
trainer <- function(training_data, label, nthreads=4){
  label_values <- as.matrix(training_data %>% select(label))
  training_data <- training_data %>% select(-label)
  training_data <- as.matrix(training_data)
  bst <- xgboost(data=training_data,
                 label = label_values,
                 max_depth = 16,
                 nthread= nthreads, 
                 objective = 'reg:linear', 
                 eval_metric='rmse',
                 nrounds=100, 
                 eta= 0.02,
                 min_child_depth = 1, 
                 colsample_bytree = 0.5, 
                 subsample = 0.5 )
  return(bst)
}


#predictor function
predictor <- function(mod,data){
  data <- as.matrix(data)
  data <- t(data)
  pred <- predict(mod,data)
  return(pred)
}


#prediction vector function
predict_vector <- function(mod,data) {
  data <- as.matrix(data)
  pred <- function(x) predictor(mod,x)
  pred_vector <- apply(data,1,pred)
  return(pred_vector)
}


#rmse calculating function
rmse_funct <- function(mod, full_m,label = 'critical_temp'){
  test_m <- full_m %>% select(-label)
  actual_ct <- full_m %>% select(label)
  pred <- predict_vector(mod,test_m)
  return( rmse(as.numeric(t(actual_ct)), pred) )
}

#full error stats calculating function
err_stats <- function(mod, full_m, label = 'critical_temp'){
  #obtain prediction on test data
  test_m <- full_m %>% select(-label)
  actual_ct <- full_m %>% select(label)
  pred <- predict_vector(mod,test_m)
  
  #obtain rmse
  rmse_full <- rmse(as.numeric(t(actual_ct)), pred)
  
  #obtain mean error and std of error
  raw_diff <- t(pred - actual_ct)
  ave_err <- mean(raw_diff)
  std_err <- std(raw_diff)
  
  #obtain count of over and under prediction
  under_pred <- raw_diff[raw_diff<0]
  over_pred <- raw_diff[raw_diff>0]
  exact_pred <- raw_diff[raw_diff == 0]
  under_cnt <- length(under_pred)
  over_cnt <- length(over_pred)
  
  #obtain number of exact predictions
  correct_cnt <- length(exact_pred)
 
  
  #obtain mean and std over_estimation
  ave_over <- mean(over_pred)
  std_over <- std(over_pred)
  
  #obtain mean and std under_estimation
  ave_under <- mean(under_pred)
  std_under <- std(under_pred)
  
  #assemble return vector
  ret_vect <- c(
    'test' = rmse_full,
    'ave_err' = ave_err,
    'std_err' = std_err,
    'under_cnt' = under_cnt,
    'over_cnt' = over_cnt,
    'correct_cnt' = correct_cnt,
    'ave_under' = ave_under,
    'std_under' = std_under,
    'ave_over' = ave_over,
    'std_over' = std_over
  )
}

#returns list of raw errors (predict - actual) and predictions
err_vect <- function(mod, full_df, label = 'critical_temp'){
  test_df <- full_df %>% select(-label)
  actual_ct <- full_df %>%select(label)
  pred <- predict_vector(mod,test_df)
  raw_diff <- pred - actual_ct
  return(t(raw_diff))
}
p <- predict_vector(bst,train[1:50,-82])
p
tr <- train[1:50,] %>% select('critical_temp')
p - tr

v <- err_vect(bst, train)

length(v)

v[v>0]
length(v[v<0])
std(v)
class(t(v))

sml <- rmse_funct(bst,train[1:50,])
v_f <- err_stats(bst,train[1:50,])
c(v_f, 'control' = sml)
#returns raw average of (prediction - actual) for target label
err_mean <- function(mod, full_df, label = 'critical_temp') {
  mean(err_vect(mod,full_df, label))
}

bst <- trainer(train,'critical_temp')
err_mean(bst, train[1:50,])
err_funct(bst,train[1:50,])

# err <- err_vect(bst, train[1:50,],'critical_temp')
# class(err)
# bst <- trainer(train,'critical_temp')
# test_df <- train %>% select(-critical_temp)
# actual_ct <- train %>% select(critical_temp)
# actual_ct <- actual_ct[1:50,]
# pred <- predict_vector(bst, test_df[1:50,])
# raw_diff <- pred - as.numeric(t(actual_ct))
# mean(raw_diff)
# df_diff <- data.frame('diff'=raw_diff)
# 
# pos_diff <- df_diff %>% filter(diff>0)
# mean(as.numeric(t(pos_diff)))
# median(as.numeric(t(pos_diff)))
# std(as.numeric(t(pos_diff)))
# nrow(pos_diff)
# 
# neg_diff <- df_diff %>% filter(diff<0)
# mean(as.numeric(t(neg_diff)))
# median(as.numeric(t(neg_diff)))
# std(as.numeric(t(neg_diff)))
# nrow(neg_diff)
# 
# 
# 
# 
# 
# 
# 
# 

