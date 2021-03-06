
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
#takes in xgbooster and data matrix (must be type matrix)
predictor <- function(mod,data_v){
  data_v <- as.matrix(data_v)
  data_v <- t(data_v)
  pred <- predict(mod,data_v)
  return(pred)
}


#prediction vector function
predict_vector <- function(mod,data_m) {
  data_m <- as.matrix(data_m)
  pred <- function(x) predictor(mod,x)
  pred_vector <- apply(data_m,1,pred)
  return(pred_vector)
}


