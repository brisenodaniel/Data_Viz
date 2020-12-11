#library(MVN)
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

### sanity check ###
#
 



#prediction vector function
predict_vector <- function(mod,data) {
  data <- as.matrix(data)
  pred <- function(x) predictor(mod,x)
  pred_vector <- apply(data,1,pred)
  return(pred_vector)
}

## sanity check
# test_df <- train %>% select(-critical_temp)
# test_df <- test_df[1:10,]
# predict_vector(bst,test_df)
# 
# train_m = as.matrix(train[,-82])
# idx = 10
# v = train_m[idx,]
# 
# predictor(bst,v)
# 
#head(train)


#rmse calculating function
err_funct <- function(mod, full_m,label){
  test_m <- full_m %>% select(-label)
  actual_ct <- full_m %>% select(label)
  pred <- predict_vector(mod,test_m)
  return( rmse(as.numeric(t(actual_ct)), pred) )
}

#sanity check
# # #err_funct(bst,train,'critical_temp')
# # head(train)
# bst <- trainer(train,label='critical_temp')
# 
# xgb.importance(model=bst)
# 
# class(bst)
# 
# 



 
# v =t(train_tst[1,])
# cor(train)
# tr <- read.csv("train.csv")
# class(tr)
# correlation = cor(tr)
# correlation[,82]
# rownames(correlation)
# colnames(correlation)
