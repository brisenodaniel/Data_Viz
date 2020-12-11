#Want to obtain: 
#     rmse_full_data_train_v
#     rmse_subset_data_train_v

#### The following variables are for increased data efficiency
NEW_MOD <- TRUE
SAVED_RMSE <- 0


#Helper function, turns list of df with same columns into
# one large df

condense <- function(df_list){
  df <- data.frame()
  for (i in 1:length(df_list)){
    df <- rbind(df, df_list[[i]])
  }
  return (df)
}

#helper function, decides if model needs to be retrained on reduced dataset
# and if so safely retrains it. Otherwise returns control_model
model_retrain <- function(data, subset, control_model, retrain, label){
  #if model should be retrained
  if(retrain){
    data_test <- intersect(data,subset) #partition of testing data also in subset
    data_train <- intersect(train,subset) #partition of superconductor dataset also in subset
    data_train <- setdiff(data_train,data_test) #remove partition in data_train also in data_test
                                                #ensures mutually exclusive testing & training data
    if(nrow(data_train)==0){#check for empty training data
      warnings("NO DATA IN SUBSET AVAILABLE TO TRAIN MODEL")
      warnings("WILL NOT RETRAIN MODEL THIS RUN")
      return(control_model)
    }
    else{#if training data exists 
      return (trainer(data_train,label))
    }
  }
  else{
    return(control_model) 
  }
}


#function to calculate rmse for given subset and training data
rmse_subset <- function(data, 
                        subset,
                        mod_control, 
                        retrain,
                        size_adj,
                        label = 'critical_temp'){
  
  
  data_test <- intersect(data,subset)
  
  #Retrain model if necessary
  mod_test <- NULL
  if (retrain){
    data_train <- intersect(train,subset)
    data_train <- setdiff(data_train, data_test)
    if(nrow(data_train)==0){
      warnings("NO DATA IN SUBSET AVAILABLE TO TRAIN MODEL")
      warnings("WILL NOT RETRAIN MODEL THIS RUN")
    }
    else{
      mod_test <- trainer(data_train,label)
      rm(data_train)
    }
  }
  else{
    mod_test <- mod_control
  }
  
  #extract control rmse  
  #obtain data partition for control rmse
  data_control = NULL
  if (size_adj){
  control_idx <- sample(1:nrow(data),size = nrow(data_test))
  data_control <- data[control_idx,]
  }
  else data_control <- data
  #extract test and control rmse
  test_rmse <- err_funct(mod_test,data_test,label)
  control_rmse <- err_funct(mod_control, data_control,label) 
  return(c('test'=test_rmse, 'control' = control_rmse))
}


# Sanity check


#v <- c(c('test' = 4, 'control' = 6), c('test'= 1,'control' = 2),c('test'=9,'control'=10))
#data.frame(t(v))
#as.matrix(t(v))

#rmse_subset(train,cu_train,bst)

#function to calculate rmse vectors for given data subsets and training data
rmse_subset_vector <- function(data,
                               subset_vect, 
                               model, 
                               label = 'critical_temp',
                               retrain=FALSE,
                               size_adj = TRUE){
  
  rmse_subset_partial <- function(subset) rmse_subset(data, 
                                                      subset, 
                                                      model, 
                                                      retrain, 
                                                      label=label,
                                                      size_adj = size_adj)
  rmse_vec <- vapply(subset_vect, rmse_subset_partial,c('test' = 0, 'control' = 0))
  rmse_vec <- cbind( t(rmse_vec), 'subset' = names(subset_vect) )
  return( rmse_vec)
#  return( data.frame( rmse_vec ) )
}
# sanity_check

# #subsets <- list('cu' = cu_train, 'fe' = fe_train, 'mg' = mg_train)
# names(subsets)
# head(subsets)
# tf <- function(x) deparse(substitute(x))
# lapply(subsets,tf)
# names(subsets)
# 
# 
# 
# system.time(
#   {
#    partitions <- partition(train,2/3)
#    train_df <- partitions[['train']]
#    test_df <- partitions[['test']]
#    bst <- trainer(train_df,'critical_temp')
# df <- rmse_subset_vector(test_df,
#                          subsets,
#                          bst, 
#                          retrain = TRUE,
#                          size_adj = FALSE)
# #}
# # )
# 
# 
# df <- data.frame(df)
# rownames(df)
# df <- rbind(df,df)
# class(df)

 rmse_subset_1trial <- function(subset, trial_num, ntrials, retrain, size_adj){
   print('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
   print(sprintf("Trial ", trial_num,"/",ntrials))
   print('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
   partitions <- partition(train,2/3)
   train_df <- partitions[['train']]
   test_df <- partitions[['test']]
   mod <- trainer(train,'critical_temp')
   return( rmse_subset_vector(test_df, subset, mod, retrain=retrain, size_adj = size_adj)  )
 }

 #Function calculates 50 iterations of rmse values for subsets
 rmse_subset_ntrials <- function(subsets, ntrials = 50, retrain = FALSE, size_adj = FALSE){
   trials <- 1:ntrials
   rmse_partial <- function(x) rmse_subset_1trial(subsets,x,ntrials,retrain, size_adj)
   return(lapply(trials,rmse_partial))
 }
#  
 
rmse_subset_ntrials_as_df <- function(subsets, ntrials=50, retrain = FALSE, size_adj = FALSE) {
  rmse_df <- rmse_subset_ntrials(subsets, ntrials, retrain, size_adj)
  return( condense(rmse_df))
}

system.time({
 df <- rmse_subset_ntrials(elemental_subsets,2)
 # length(df)
 #  rbind(df[[1]], df[[2]])
 #  empty_df <- data.frame()
 #  rbind(empty_df,df[[1]])
})
rmse_subset_ntrials_as_df(elemental_subsets,2)

# lst <- list(1,2,3)
# names(lst) <- c(1,2,3)
