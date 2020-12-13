#Want to obtain: 
#     rmse_full_data_train_v
#     rmse_subset_data_train_v

#### The following variables are for increased data efficiency
NEW_MOD <- TRUE #Flag indicating if new control model has been set
control_rmse <- 0


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
                        label = 'critical_temp'){
  #obtain testing data partition for subset of interest
  data_test <- intersect(data,subset) 
  #if retrain=T, retrain model only on training data also present in subset 
  mod_test <- model_retrain(data, subset, mod_control, retrain, label)
  
  #obtain RMSE values for control and test models and data
  if (NEW_MOD){ #only compute control RMSE if control model or control data
                  #have changed or been set for the first time
    NEW_MOD <<- FALSE
    control_rmse <<- rmse_funct(mod_control,data,label)
  }
  #compute error statistics for test model and data
  test_err <- err_stats(mod_test,data_test,label)
  return(c(test_err, 'control' = control_rmse))
}


#function to calculate rmse vectors for given data subsets and training data
rmse_subset_vector <- function(data,
                               subset_vect, 
                               model, 
                               label = 'critical_temp',
                               retrain=FALSE){
  
  rmse_subset_partial <- function(subset) rmse_subset(data, 
                                                      subset, 
                                                      model, 
                                                      retrain, 
                                                      label=label)
  #assemble template return value vector for err_subset
  template_vector <- c(
    'test' = 0,
    'ave_err' = 0,
    'std_err' = 0,
    'under_cnt' = 0,
    'over_cnt' = 0,
    'correct_cnt' = 0,
    'ave_under' = 0,
    'std_under' = 0,
    'ave_over' = 0,
    'std_over' = 0,
    'control' = 0
  )
  
  rmse_vec <- vapply(subset_vect, rmse_subset_partial, c(template_vector))
  rmse_vec <- cbind( t(rmse_vec), 'subset' = names(subset_vect) ) #TODO: might cause issues
  return( rmse_vec)
}

 rmse_subset_1trial <- function(subset, trial_num, ntrials, retrain){
   NEW_MOD <<- TRUE
   print('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
   print(sprintf("Trial ", trial_num,"/",ntrials))
   print('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
   partitions <- partition(train,2/3)
   train_df <- partitions[['train']]
   test_df <- partitions[['test']]
   mod <- trainer(train,'critical_temp')
   return( rmse_subset_vector(test_df, subset, mod, retrain = retrain)  )
 }

 #Function calculates 50 iterations of rmse values for subsets
 rmse_subset_ntrials <- function(subsets, ntrials = 50, retrain = FALSE){
   trials <- 1:ntrials
   rmse_partial <- function(x) rmse_subset_1trial(subsets,x,ntrials,retrain)
   return(lapply(trials,rmse_partial))
 }

#same functionaity as rmse_subset_ntrials, but return value is a single 
 #dataframe instead of a list of dataframes
rmse_subset_ntrials_as_df <- function(subsets, ntrials=50, retrain = FALSE){
  rmse_df <- rmse_subset_ntrials(subsets, ntrials, retrain)
  return( condense(rmse_df))
}


################################## Raw Error Collection ####################################
 
# #collects raw error vector for given subset 
 raw_err_collector <- function(model,subsets){
  err_vect_partial <- function(subset) err_vect(model,subset)
  return(lapply(subsets,err_vect_partial))
 }


#deciles_err <- raw_err_collector(bst, deciles)













