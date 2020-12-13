
source('Data_Splitter.R')
source('Model_Trainer.R')
source('RMSE_Calculator.R')


#full set of err vects
control_df <- rmse_subset_ntrials_as_df(list('trn'=train),2)

system.time({
###### Get RMSE Data by Element Subsets

elemental_subsets <- list(fe_train, hg_train, cu_train)
names(elemental_subsets) = c('Fe','Hg','Cu')

elemental_rmse_no_retrain <- rmse_subset_ntrials_as_df(elemental_subsets,2)

###### Get RMSE Data by Element Subsets with retraining 

elemental_rmse_retrained <- rmse_subset_ntrials_as_df(elemental_subsets,2,retrain = TRUE)

###### Get RMSE Data by Quartile
filt <- function(x) subset(x,select = -c(decile, quartile))
quartiles <- lapply(train_quartiles,filt)
names(quartiles) <- c(1:4)
quartile_rmse <- rmse_subset_ntrials_as_df(quartiles,2)




###### Get RMSE Data by Decile
deciles <- lapply(train_deciles,filt)
names(deciles) <- c(1:10)
decile_rmse <- rmse_subset_ntrials_as_df(deciles,2)
})

elemental_rmse_no_retrain
elemental_rmse_retrained
quartile_rmse
decile_rmse


########################## Get Error Vectors ##################


train_fe <- intersect(part$test,fe_train)
part <- partition(train,2/3)
nrow(part$test)

661+142
nrow(train_fe)
