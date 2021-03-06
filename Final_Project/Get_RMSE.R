source('Dependencies.R')
source('Data_Splitter.R')
source('Model.R')
source('RMSE_Calculator.R')

system.time({
ntrials <- 50
####### full set of err vects
control_err <- rmse_subset_ntrials_as_df(list('trn'=train),ntrials)
saveRDS(control_err, file = './Output/control_err.rds')
gc()
###### Get RMSE Data by Element Subsets with no retraining
elemental_subsets <- list(fe_train, hg_train, cu_train, b2mg_train)
names(elemental_subsets) = c('Fe','Hg','Cu' ,'B2Mg')
elemental_err_no_retrain <- rmse_subset_ntrials_as_df(elemental_subsets,ntrials)
saveRDS(elemental_err_no_retrain, file = './Output/elemental_err_n_rt.rds')
gc()
###### Get RMSE Data by Element Subsets with retraining 
elemental_err_retrained <- rmse_subset_ntrials_as_df(elemental_subsets,ntrials,retrain = TRUE)
rm(elemental_subsets)
gc()
saveRDS(elemental_err_retrained, file = './Output/elemental_err_rt.rds')
###### Get RMSE Data by Quartile
filt <- function(x) subset(x,select = -c(decile, quartile))
quartiles <- lapply(train_quartiles,filt)
rm(train_quartiles)
names(quartiles) <- c(1:4)
quartile_err <- rmse_subset_ntrials_as_df(quartiles,ntrials)
rm(quartiles)
gc()
saveRDS(quartile_err, file = './Output/quartile_err.rds')

###### Get RMSE Data by Decile
deciles <- lapply(train_deciles,filt)
rm(train_deciles)
names(deciles) <- c(1:10)
decile_err <- rmse_subset_ntrials_as_df(deciles,ntrials)
rm(deciles)
gc()
saveRDS(decile_err, file = './Output/decile_err.rds')

##### Get Error Statistics by output quartile
output_quartile_errs <- err_output_quartiles_ntrials(ntrials)
saveRDS(output_quartile_errs, file = './Output/output_quartile_errs.rds')

})


