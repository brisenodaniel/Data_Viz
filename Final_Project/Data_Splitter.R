rm(list = ls())
library(dplyr)
library(purrr)
load('./Data/tc.RData')


########################### Split by Element #################################################
cutter <- function( elem_list, exclusive = FALSE){
  if (!exclusive) {
    df_train <- train %>% filter(unique_m[,elem_list] != 0)
    df_unique_m <- unique_m %>% filter(unique_m[,elem_list] != 0)
    return(list(df_train, df_unique_m))
  }
  else {
    df_train <- train %>% filter(unique_m[,elem_list] == 0)
    df_unique_m <- unique_m %>% filter(unique_m[,elem_list] == 0)
    return(list(df_train, df_unique_m))
  }
}


############################ Elements with Iron 
fe_train <- data.frame(cutter('Fe')[1])
fe_unique_m <- data.frame(cutter('Fe')[2])

# ############################# Elements with no Iron
# no_fe_train <- data.frame(cutter('Fe', exclusive = TRUE)[1])
# no_fe_unique_m <- data.frame(cutter('Fe',exclusive= TRUE)[2])


############################ Elements with Mercury 
hg_train <- data.frame(cutter('Mg')[1])
hg_unique_m <- data.frame(cutter('Mg')[2])
# 
# ########################### Elements with no Mercury
# no_hg_train <- data.frame(cutter('Hg', exclusive = TRUE)[1])
# no_hg_unique_m <- data.frame(cutter('Hg', exclusive = TRUE)[2])


############################# Cuprates 
cu_train <- data.frame(cutter('Cu')[1])
cu_unique_m <- data.frame(cutter('Cu')[2])



# ############################# Non-Cuprates
# no_cu_train <- data.frame(cutter('Cu', exclusive = TRUE)[1])
# no_cu_unique_m <- data.frame(cutter('Cu', exclusive = TRUE)[2])
# 

######################### Split By Tc Quantile #######################



train_tile <- train %>% mutate(decile = ntile(train$critical_temp, 10), 
                               quartile = ntile(train$critical_temp,4))

######################## Deciles
flt <- function(x) filter(train_tile, decile==x)
train_deciles <- lapply(c(1:10),flt)

flt <- function(x) filter(unique_m, train_tile$decile==x)
unique_m_deciles <- lapply(c(1:10),flt)



######################### Quantiles
flt <- function(x) filter(train_tile, quartile==x)
train_quartiles <- lapply(c(1:4), flt)


flt <- function(x) filter(unique_m, train_tile$quartile==x)
unique_m_quartiles <- lapply(c(1:4),flt)


####################### Test and Training Data Functions

partition <- function(data, train_partition){
  train_idx <- sample(1:nrow(data), train_partition*nrow(data))
  test_idx <- setdiff(1:nrow(data), train_idx)
  train_dat <- data[train_idx,]
  test_dat <- data[test_idx,]
  
  return(list('train'=train_dat, "test"=test_dat))
}









# Found duplicate data in train dataframe
# This might be an error as distinct materials are exteremely unlikely to have
# exactly the same properties. In fact, this may be impossible

# The following code removes all duplicate data from the df



rmDuplicates <- function(data, nms) {
  dupes <- duplicated(data)
  dupes_idx <- which(dupes)
  data_nd <- data[-dupes_idx]
  nms_nd <- nms[-dupes_idx]
  return( list(data_nd, nms_nd) )
}
# 
# unduped <- rmDuplicates(train, unique_m)
# train <- unduped[[1]]
# unique_m <- unduped[[2]]
# head(train)
# head(unique_m)
