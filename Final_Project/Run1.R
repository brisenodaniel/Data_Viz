library(xgboost)
library(CHNOSZ)

load('./Data/tc.RData')


predict_tc("Ba0.2La1.8Cu1O4", verbose = TRUE)
predict_tc("MgB2")
predict_tc("Hg")
predict_tc("Ca0.5Sr0.5C6", verbose = TRUE)
predict_tc("NaSn2As2", verbose = TRUE)
predict_tc("H2S", verbose = TRUE)
predict_tc("FCl", verbose = TRUE)
predict_tc("mgB2", verbose = TRUE)

write.csv(train, "train.csv", row.names = F)
write.csv(unique_m, "unique_m.csv", row.names = F)

head(train)
head(unique_m)
