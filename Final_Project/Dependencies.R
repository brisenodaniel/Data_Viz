#Install and load required packages
if(!require(pacman)){
  install.packages('pacman')
}

pacman::p_load(dplyr,
                stringr,
                xgboost,
                Matrix,
                Metrics)

#Prepare Global Environment
rm(list = ls())
load('./Data/tc.RData')