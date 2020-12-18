#Install and load required packages
if(!require(pacman)){
  install.packages('pacman')
}

pacman::p_load(dplyr,
                stringr,
                xgboost,
                Matrix,
                Metrics,
               tidyverse,
               gridExtra,
               cowplot)

#Prepare Global Environment
rm(list = ls())
load('./Data/tc.RData')