# quick fix would be to differentiate
# all 1 time and drop the one that needs 2

# do analysis for different data sets and combinations
getwd()
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
#library(caret)
library(glmnet)
#library(zoo)
library(dplyr)
library(tidyverse)
library(lubridate)
library(feasts)

precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst <- as.matrix(sst)
sst <- add_colnames("data/interim/sst/ersst_setreftime.nc",sst)
sst <- prepare_sst(sst)
dim(sst)
anyNA(sst)
precip <- as.matrix(precip)
precip <- apply(precip, 2, mean)

features_cv <- sst[1:370,]
target_cv <- precip[1:370]

lasso_on_des <- cv_for_ts(features_cv, target_cv, nfold = 5, size_train = 60, size_test = 14,
                              save_folder = "cv-lasso-des",
                              include_ts_vars=FALSE, stand=FALSE, diff_features=FALSE,
                              des_features=TRUE)
# debug(cv_for_ts)
# lasso_on_og_data_timelag
# la <- readRDS("results/CV-lasso/cv-lasso-og-timelag-25-06-22/fold-models/lambda-vec-fold5.rds")
# apply(lasso_on_og_data_timelag, 2, which.min)