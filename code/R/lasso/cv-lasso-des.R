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

sst_cv <- readRDS("data/processed/sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")

lasso_on_des <- cv_for_ts(features_cv, target_cv, nfold = 5, size_train = 60, size_test = 14,
                              save_folder = "cv-lasso-des",
                              include_ts_vars=FALSE, stand=FALSE, diff_features=FALSE,
                              des_features=TRUE)
# debug(cv_for_ts)
# lasso_on_og_data_timelag
# la <- readRDS("results/CV-lasso/cv-lasso-og-timelag-25-06-22/fold-models/lambda-vec-fold5.rds")
# apply(lasso_on_og_data_timelag, 2, which.min)