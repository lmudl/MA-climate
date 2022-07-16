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

# Data loading
sst_cv <- readRDS("data/processed/sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")

lasso_on_og_data_timelag <- cv_for_ts(sst_cv, precip_cv, nfold = 5, size_train = 60, size_test = 14,
                              save_folder = "cv-lasso-og-timelag-28-06-22-lag-1-3",
                              include_ts_vars=TRUE, stand=FALSE)
# debug(cv_for_ts)
# lasso_on_og_data_timelag
# la <- readRDS("results/CV-lasso/cv-lasso-og-timelag-25-06-22/fold-models/lambda-vec-fold5.rds")
# apply(lasso_on_og_data_timelag, 2, which.min)
