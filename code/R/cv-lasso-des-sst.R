# cv lasso des sst
# do analysis for different data sets and combinations
getwd()
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
library(glmnet)


des_sst_cv <- readRDS("data/processed/des_sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")

lasso_des_sst <- cv_for_ts(sst = des_sst_cv, precip = precip_cv, nfold = 5, 
                      size_train = 60, size_test = 14,
                      save_folder = "cv-lasso-des-sst",
                      model = "lasso", 
                      include_ts_vars=FALSE,
                      stand=FALSE, diff_features=FALSE, 
                      des_features=FALSE)

