getwd()
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
library(glmnet)

sst_cv <- readRDS("data/processed/sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")

# lasso_standardized <- cv_for_ts(sst = sst_cv, precip = precip_cv, nfold = 5, 
#                               size_train = 60, size_test = 14,
#                               save_folder = "cv-lasso-standardize",
#                               model = "lasso", 
#                               include_ts_vars=FALSE,
#                               stand=TRUE, diff_features=FALSE, 
#                               des_features=FALSE)

lasso_standardized <- cv_for_ts(sst = sst_cv, precip = precip_cv, nfold = 5, 
                                size_train = 60, size_test = 14,
                                save_folder = "cv-lasso-standardize-updated",
                                model = "lasso", 
                                include_ts_vars=FALSE,
                                stand=FALSE, diff_features=FALSE, 
                                des_features=FALSE,
                                standardize_features = TRUE,
                                standardize_response = TRUE)
