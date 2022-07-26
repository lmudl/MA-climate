# do analysis for different data sets and combinations
getwd()
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
#library(caret)
library(glmnet)

#save_to_folder <- "cv-lasso-og-data-31-05-22"
# 
# features_cv <- sst[1:370,]
# target_cv <- precip[1:370]

sst_cv <- readRDS("data/processed/noclust_sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")

lasso_og <- cv_for_ts(sst = sst_cv, precip = precip_cv, nfold = 5, 
                              size_train = 60, size_test = 14,
                              save_folder = "noclust-lasso",
                              model = "lasso", 
                              include_ts_vars=FALSE,
                              stand=FALSE, diff_features=FALSE, 
                              des_features=FALSE)
# plot results
# ids <- createTimeSlices(1:370, initialWindow=60, horizon=14,
#                         skip=60+14-1)
# lambdas <- readRDS("results/CV-lasso/cv-lasso-og-data-16-06-22/lambda-vec.rds")
# err_mat <- readRDS("results/CV-lasso/cv-lasso-og-data-16-06-22/err-mat.rds")
# 
# 
# # features_cv2 <- add_colnames(features_path, features_cv2)
# # features_cv2 <- prepare_sst(features_cv2)
# # dim(features_cv2)
# features_cv2 <- prepare_sst(features_cv2)
# plot_and_save_cv_results(err_mat, 5, ids, lambdas, features_cv2, target_cv2,
#                          save_to = "results/CV-lasso/cv-lasso-og-data-16-06-22")
