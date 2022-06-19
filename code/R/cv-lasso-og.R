# do analysis for different data sets and combinations
getwd()
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
#library(caret)
library(glmnet)

#save_to_folder <- "cv-lasso-og-data-31-05-22"

# analysis of deseasonalised data already done see cv-lasso-08-11-21 ####
# done like in cv-for-ts

# analysis of original data already done see cv-lasso-12-05 ####
# done in regress
# target_path <- "data/interim/drought/chirps_setreftime_aggregated.rds"
# features_path <- "data/interim/sst/ersst_setreftime.nc"
# target <- load_data(target_path)
# target <- values(target)
# target <- apply(target, 2, mean)
# features <- load_data(features_path, "sst")
# features <- add_colnames(features_path, features)

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

lasso_on_og_data <- cv_for_ts(features_cv, target_cv, nfold = 5, size_train = 60, size_test = 14,
                 save_folder = "cv-lasso-og-data-16-06-22")
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
