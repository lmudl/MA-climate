# do analysis for different data sets and combinations
getwd()
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
library(caret)
library(glmnet)

#save_to_folder <- "cv-lasso-og-data-31-05-22"

# analysis of deseasonalised data already done see cv-lasso-08-11-21 ####
# done like in cv-for-ts

# analysis of original data already done see cv-lasso-12-05 ####
# done in regress
target_path <- "data/interim/drought/chirps_setreftime_aggregated.rds"
features_path <- "data/interim/sst/ersst_setreftime.nc"
target <- load_data(target_path)
target <- values(target)
target <- apply(target, 2, mean)
features <- load_data(features_path, "sst")
features <- add_colnames(features_path, features)
features_cv2 <- features[1:370,]
target_cv2 <- target[1:370]
lasso_on_og_data <- cv_for_ts(features_cv2, target_cv2, nfold = 5, size_train = 60, size_test = 14,
                 save_folder = "cv-lasso-og-data-31-05-22")
# plot results
ids <- createTimeSlices(1:370, initialWindow=60, horizon=14,
                        skip=60+14-1)
lambdas <- readRDS("results/CV-lasso/cv-lasso-og-data-31-05-22/lambda-vec.rds")
err_mat <- readRDS("results/CV-lasso/cv-lasso-og-data-31-05-22/err-mat.rds")


# features_cv2 <- add_colnames(features_path, features_cv2)
# features_cv2 <- prepare_sst(features_cv2)
# dim(features_cv2)
features_cv2 <- prepare_sst(features_cv2)
plot_and_save_cv_results(err_mat, 5, ids, lambdas, features_cv2, target_cv2,
                         save_to = "results/CV-lasso/cv-lasso-og-data-31-05-22")
