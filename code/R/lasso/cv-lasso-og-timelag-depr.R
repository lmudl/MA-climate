# do analysis for different data sets and combinations
getwd()
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
library(caret)
library(glmnet)
library(tsutils)

# analysis of original data already done see cv-lasso-12-05 ####
# done in regress
target_path <- "data/interim/drought/chirps_setreftime_aggregated.rds"
features_path <- "data/interim/sst/ersst_setreftime.nc"
target <- load_data(target_path)
target <- values(target)
target <- apply(target, 2, mean)
features <- load_data(features_path, "sst")
features <- add_colnames(features_path, features)
dim(features)
#library(tsutils)
# add timelags
dim(features)
features <- t(na.omit(t(features)))
dim(features)
lag_mat <- matrix(nrow=nrow(features),ncol=1)
lag <- 3
# for(i in 1:ncol(features)) {
#   lag <- lagmatrix(features[,i], c(1:3))
#   lag_mat <- cbind(lag_mat, lag)
#   print(paste("done with",i))
# }
# saveRDS(lag_mat, "results/CV-lasso/cv-lasso-og-timelag-03-06-22/lag_mat.rds")
lag_mat <- readRDS("results/CV-lasso/cv-lasso-og-timelag-03-06-22/lag_mat.rds")
# test(head(lag_mat[,1:10]))
# drop_na_from_lag_mat <- function(lag_mat) {
#   keep_col <- apply(lag_mat, 2, function(x) !all(is.na(x)))
#   lag_mat <- lag_mat[,keep_col]
#   keep_row <- apply(lag_mat, 1, function(x) !any(is.na(x)))
#   lag_mat <- lag_mat[keep_row,]
#   return(lag_mat)
# }
# drop_na_from_lag_mat(test)
lag_mat <- lag_mat[-c(1:lag),-1]
dim(lag_mat)[2] == dim(features)[2]*3
features <- features[-c(1:lag),]
dim(lag_mat)[1] == dim(features)[1]
features <- cbind(features, lag_mat)
saveRDS(features, "results/CV-lasso/cv-lasso-og-timelag-03-06-22/timelag_features.rds")

features_cv6 <- features[1:370,]
target_cv6 <- target[-c(1:3)]
target_cv6 <- target[1:370]

lasso_on_og_data <- cv_for_ts(features_cv6, target_cv6, nfold = 5, size_train = 60, size_test = 14,
                              save_folder = "cv-lasso-og-timelag-03-06-22")
# plot results
ids <- createTimeSlices(1:370, initialWindow=60, horizon=14,
                        skip=60+14-1)
lambdas <- readRDS("results/CV-lasso/cv-lasso-og-timelag-03-06-22/lambda-vec.rds")
err_mat <- readRDS("results/CV-lasso/cv-lasso-og-timelag-03-06-22/err-mat.rds")


# features_cv2 <- add_colnames(features_path, features_cv6)
# features_cv2 <- prepare_sst(features_cv6)
# dim(features_cv2)

# will not directly work with timelag because we have other variables included with timelag
# features_cv2 <- prepare_sst(features_cv6)
# plot_and_save_cv_results(err_mat, 5, ids, lambdas, features_cv6, target_cv6,
#                          save_to = "results/CV-lasso/cv-lasso-og-timelag-03-06-22")
