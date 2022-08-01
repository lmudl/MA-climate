# do analysis for different data sets and combinations
getwd()
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
library(glmnet)

save_to <- "test-boxcox-lasso"
sst_cv_path <- "data/processed/sst_cv.rds"
precip_cv_path <- "data/processed/precip_cv.rds"
standardize_features <- FALSE
standardize_response <- FALSE
sst_eval_path <- "data/processed/sst_eval.rds"
precip_eval_path <- "data/processed/precip_eval.rds"
center_response <- FALSE
diff_n <- FALSE
des_features <- FALSE
boxcox_response <- TRUE

sst_cv <- readRDS(sst_cv_path)
precip_cv <- readRDS(precip_cv_path)

test_lasso <- cv_for_ts(sst = sst_cv, precip = precip_cv, nfold = 5, 
                        size_train = 60, size_test = 14,
                        save_folder = save_to,
                        model = "lasso", 
                        include_ts_vars=FALSE,
                        stand=FALSE, diff_features=FALSE, 
                        des_features=des_features,
                        center_response=FALSE,
                        standardize_features = standardize_features,
                        boxcox_response = TRUE)

err_bar_plot <- readRDS("results/CV-lasso/test-deseas-lasso/err-mat-plots/err-bars-plot.rds")
ll <- get_l_1se_high_low(err_bar_plot)

fit_eval_full_lasso(save_to, sst_cv_path, precip_cv_path,
                    sst_eval_path, precip_eval_path,
                    standardize_response, center_response,
                    standardize_features = standardize_features, diff_n,
                    des_features=des_features,
                    boxcox_response = boxcox_response)

