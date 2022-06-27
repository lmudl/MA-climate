# evalute lasso og timelag
# analyse CV
setwd("Repos/MA-climate/")
library(raster)
library(ggplot2)
source("code/R/helper-functions.R")

model_folder <- "cv-lasso-og-timelag-25-06-22-rm4"
save_to <- paste0("results/CV-lasso/", model_folder, "/")

# load the error-matrix, lambda and the model_list ####
err_mat <- readRDS(paste0(save_to,"err-mat.rds"))
lambdas <- readRDS(paste0(save_to,"lambda-vec.rds"))
ids <- readRDS(paste0(save_to, "index-list.rds"))
model_list <- load_models(paste0(save_to,"fold-models"))

# Data loading
sst_cv <- readRDS("data/processed/sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")


plot_save_errors(err_mat, lambdas, save_to)
#plot_coef_maps(model_list, err_mat = err_mat, save_to=save_to)
plot_predictions_best_l(err_mat, model_list, ids, features=sst_cv, target=precip_cv,
                        lambdas, save_to = save_to, standardize=FALSE,
                        include_ts_vars=TRUE)
