# analyse CV
setwd("Repos/MA-climate/")
library(raster)
library(ggplot2)
source("code/R/helper-functions.R")

model_folder <- "cv-lasso-des-sst-des-precip"
save_to <- paste0("results/CV-lasso/", model_folder, "/")

# load the error-matrix, lambda and the model_list ####
err_mat <- readRDS(paste0(save_to,"err-mat.rds"))
lambdas <- readRDS(paste0(save_to,"lambda-vec.rds"))
ids <- readRDS(paste0(save_to, "index-list.rds"))
model_list <- load_models(paste0(save_to,"fold-models"))

# Data loading
des_sst_cv <- readRDS("data/processed/des_sst_cv.rds")
des_precip_cv <- readRDS("data/processed/des_precip_cv.rds")

plot_save_errors(err_mat = err_mat, lambdas = lambdas, save_to = save_to)
plot_coef_maps(model_list = model_list, err_mat = err_mat, save_to=save_to)
plot_predictions_best_l(err_mat = err_mat, model_list = model_list, ids = ids, 
                        features=des_sst_cv, target=des_precip_cv,
                        lambdas = lambdas, save_to = save_to)

