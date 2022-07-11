# evaluate small fused cv 1k
getwd()
setwd("Repos/MA-climate/")
library(genlasso)
library(ggplot2)
library(dplyr)
source("code/R/helper-functions.R")
model_path <- "results/CV-lasso/small-fused-cv-1k-stand-updated/"
maxsteps <- 1000
# load small sst eval
small_sst_cv <- readRDS("data/processed/small_sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")

ids <- readRDS(paste0(model_path, "index-list.rds"))
err_mat <- readRDS(paste0(model_path, "err-mat.rds"))
mins <- apply(err_mat, 2, which.min)

# load models
model_list <- load_models(paste0(model_path, "fold-models"))

# plot predictions
plot_predictions_best_l_fused(err_mat, model_list, ids,features = small_sst_cv,
                              target = precip_cv, save_to=model_path,
                              standardize_features = FALSE)
readRDS(paste0(model_path,"/pred-plots/pred-plot-fold-5.rds"))


# plotting errors ####

plot_all_fold_error_fused(model_list, err_mat, save_to=model_path)
p <- plot_errline_gg_fused(model_list, err_mat, save_to=model_path)

p1 <- readRDS(paste0(model_path, "err-mat-plots/err-line-plot.rds"))
p2 <- readRDS(paste0(model_path, "err-mat-plots/err-plot-fold5.rds"))
p2

# PLOT non-coefficients ####
plot_coef_maps_fused(model_list, err_mat, save_to=model_path, small_sst_cv,
                     target = precip_cv, standardize_features = FALSE, ids)

readRDS(paste0(model_path, "/coef-plots/coef-plot-fold-5.rds"))

plot_coef_maps_fused(model_list, err_mat, save_to=model_path, small_sst_cv,
                     target = precip_cv, standardize_features = FALSE, ids,
                     drop_out = TRUE)
readRDS(paste0(model_path, "/coef-plots/coef-plot-drop-out-fold-1.rds"))



