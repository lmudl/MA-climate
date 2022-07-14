# inspect coefficient plots
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
model_name <- "small-fused-1k-gamma-01"
# TODO change
model_path <- paste0("results/CV-fused/small-fused-cv-1k-gamma-01", "/")
path_config <- paste0("code/R/fused-lasso/", model_name, "/config-", model_name, ".yml")
conf <- config::get(file = path_config)

sst_path <- conf$features_cv_path
precip_path <- conf$target_cv_path
standardize_features <- conf$standardize_features

sst_cv <- readRDS(sst_path)
precip_cv <- readRDS(precip_path)

ids <- readRDS(paste0(model_path, "index-list.rds"))
err_mat <- readRDS(paste0(model_path, "err-mat.rds"))
mins <- apply(err_mat, 2, which.min)

model_list <- load_models(paste0(model_path, "fold-models"))

plot_coef_maps_fused(model_list, err_mat, save_to = model_path,
                     features = sst_cv,
                     target = precip_cv,
                     standardize_features = standardize_features,
                     ids = ids,
                     drop_out  = TRUE)
debug(plot_coef_maps_fused)
