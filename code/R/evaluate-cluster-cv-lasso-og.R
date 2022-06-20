# analyse CV
setwd("Repos/MA-climate/")
library(raster)
library(ggplot2)
library(glmnet)
source("code/R/helper-functions.R")
# load the error-matrix, lambda and define filepath for saving the plots ####

# Data preperation 
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst <- as.matrix(sst)
sst <- add_colnames("data/interim/sst/ersst_setreftime.nc",sst)
sst <- prepare_sst(sst)
sst_cv <- sst[1:370,]

cluster_means_list <- readRDS("results/CV-lasso/cluster-cv-lasso-og/cluster-means-list.rds")

evaluate_cluster_cv <- function(path_to_model_folder, cluster_means_list,
                                sst, ncluster) {
  for(i in seq(ncluster)) {
    load_path <- paste0(path_to_model_folder, "/cluster-", i, "/")
    dir.create(load_path)
    err_mat <- readRDS(paste0(load_path, "err-mat.rds"))
    lambdas <- readRDS(paste0(load_path, "lambda-vec.rds"))
    ids <- readRDS(paste0(load_path, "index-list.rds"))
    model_list <- load_models(paste0(load_path,"fold-models"))
    plot_save_errors(err_mat, lambdas, save_to = load_path)
    plot_coef_maps(model_list, err_mat = err_mat, save_to=load_path)
    precip <- cluster_means_list[[i]]
    plot_predictions_best_l(err_mat, model_list, ids, features=sst, target=precip,
                            lambdas, save_to = load_path)
  }
}

evaluate_cluster_cv("results/CV-lasso/cluster-cv-lasso-og", cluster_means_list, sst_cv, 5)
