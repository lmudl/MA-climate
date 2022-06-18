# analyse CV
setwd("Repos/MA-climate/")
library(Hmisc)
library(raster)
source("code/R/helper-functions.R")
# load the error-matrix, lambda and define filepath for saving the plots ####
err_mat <- readRDS("results/CV-lasso/cv-lasso-og-data-16-06-22/err-mat.rds")
lambdas <- readRDS("results/CV-lasso/cv-lasso-og-data-16-06-22/lambda-vec.rds")
save_to <- "results/CV-lasso/cv-lasso-og-data-16-06-22"
model_list <- load_models("results/CV-lasso/cv-lasso-og-data-16-06-22/fold-models")

plot_save_errors(err_mat, lambdas, save_to)
plot_all_coef_maps(model_list, err_mat = err_mat, save_to=save_to)

# Data preperation 
ids <- readRDS("results/CV-lasso/cv-lasso-og-data-16-06-22/index-list.rds")
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst <- as.matrix(sst)
sst <- add_colnames("data/interim/sst/ersst_setreftime.nc",sst)
sst <- prepare_sst(sst)
dim(sst)
anyNA(sst)
precip <- as.matrix(precip)
precip <- apply(precip, 2, mean)

plot_predictions_best_l(err_mat, model_list, ids, features=sst, target=precip,
                        save_to = save_to)


