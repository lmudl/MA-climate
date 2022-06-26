# evalute lasso og timelag

# analyse CV
setwd("Repos/MA-climate/")
library(raster)
library(ggplot2)
source("code/R/helper-functions.R")
# load the error-matrix, lambda and define filepath for saving the plots ####
err_mat <- readRDS("results/CV-lasso/cv-lasso-og-timelag-25-06-22-rm4/err-mat.rds")
lambdas <- readRDS("results/CV-lasso/cv-lasso-og-timelag-25-06-22-rm4/lambda-vec.rds")
save_to <- "results/CV-lasso/cv-lasso-og-timelag-25-06-22-rm4"
model_list <- load_models("results/CV-lasso/cv-lasso-og-timelag-25-06-22-rm4/fold-models")

# Data preperation 
ids <- readRDS("results/CV-lasso/cv-lasso-og-timelag-25-06-22-rm4/index-list.rds")
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst <- as.matrix(sst)
sst <- add_colnames("data/interim/sst/ersst_setreftime.nc",sst)
sst <- prepare_sst(sst)
dim(sst)
anyNA(sst)
precip <- as.matrix(precip)
precip <- apply(precip, 2, mean)

plot_save_errors(err_mat, lambdas, save_to)
#plot_coef_maps(model_list, err_mat = err_mat, save_to=save_to)
plot_predictions_best_l(err_mat, model_list, ids, features=sst, target=precip,
                        lambdas, save_to = save_to, standardize=FALSE,
                        include_ts_vars=TRUE)
