# fused cv-full
# fused lasso test with new cv method
getwd() # is MA-climate on the shell 
# but we could add a check here
# setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")

# load packages
# requiredPackages = c('raster','igraph','glmnet', 'caret')
# for(p in requiredPackages){
#   if(!requid.Rre(p,character.only = TRUE)) install.packages(p)
#   library(p,character.only = TRUE)
# }
library(sp)
library(raster)
library(igraph)
library(genlasso)
#library(caret)
train_max <- 1:370
maxsteps <- 1000

# load data sst
sst <- brick("data/interim/sst/ersst_setreftime.nc", var = "sst")
ext <- extent(-180,0,-50,40)
sst <- crop(sst,ext)
# create igraph object
g <- igraph_from_raster(sst)
rm(sst)

small_sst_cv <- readRDS("data/processed/small_sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")
# drop intercept

err <- cv_for_ts(small_sst_cv, precip_cv, nfold=5, size_train=60, size_test=14, 
                 save_folder="small-fused-cv-1k-stand-updated",
                 model = "fused", graph = g, maxsteps = maxsteps,
                 stand=FALSE, standardize_features = TRUE,
                 standardize_response = TRUE)

# create plots from results
library(dplyr)
library(ggplot2)
model_list <- load_models("results/CV-lasso/small-fused-cv-1k-stand-updated/fold-models/")
err_mat <- readRDS("results/CV-lasso/small-fused-cv-1k-stand-updated/err-mat.rds")
ids <- readRDS("results/CV-lasso/small-fused-cv-1k-stand-updated/index-list.rds")

plot_all_fold_error_fused(model_list, err_mat = err_mat, save_to="results/CV-lasso/small-fused-cv-1k-stand-updated/")
plot_errline_gg_fused(model_list, err_mat, save_to="results/CV-lasso/small-fused-cv-1k-stand-updated/")
readRDS("results/CV-lasso/small-fused-cv-1k-stand-updated/err-mat-plots/err-line-plot.rds")
plot_predictions_best_l_fused(err_mat = err_mat,
                              model_list = model_list,
                              ids = ids, features = small_sst_cv,
                              target = precip_cv,
                              standardize_features = TRUE,
                              standardize_response = TRUE,
                              save_to="results/CV-lasso/small-fused-cv-1k-stand-updated/")
