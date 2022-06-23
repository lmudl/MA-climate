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

cluster_means_list <- readRDS("results/CV-lasso/cluster-cv-lasso-og2/cluster-means-list.rds")
evaluate_cluster_cv("results/CV-lasso/cluster-cv-lasso-og2", cluster_means_list, sst_cv, 5)

