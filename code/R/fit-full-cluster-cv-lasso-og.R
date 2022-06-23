# fit-full model for each cluster
setwd("Repos/MA-climate")
source("code/R/helper-functions.R")
library(glmnet)
library(ggplot2)
# load data

# start loop like in evaluate clustered
# then fit and plot full model

save_to <- "cluster-cv-lasso-og2"
full_save_to <- paste0("results/CV-lasso/", save_to, "/")

#inside loop with function
#load err_mat, load lambdas
#get lminid and lmin

#load means of precip

#load sst 
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst <- as.matrix(sst)
sst <- add_colnames("data/interim/sst/ersst_setreftime.nc",sst)
sst <- prepare_sst(sst)

fit_full_model_for_cluster(sst, full_save_to, ncluster=5)

