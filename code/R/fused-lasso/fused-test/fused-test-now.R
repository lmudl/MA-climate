# test cv #####
path_config <- "code/R/fused-lasso/fused-test/config-fused-test.yml"
cons <- config::get(file = path_config)
# setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")

# load packages
library(sp)
library(raster)
library(igraph)
library(genlasso)

# load data
features <- readRDS(cons$features_cv_path)
targets <- readRDS(cons$target_cv_path)

# load graph
if(cons$small) {
  g <- readRDS("data/processed/small_graph_sst.rds")
} 
if(!cons$small) {
  g <- readRDS("data/processed/graph_sst.rds")
}

res <- cv_for_ts(sst = features, 
          precip = targets, 
          nfold = cons$nfold, 
          size_train = cons$size_train,
          size_test = cons$size_test,
          save_folder = cons$save_folder,
          model = cons$model,
          graph = g,
          maxsteps = cons$maxsteps,
          include_ts_vars = cons$include_ts_vars,
          diff_features = cons$diff_features,
          des_features = cons$des_features,
          standardize_features = cons$standardize_features, 
          standardize_response = cons$standardize_response,
          gamma = cons$gamma,
          parallelize = TRUE)
res
# res <- readRDS("results/CV-fused/fused-cv-test/err-mat.rds")
# res
#debug(cv_fused_lasso)

