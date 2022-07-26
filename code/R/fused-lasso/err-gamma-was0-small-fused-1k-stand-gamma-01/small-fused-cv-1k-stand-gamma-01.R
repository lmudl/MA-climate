path_config <- "code/R/fused-lasso/small-fused-1k-stand-gamma-01/config-small-fused-1k-stand-gamma-01.yml"
conf <- config::get(file = path_config)
# setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
source("code/R/helper-functions-parallel-cv.R")

# load packages
library(sp)
library(raster)
library(igraph)
library(genlasso)

# load data
features <- readRDS(conf$features_cv_path)
targets <- readRDS(conf$target_cv_path)

# load graph
if(conf$small) {
  g <- readRDS("data/processed/small_graph_sst.rds")
} 
if(!conf$small) {
  g <- readRDS("data/processed/graph_sst.rds")
}

cv_for_ts(sst = features, 
          precip = targets, 
          nfold = conf$nfold, 
          size_train = conf$size_train,
          size_test = conf$size_test,
          save_folder = conf$save_folder,
          model = conf$model,
          parallelize = conf$parallelize,
          graph = g,
          maxsteps = conf$maxsteps,
          include_ts_vars = conf$include_ts_vars,
          diff_features = conf$diff_features,
          des_features = conf$des_features,
          standardize_features = conf$standardize_features, 
          standardize_response = conf$standardize_response,
          gamma = conf$gamma)