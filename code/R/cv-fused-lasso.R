# do analysis for different data sets and combinations
getwd()
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
library(caret)
library(glmnet)
# test on smaller set already done
# see functions-for-igraph
# helpers ####
create_coords <- function(vec) {
  x_vec <- rep(c(1:vec[1]), vec[2])
  y_vec <- c()
  for(i in 1:vec[2]) {
    y_vec <- append(y_vec,(rep(i, vec[1])))
  }
  df <- as.data.frame(cbind(x_vec, y_vec))
  return(df)
}


# plot(e, vertex.size = 0.001, edge.width=0.00001,
#      vertex.label = labels)
# begins top left

igraph_from_raster <- function(raster_object) {
  dims <- dim(raster_object)[1:2]
  dims <- rev(dims)
  g <- make_lattice(dims)
  ec <- create_coords(dims)
  V(g)$x <- ec$x_vec
  V(g)$y <- rev(ec$y_vec)
  vals <- values(raster_object[[1]])
  land <- which(is.na(vals))
  g <- delete_vertices(g, land)
  return(g)
}

# load and prepare ####
target_path <- "data/interim/drought/chirps_setreftime_aggregated.rds"
features_path <- "data/interim/sst/ersst_setreftime.nc"
target <- load_data(target_path)
target <- values(target)
target <- apply(target, 2, mean)
features <- load_data(features_path, "sst")


sst <- brick("data/interim/sst/ersst_setreftime.nc", var = "sst")
# ext <- extent(-180,0,-50,40)
# sst <- crop(sst,ext)
g <- igraph_from_raster(sst)
plot(g, vertex.size = 0.001, edge.width=0.00001,
     vertex.label = NA)

features <- add_colnames(features_path, features)
features <- features[1:370,]
target <- target[1:370]
# will have to rewrite cv_for_ts with fused option
# lasso_on_og_data <- cv_for_ts(features_cv2, target_cv2, nfold = 5, size_train = 60, size_test = 14,
#                               save_folder = "cv-fused-lasso")
# for the cv for ts 
# inside the for j in 1_length loop
# before trained_model add variable model = lasso as default.
# and graph=NULL
# check if model=lasso then graph must not be NULL

if(model = "lasso") {
  trained_model <- glmnet(x_train, y_train, lambda=rev(lambda_vec))
  #TODO change value her for s
  predicted <- predict(trained_model, newx = data.matrix(x_test), s = rev(lambda_vec))
  err_col <- apply(predicted, 2, function(x) mean((x-y_test)^2))
  err_mat[,j] <- err_col
}


if(model = "fused_lasso") {
  trained_model <- fusedlasso(y=y_train, X=x_train, graph=g)
  # test predictions with Fusedlasso model
}

# fi