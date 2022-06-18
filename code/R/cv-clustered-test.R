# apply lasso on the clustering results
# lasso og non-centered or so
library(raster)
library(glmnet)
library(ggplot2)
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
kmeans_result <- readRDS("results/clustering/km_pca_result.rds")
kmeans_result


# load original precipitation data
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")

create_cluster_id_list <- function(precip_raster, clustering_result) {
  cluster_id_list <- list()
  vals <- getValues(precip)
  for(i in 1:max(clustering_result$cluster)) {
    cluster_id_list[[i]] <- vals[clustering_result$cluster==i,]
  }
  return(cluster_id_list)
}

cluster_id_list <- create_cluster_id_list(precip_raster = precip,
                                          clustering_result = kmeans_result)

compute_cluster_means <- function(cluster_id_list) {
  for(i in 1:length(cluster_id_list)) {
    cluster_id_list[[i]] <- apply(cluster_id_list[[i]], 2, mean)
  }
  return(cluster_id_list)
}

cluster_means_list <- compute_cluster_means(cluster_id_list)

run_cv_over_clusters <- function(sst,cluster_means_list, nfold, size_train,
                                 size_test, save_folder_cluster_result) {
  # we can create ids for test and train already here
  dir.create(paste0("results/CV-lasso/", save_folder_cluster_result))
  ncluster <- length(cluster_means_list)
  for (i in 1:ncluster) {
    # create save_folder sub directory for each cluster
    save_folder <- paste0(save_folder_cluster_result,"/cluster-",i)
    cluster_means_list[[i]] <- cluster_means_list[[i]][1:370]
    err_mat <- cv_for_ts(sst, cluster_means_list[[i]],
                               nfold, size_train, size_test, save_folder)
    # cluster_model is err_mat each fold is column
    print(paste("cluster", i, "finished"))
    lambdas <- readRDS(paste0("results/CV-lasso/",save_folder,"/lambda-vec.rds"))
    ids <- createTimeSlices(1:nrow(sst), initialWindow=size_train, horizon=size_test,
                            skip=size_train+size_test-1)
    sst <- prepare_sst(sst)
    plot_and_save_cv_results(err_mat, nfold, ids, lambdas, sst, cluster_means_list[[i]],
                             save_to = paste0("results/CV-lasso/",save_folder))
  }
}

target_path <- "data/interim/drought/chirps_setreftime_aggregated.rds"
features_path <- "data/interim/sst/ersst_setreftime.nc"
features <- load_data(features_path, "sst")
features <- add_colnames(features_path, features)
features <- features[1:370,]
run_cv_over_clusters(features, cluster_means_list, nfold=5, size_train=60, 
                     size_test=14, 
                     save_folder_cluster_result = "cluster-cv-lasso-og")
