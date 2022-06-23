# apply lasso on the clustering results
# lasso og non-centered or so
library(raster)
library(glmnet)
library(ggplot2)
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
kmeans_result <- readRDS("results/clustering/km_pca_result.rds")
# kmeans_result

# save all to cluster-cv-lasso-og2

# load original precipitation data
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
#precip <- as.matrix(precip)
#precip <- precip[,1:370]



cluster_precip_list <- create_cluster_precip_list(precip = precip,
                                          clustering_result = kmeans_result)

cluster_means_list <- compute_cluster_means(cluster_precip_list)

saveRDS(cluster_means_list, "results/CV-lasso/cluster-cv-lasso-og2/cluster-means-list.rds")

for(i in seq(length(cluster_means_list))) {
  cluster_means_list[[i]] <- cluster_means_list[[i]][1:370]
}


sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst <- as.matrix(sst)
sst <- add_colnames("data/interim/sst/ersst_setreftime.nc",sst)
sst <- prepare_sst(sst)
dim(sst)
anyNA(sst)
features <- sst[1:370,]

debug(run_cv_over_clusters)
run_cv_over_clusters(features, cluster_means_list, nfold=5, size_train=60, 
                     size_test=14, 
                     save_folder_cluster_result = "cluster-cv-lasso-og2")

