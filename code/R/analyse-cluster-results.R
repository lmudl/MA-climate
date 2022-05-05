setwd("Repos/MA-climate/")
library(factoextra)
library(raster)
library(dplyr)
library(patchwork)
library(cluster)
library(ggplot2)
library(ggfortify)
#source("code/R/helper-functions.R")
set.seed(1234)
plot <- readRDS("results/clustering/gap_pc3_centered_kmeans_plot.rds")
get_tibsh_k_from_gapplot <- function(df) {
  gk <- df[,"gap"]
  l <- length(gk)
  gk <- gk[-l]
  gk1 <- df[,"gap"][-1]
  sk1 <- df[,"SE.sim"][-1]
  d <- gk1 - sk1
  return(which(gk >= d)[1])
}
get_tibsh_k_from_gapplot(plot$data)

# helpers ####

get_cluster_id <- function(cluster_result, cluster_number) {
  cluster_id <- c()
  if(attributes(cluster_result)$class[1] == "kmeans") cluster_id <- cluster_result$cluster==cluster_number
  if(attributes(cluster_result)$class[1] == "pam") cluster_id <- cluster_result$clustering==cluster_number
  return(cluster_id)
}
add_cell_number <- function(df, ids) {
  l <- 1:ncol(df)
  z <-  l[ids]
  df <- as.data.frame(cbind(z, df))
  return(df)
}
tidy_df <- function(df) {
  df <- tidyr::pivot_longer(df, cols = 2:ncol(df),
                            names_to = "month",
                            values_to = "precip")
  df$month <- unlist(lapply(strsplit(df$month, "X"), function(x) as.numeric(x[2])))
  return(df)
}
gg_result <- function(df) {
  temp <- df %>% group_by(month) %>% summarise(precip_mean = mean(precip))
  df <- inner_join(df, temp, by="month")
  plt <- ggplot() +
    geom_line(data = df, aes(x=month, y = precip, group = z), color="grey") + 
    labs(y="Centered precipitation", x  = "Month") +
    geom_line(data=df, aes(x=month, y=precip_mean), colour="blue") 
  return(plt)
}
plot_cluster_intern <- function(og_data, cluster_result, cluster_number) {
  cluster_id <- get_cluster_id(cluster_result, cluster_number)
  cluster_df <- getValues(og_data)[cluster_id,]
  cluster_df <- add_cell_number(cluster_df, cluster_id)
  cluster_df <- tidy_df(cluster_df)
  plt <- gg_result(cluster_df)
  plt <- plt + ggtitle("Time series in cluster", cluster_number)
  return(plt)
}
# also need to load plot_kmeans and plot_pam in cluster-precip

# load data and do pca ####
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
# do pca #####
pca_centered <- prcomp(getValues(precip), retx = TRUE,
                       center = TRUE, scale. = FALSE)
# precip <- getValues(precip)

# analyse kmeans #####
# kmeans on map
kmeans_result <- kmeans(scale(getValues(precip),scale=FALSE,center=TRUE),centers = 5,nstart = 20,
                        iter.max = 30)
km_map_plot <- plot_kmeans(precip, kmeans_result)
#saveRDS(km_map_plot, "./results/clustering/km_map_plot.rds")

# kmeans on map after pca
km_pca_result <- kmeans(pca_centered$x[,1:3], centers=5,nstart=20,iter.max = 20)
saveRDS(km_pca_result, "results/clustering/km_pca_result.rds")
km_pca_map_plot <- plot_kmeans(precip, km_pca_result)
#saveRDS(km_pca_map_plot, "./results/clustering/km_pca_map_plot.rds")

# number of clusters, (number assignments can differ)
# 1   2   3   4   5 
# 234 156  66  91  65 

# compare
km_map_plot + km_pca_map_plot

# test <- apply(getValues(precip), 2, function(x) scale(x, center = TRUE, scale = FALSE))
# test2 <- getValues(scale(precip, center=TRUE, scale=FALSE))
# all.equal(test, test2, check.attributes = FALSE)

scaled_prec <- scale(precip, center=TRUE,scale=FALSE)
# kmeans inspect time series inside the clusters 
c1_km_plot <- plot_cluster_intern(scaled_prec, kmeans_result, 1)
c2_km_plot <- plot_cluster_intern(scaled_prec, kmeans_result, 2)
c3_km_plot <- plot_cluster_intern(scaled_prec, kmeans_result, 3)
c4_km_plot <- plot_cluster_intern(scaled_prec, kmeans_result, 4)
c5_km_plot <- plot_cluster_intern(scaled_prec, kmeans_result, 5)

# km_plot_list <- list(c1_km_plot, c2_km_plot, c3_km_plot, c4_km_plot,
#                      c5_km_plot)
# j <- 1
# for(i in km_plot_list) {
#   saveRDS(i, paste0("./results/clustering/c",j,"_km_plot.rds"))
#   j <- j+1
# }
# rm(km_plot_list)




# kmeans after pca, inspect time series inside the clusters
c1_pca_km_plot <- plot_cluster_intern(scaled_prec, km_pca_result, 1)
c2_pca_km_plot <- plot_cluster_intern(scaled_prec, km_pca_result, 2)
c3_pca_km_plot <- plot_cluster_intern(scaled_prec, km_pca_result, 3)
c4_pca_km_plot <- plot_cluster_intern(scaled_prec, km_pca_result, 4)
c5_pca_km_plot <- plot_cluster_intern(scaled_prec, km_pca_result, 5)

# pca_km_plot_list <- list(c1_pca_km_plot, c2_pca_km_plot, c3_pca_km_plot,
#                          c4_pca_km_plot, c5_pca_km_plot)
# j <- 1
# for(i in pca_km_plot_list) {
#   saveRDS(i, paste0("./results/clustering/c",j,"_pca_km_plot.rds"))
#   j <- j+1
# }
# rm(pca_km_plot_list)


sum1 <- km_plot + c1 + c2 + c3 + c4 + c5
sum2 <- km_pca_plot + p1 + p2 + p3 + p4 + p5


# analyse pam cluster results, maybe
pam_pca_result <- pam(pca_centered$x[,1:3], k=5)
pam_pca_map <- plot_pam(precip, pam_pca_result)
km_pca_plot + pam_pca_map

# boxplot ####
add_cluster <- function(df, cluster_result) {
  df <- as.data.frame(cbind(df, cluster_result$cluster))
  names(df)[max(ncol(df))] <- "cluster"
  return(df)
}
tidy_df2 <- function(df) {
  df <- tidyr::pivot_longer(df, cols = 2:(ncol(df)-1),
                            names_to = "month",
                            values_to = "precip")
  return(df)
}

plot_cluster_boxplot <- function(og_data, cluster_result) {
  cluster_df <- as.data.frame(getValues(og_data))
  cluster_df <- add_cluster(cluster_df, cluster_result)
  cluster_df <- tidy_df2(cluster_df)
  plt <- ggplot(cluster_df, aes(group=cluster, y=precip)) + 
    geom_boxplot()
  return(plt)
}
precip_centered <- scale(precip, center = TRUE, scale = FALSE)
# undebug(plot_cluster_boxplot)
plot_cluster_boxplot(precip_centered, kmeans_result)

test <- add_cluster(getValues(precip_centered), kmeans_result)
names(test)
d <- getValues(precip_centered)
cbind(d, kmeans_result$cluster)



