# cluster kmeans

# cluster precipitation data
# load precip
setwd("Repos/MA-climate/")

source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
library(cluster)

# load data #####
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")

# kmeans vanilla
set.seed(1234)
gap_kmeans <- clusGap(getValues(precip), FUN = kmeans, B = 50, K.max = 20, nstart = 10,
                         iter.max = 30)
gap_kmeans_plot <- factoextra::fviz_gap_stat(gap_kmeans) + ggtitle("Gap statistic kmeans, uncentered")
saveRDS(gap_kmeans_plot, "results/clustering/gap_kmeans_plot.rds")

# kmeans on centered data
centered_precip <- scale(getValues(precip), center = TRUE, scale = FALSE)
set.seed(1234)
gap_kmeans_centered <- clusGap(centered_precip, FUN = kmeans, B = 50, K.max = 20, nstart = 10,
                              iter.max = 30)
gap_kmeans_centered_plot <- factoextra::fviz_gap_stat(gap_kmeans_centered) + ggtitle("Gap statistic kmeans, centered")
saveRDS(gap_kmeans_centered_plot, "results/clustering/gap_kmeans_centered_plot.rds")
# centered_kmean_gap_b50 <- readRDS("results/centered_kmean_gap_b50.rds")

