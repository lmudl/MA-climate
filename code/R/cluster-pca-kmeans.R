# cluster pca unscaled
# cluster precipitation data
# load precip
setwd("Repos/MA-climate/")

source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
library(cluster)

# load data #####
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")


# pca centered, unscaled and screeplot ####
pca_centered <- prcomp(getValues(precip), retx = TRUE,
              center = TRUE, scale. = FALSE)

var_exp_centered <- pca_centered$sdev^2 / sum(pca_centered$sdev^2)
var_exp_centered_plot <- var_exp_centered[1:30]
scree_plot_pca_centered <- qplot(c(1:30), var_exp_centered_plot) + 
  geom_line() + 
  xlab(" Principal Component") + 
  ylab(" Variance Explained") +
  ggtitle(" Scree Plot") +
  ylim(0, 1)

saveRDS(scree_plot_pca_centered, "results/scree_plot_pca_centered.rds")

# kmeans after pca, 2,3,4,5 components ####

# set.seed(1234)
# gap_pc2_centered_kmeans <- clusGap(pca_centered$x[,1:2], FUN=kmeans,B=100, K.max = 20, nstart = 20,
#                              iter.max = 30)
# gap_pc2_centered_kmeans_plot <- factoextra::fviz_gap_stat(gap_pc2_centered_kmeans) + ggtitle("PC 2, centered, Kmeans")
# saveRDS(gap_pc2_centered_kmeans_plot, "results/clustering/gap_pc2_centered_kmeans_plot.rds")


# 3 ####
set.seed(1234)
gap_pc3_centered_kmeans <- clusGap(pca_centered$x[,1:3], FUN=kmeans,B=100, K.max = 20, nstart = 20,
                  iter.max = 30)
gap_pc3_centered_kmeans_plot <- factoextra::fviz_gap_stat(gap_pc3_centered_kmeans) + ggtitle("PC 3, centered, Kmeans")
saveRDS(gap_pc3_centered_kmeans_plot, "results/clustering/gap_pc3_centered_kmeans_plot.rds")

set.seed(1234)
gap_pc3_centered_kmeans_og <- clusGap(pca_centered$x[,1:3], FUN=kmeans,B=100, K.max = 20, nstart = 20,
                              iter.max = 30, spaceH0 = "original")
gap_pc3_centered_kmeans_og_plot <- factoextra::fviz_gap_stat(gap_pc3_centered_kmeans_og) + ggtitle("PC 3, centered, spaceH0:original,Kmeans")
saveRDS(gap_pc3_centered_kmeans_og_plot, "results/clustering/gap_pc3_centered_kmeans_og_plot.rds")


# 4 ####
set.seed(1234)
gap_pc4_centered_kmeans <- clusGap(pca_centered$x[,1:4], FUN=kmeans,B=100, K.max = 20, nstart = 20,
                                   iter.max = 30)
gap_pc4_centered_kmeans_plot <- factoextra::fviz_gap_stat(gap_pc4_centered_kmeans) + ggtitle("PC 4, centered, Kmeans")
saveRDS(gap_pc4_centered_kmeans_plot, "results/clustering/gap_pc4_centered_kmeans_plot.rds")

set.seed(1234)
gap_pc4_centered_kmeans_og <- clusGap(pca_centered$x[,1:4], FUN=kmeans,B=100, K.max = 20, nstart = 20,
                                      iter.max = 30, spaceH0 = "original")
gap_pc4_centered_kmeans_og_plot <- factoextra::fviz_gap_stat(gap_pc4_centered_kmeans_og) + ggtitle("PC 4, centered, spaceH0:original,Kmeans")
saveRDS(gap_pc4_centered_kmeans_og_plot, "results/clustering/gap_pc4_centered_kmeans_og_plot.rds")


# 5 ####
# clus_5_centered <- clusGap(pca_centered$x[,1:5], FUN=kmeans,B=100, K.max = 20, nstart = 20,
#                   iter.max = 30)
# pc5_gap_centered_plot <- factoextra::fviz_gap_stat(clus_4_centered)
# saveRDS(pc5_gap_centered_plot, "results/clustering/pc5_centered_gap_plot.rds")
# 


# PRIO LOW
# 2 ####
# this one was done without centering
pca_uncentered <- prcomp(getValues(precip), retx = TRUE,
                         center = FALSE, scale. = FALSE)

var_exp_uncentered <- pca_uncentered$sdev^2 / sum(pca_uncentered$sdev^2)
var_exp_uncentered_plot <- var_exp_uncentered[1:30]
scree_plot_pca_uncentered <- qplot(c(1:30), var_exp_uncentered_plot) + 
  geom_line() + 
  xlab(" Principal Component") + 
  ylab(" Variance Explained") +
  ggtitle(" Scree Plot") +
  ylim(0, 1)

saveRDS(scree_plot_pca_uncentered,"results/scree_plot_pca_uncentered.rds")

# set.seed(1234)
# clus_1_uncentered <- clusGap(as.matrix(pca_uncentered$x[,1]), FUN=kmeans,B=100, K.max = 20, nstart = 20,
#                              iter.max = 30)
# pc1_gap_uncentered_plot <- factoextra::fviz_gap_stat(clus_1_uncentered) + ggtitle("Gap statistic, PC1 uncentered, kmeans ")
# saveRDS(pc2_gap_uncentered_plot, "results/clustering/gap_pc1_uncentered_kmeans_plot.rds")


set.seed(1234)
clus_2_uncentered <- clusGap(pca_uncentered$x[,1:2], FUN=kmeans,B=100, K.max = 20, nstart = 20,
                  iter.max = 30)
pc2_gap_uncentered_plot <- factoextra::fviz_gap_stat(clus_2_uncentered) + ggtitle("Gap statistic, PC2 uncentered, kmeans ")
saveRDS(pc2_gap_uncentered_plot, "results/clustering/gap_pc2_uncentered_kmeans_plot.rds")

#kmeans_uncentered_result <- kmeans(pca_uncentered$x[,1:2], centers = 3)
#plot_kmeans(precip, kmeans_uncentered_result)

set.seed(1234)
clus_3_uncentered <- clusGap(pca_uncentered$x[,1:3], FUN=kmeans,B=100, K.max = 20, nstart = 20,
                             iter.max = 30)
pc3_gap_uncentered_plot <- factoextra::fviz_gap_stat(clus_3_uncentered) + ggtitle("Gap statistic, PC3 uncentered, kmeans ")
saveRDS(pc3_gap_uncentered_plot, "results/clustering/gap_pc3_uncentered_kmeans_plot.rds")


# plot
kmeans_res <- kmeans(pca_uncentered$x[,1:2], center = 3)
plot_kmeans(precip, kmeans_res)

