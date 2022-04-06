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
scree_plot_pca_centered

# pam after pca, 3,4,5 components ####

# 2 ####
# set.seed(1234)
# pam_pca_centered_2 <- clusGap(pca_centered$x[,1:2], FUN = pam, K.max = 20,
#                               B = 50, nstart = 10, keep.diss = FALSE,
#                               keep.data = FALSE, do.swap = TRUE,
#                               stand = FALSE)
# pam_pca_centered_2_plot <- factoextra::fviz_gap_stat(pam_pca_centered_2)
# saveRDS(pam_pca_centered_2_plot, "results/clustering/pam_pca_centered_2_plot.rds")

# 3 ####
set.seed(1234)
gap_pc3_centered_pam <- clusGap(pca_centered$x[,1:3], FUN = pam, K.max = 20,
                     B = 50, nstart = 10, keep.diss = FALSE,
                     keep.data = FALSE, do.swap = TRUE,
                     stand = FALSE)
gap_pc3_centered_pam_plot <- factoextra::fviz_gap_stat(gap_pc3_centered_pam) + ggtitle("3 PC, centered, PAM")
saveRDS(gap_pc3_centered_pam_plot, "results/clustering/gap_pc3_centered_pam_plot.rds")

# 4 ####
# chooses 1 but 3 seems to be very reasonable as well
set.seed(1234)
gap_pc4_centered_pam <- clusGap(pca_centered$x[,1:4], FUN = pam, K.max = 20,
                     B = 50, nstart = 10, keep.diss = FALSE,
                     keep.data = FALSE, do.swap = TRUE,
                     stand = FALSE)
(gap_pc4_centered_pam_plot <- factoextra::fviz_gap_stat(gap_pc4_centered_pam)) + ggtitle("4 PC, centered, PAM")
saveRDS(gap_pc4_centered_pam_plot, "results/clustering/gap_pc4_centered_pam_plot.rds")

# # change name
# p <- readRDS("results/clustering/gap_pc4_centered_pam_plot.rds")
# p <- p + ggtitle("PC 4, centered, PAM")
# saveRDS(p, "results/clustering/gap_pc4_centered_pam_plot.rds")
# gap_pc4_centered_pam_plot <- readRDS("results/clustering/gap_pc4_centered_pam_plot.rds")


# # 5 ####
# set.seed(1234)
# pam_pca_centered_5 <-  clusGap(pca_centered$x[,1:5], FUN = pam, K.max = 20,
#                       B = 50, nstart = 10, keep.diss = FALSE,
#                       keep.data = FALSE, do.swap = TRUE,
#                       stand = FALSE)
# (pam_pca_centered_5_plot <- factoextra::fviz_gap_stat(pam_pca_centered_5))
# saveRDS(pam_pca_centered_5_plot, "results/clustering/pam_pca_centered_5_plot.rds")


# PRIO LOW
# uncentered ####
# this is done without centering
pca_uncentered <- prcomp(getValues(precip), retx = TRUE,
                       center = FALSE, scale. = FALSE)
plot(pca_uncentered)

var_exp_uncentered <- pca_uncentered$sdev^2 / sum(pca_uncentered$sdev^2)
var_exp_uncentered_plot <- var_exp_uncentered[1:30]
scree_plot_pca_uncentered <- qplot(c(1:30), var_exp_uncentered_plot) + 
  geom_line() + 
  xlab(" Principal Component") + 
  ylab(" Variance Explained") +
  ggtitle(" Scree Plot") +
  ylim(0, 1)

scree_plot_pca_uncentered


# 2 ####
set.seed(1234)
# gap_pc2_uncentered_pam
pam_pca_uncentered_2 <- clusGap(pca_uncentered$x[,1:2], FUN = pam, K.max = 20,
                     B = 50, nstart = 10, keep.diss = FALSE,
                     keep.data = FALSE, do.swap = TRUE,
                     stand = FALSE) # stand should be FALSE as well
#gap_pc2_uncentered_pam_plt 
pam_pca_uncentered_2_plot <- factoextra::fviz_gap_stat(gap_pc2_uncentered_pam) + ggtitle("Gap statistic, PC 2 uncentered, pam")
saveRDS(pam_pca_uncentered_2_plot, "results/clustering/pam_pca_uncentered_2_plot.rds")
# renaming plot/rds
gap_pc2_uncentered_pam_plot <- readRDS("results/clustering/pam_pca_uncentered_2_plot.rds")
saveRDS(gap_pc2_uncentered_pam_plot, "results/clustering/gap_pc2_uncentered_pam_plot.rds")


# 3 ####
set.seed(1234)
pam_pca_uncentered_3 <- clusGap(pca_uncentered$x[,1:3], FUN = pam, K.max = 20,
                                B = 50, nstart = 10, keep.diss = FALSE,
                                keep.data = FALSE, do.swap = TRUE,
                                stand = FALSE) # stand should be FALSE as well
pam_pca_uncentered_3_plot <- factoextra::fviz_gap_stat(pam_pca_uncentered_3) + ggtitle("Gap statistic, PC 3 uncentered, pam")
saveRDS(pam_pca_uncentered_3_plot, "results/clustering/pam_pca_uncentered_3_plot.rds")
# renaming plot/rds
gap_pc3_uncentered_pam_plot <- readRDS("results/clustering/pam_pca_uncentered_3_plot.rds")
saveRDS(gap_pc3_uncentered_pam_plot, "results/clustering/gap_pc3_uncentered_pam_plot.rds")

