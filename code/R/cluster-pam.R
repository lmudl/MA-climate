# cluster pam

# cluster precipitation data
# load precip
setwd("Repos/MA-climate/")

source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
library(cluster)

# load data #####
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")


# if we only want center we need to center data before and then do pam
set.seed(1234)
centered_precip <- scale(getValues(precip), center = TRUE, scale = FALSE)
gap_pam_centered <- clusGap(centered_precip, FUN = pam, K.max = 20,
                              B = 50, nstart = 10, keep.diss = FALSE,
                              keep.data = FALSE, do.swap = TRUE,
                              stand = FALSE)
gap_pam_centered_plot <- factoextra::fviz_gap_stat(gap_pam_centered) + 
  ggtitle("Gap statistic, pam centered")
saveRDS(gap_pam_centered_plot, "results/clustering/gap_pam_centered_plot.rds")

set.seed(1234)
gap_pam <- clusGap(getValues(precip), FUN = pam, K.max = 20,
                            B = 50, nstart = 10, keep.diss = FALSE,
                            keep.data = FALSE, do.swap = TRUE,
                            stand = FALSE)
gap_pam_plot <- factoextra::fviz_gap_stat(gap_pam) + ggtitle("Gap statistic, pam uncentered")
saveRDS(gap_pam_plot, "results/clustering/gap_pam_plot.rds")

