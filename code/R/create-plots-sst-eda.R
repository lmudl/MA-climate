# create plots for SST EDA
getwd()
setwd("Repos/MA-climate/")
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(patchwork)
library(ggpubr)
source("code/R/helper-functions.R")

# density plot ####
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst_dens_plot <- plot_dens_gg(sst)
saveRDS(sst_dens_plot, "results/eda/sst_dens_plot.rds")

# mean on raster plot ####
sst_means <- calc(sst, mean)
sst_means_loc_plot <- plot_summary(sst_means) + 
  ggtitle("Mean of sst at each location")
saveRDS(sst_means_loc_plot, "results/eda/sst_means_loc_plot.rds")

# density of means ####
sst_means_dens_plot <- plot_dens_gg(sst_means) + 
  ggtitle("Density of means computed on each location")
saveRDS(sst_means_dens_plot, "results/eda/sst_means_dens_plot.rds")

# sd on raster plot ####
sst_sds <- calc(sst, sd)
sst_sds_loc_plot <- plot_summary(sst_sds)
saveRDS(sst_sds_loc_plot, "results/eda/sst_sds_loc_plot.rds")

# density of sds ####
sst_sds_dens_plot <- plot_dens_gg(sst_sds) + 
  ggtitle("Density of SDs computed on each location")
saveRDS(sst_sds_dens_plot, "results/eda/sst_sds_dens_plot.rds")

# trend on raster plot ####
# NOTE: we take trends from deseasonalised data
sst_des <- readRDS("data/processed/rtsa_deseasonalised_sst.rds")
sst_trends_loc_stl_plot <- plot_summary(sst_des@trend_slope)
saveRDS(sst_trends_loc_stl_plot, "results/eda/sst_trends_loc_stl_plot.rds")
# OR
sst_trends <- apply(getValues(sst), 1, get_trend)
sst_trends_loc_lm_plot <- plot_trends(sst, sst_trends)
saveRDS(sst_trends_loc_lm_plot, "results/eda/sst_trends_loc_lm_plot.rds")
# compare and find that they are on a factor different

# plot list of monthly mean and sd ####
sst_plot_list <- mon_plots(sst)

sst_mon_mean <- sst_plot_list[[1]]
sst_mean_arrange <- ggarrange(plotlist = sst_mon_mean, ncol = 3, nrow = 4,
                                 common.legend = TRUE, legend = "bottom")
saveRDS(sst_mean_arrange, "results/eda/sst_mean_arrange.rds")

sst_mon_sd <- sst_plot_list[[2]]
sst_sd_arrange <- ggarrange(plotlist = sst_mon_sd, ncol = 3, nrow = 4,
                               common.legend = TRUE, legend = "bottom")
saveRDS(sst_sd_arrange, "results/eda/sst_sd_arrange.rds")

sst_mon_trend <- sst_plot_list[[3]]
sst_trend_arrange <- ggarrange(plotlist = sst_mon_trend, ncol = 3, nrow = 4,
                                  common.legend = TRUE, legend = "bottom")
saveRDS(sst_trend_arrange, "results/eda/sst_trend_arrange.rds")

