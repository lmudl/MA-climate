# create plots for precip EDA
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

world <- ne_countries(scale = "medium", returnclass = "sf")
theme_set(theme_bw())
cab_square_plot <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-100,-20), ylim = c(40,-40), expand = TRUE) +   
  geom_rect(xmin = -70, xmax = -55, ymin = -10, ymax = 0,
            fill = NA, colour = "black", size = 1.5)
saveRDS(cab_square_plot, "results/eda/cab_square_plot.rds")

# density plot ####
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
precip_dens_plot <- plot_dens_gg(precip)
saveRDS(precip_dens_plot, "results/eda/precip_dens_plot.rds")

# mean on raster plot ####
precip_means <- calc(precip, mean)
attr(precip_means, "var") <- "precip"
precip_means_loc_plot <- plot_summary(precip_means) + 
ggtitle("Mean of precipitation at each location")
saveRDS(precip_means_loc_plot, "results/eda/precip_means_loc_plot.rds")

# density of means ####
precip_means_dens_plot <- plot_dens_gg(precip_means) + 
ggtitle("Density of means computed on each location")
saveRDS(precip_means_dens_plot, "results/eda/precip_means_dens_plot.rds")

# sd on raster plot ####
precip_sds <- calc(precip, sd)
attr(precip_sds, "var") <- "precip"
precip_sds_loc_plot <- plot_summary(precip_sds)
saveRDS(precip_sds_loc_plot, "results/eda/precip_sds_loc_plot.rds")

# density of sds ####
precip_sds_dens_plot <- plot_dens_gg(precip_sds) + 
ggtitle("Density of SDs computed on each location")
saveRDS(precip_sds_dens_plot, "results/eda/precip_sds_dens_plot.rds")

# trend on raster plot ####
# NOTE: we take trends from deseasonalised data
precip_des <- readRDS("data/processed/rtsa_deseasonalised_precip.rds")
precip_des@trend_slope <- add_var_as_attr(precip_des@trend_slope, "precip")
precip_trends_loc_stl_plot <- plot_summary(precip_des@trend_slope)
saveRDS(precip_trends_loc_stl_plot, "results/eda/precip_trends_loc_stl_plot.rds")
# OR
precip_trends <- apply(getValues(precip), 1, get_trend)
precip_trends_loc_lm_plot <- plot_trends(precip, precip_trends, var="precip")
saveRDS(precip_trends_loc_lm_plot, "results/eda/precip_trends_loc_lm_plot.rds")
# compare and find that they are on a factor different

# plot list of monthly mean and sd ####
precip_plot_list <- mon_plots(precip, "precip")

precip_mon_mean <- precip_plot_list[[1]]
precip_mean_arrange <- ggarrange(plotlist = precip_mon_mean, ncol = 3, nrow = 4,
                          common.legend = TRUE, legend = "bottom")
saveRDS(precip_mean_arrange, "results/eda/precip_mean_arrange.rds")

precip_mon_sd <- precip_plot_list[[2]]
precip_sd_arrange <- ggarrange(plotlist = precip_mon_sd, ncol = 3, nrow = 4,
                                 common.legend = TRUE, legend = "bottom")
saveRDS(precip_sd_arrange, "results/eda/precip_sd_arrange.rds")

precip_mon_trend <- precip_plot_list[[3]]
precip_trend_arrange <- ggarrange(plotlist = precip_mon_trend, ncol = 3, nrow = 4,
                               common.legend = TRUE, legend = "bottom")
saveRDS(precip_trend_arrange, "results/eda/precip_trend_arrange.rds")

