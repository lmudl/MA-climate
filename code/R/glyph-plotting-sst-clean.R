# have to write more  glpyh helpers
# glyph plotting for sst data
# testing tidyverse
setwd("Repos/MA-climate/")
source("code/R/glyph-helpers.R")
source("code/R/glyph-helpers-2.R")
#source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
library(GGally)
library(patchwork)
library(plyr)
library(mgcv)
library(maps)
# load data and prepare sst

sst <- brick("data/interim/sst/ersst_setreftime.nc", var="sst")
# prepare data
sst <- raster::aggregate(sst, fact=2.5)
sst_df <- raster::as.data.frame(sst, xy = TRUE, long = TRUE)
names(sst_df) <- c("long", "lat", "layer", "sst")
sst_df <- prepare_df_glyph(sst_df)
sst_df <- na.omit(sst_df)

# plot seasonalities
sst_seasonal_models_world <- fit_seasonal_model(sst_df)
sst_month_grid <- create_month_grid(sst_df)
sst_month_preds_world <- get_preds(sst_seasonal_models_world, sst_month_grid)
sst_month_preds_world <- glyphs(sst_month_preds_world, "long", "cyc_month", "lat", "pred")
sst_seasonal_plot_world <- gg_glyph(sst_month_preds_world, worldmap=TRUE)

saveRDS(sst_seasonal_plot_world, "results/glyph/sst_seasonal_plot_world.rds")

# fit smoothed models
sst_smoothed_models_world <- fit_smoothed_models(sst_df)

saveRDS(sst_smoothed_models_world, "results/glyph/sst_smoothed_models_world.rds")
#sst_smoothed_models_world <- readRDS("results/glyph/sst_smoothed_models_world.rds")
# plot smoothed/ deseasonalise models
sst_smooth_pred_grid <- create_smooth_grid(sst_df)
sst_smoothed_preds_world <- get_preds(sst_smoothed_models_world, sst_smooth_pred_grid) 
sst_smoothed_preds_world <- glyphs(sst_smoothed_preds_world, "long", "month", "lat", "pred")
sst_smoothed_plot_world <- gg_glyph(sst_smoothed_preds_world, worldmap = TRUE)

saveRDS(sst_smoothed_plot_world, "results/glyph/sst_smoothed_plot_world.rds")

# plot deseasonalised locally scaled
sst_smoothed_preds_world2 <- scale_preds_globally(sst_smoothed_preds_world)
sst_smoothed_preds_world2 <-  glyphs(sst_smoothed_preds_world2, "long", "month", "lat", "pred_s")
grid <- unique(sst_smoothed_preds_world2[c("lat", "long", "range")])
sst_smoothed_scaled_colour_plot_world <- gg_glyph_scale(sst_smoothed_preds_world2, grid)

saveRDS(sst_smoothed_scaled_colour_plot_world, "results/glyph/sst_smoothed_scaled_colour_plot_world.rds")

# for smaller window ####
xlim <- c(-180,0)
ylim <- c(-50,40)
world <- getbox(world, xlim = xlim, ylim = ylim)
ggworld <- list(
  geom_polygon(aes(long, lat, group = group), inherit.aes = FALSE, 
               data = world, show.legend = FALSE, fill = "grey80", colour = "grey90"),
  scale_x_continuous(expand = c(0.02, 0)),
  scale_y_continuous(expand = c(0.02, 0)), 
  xlab(NULL),
  ylab(NULL))

sub_sst_month_preds <- subset(sst_month_preds_world, (long > xlim[1]) & (long < xlim[2]) & (lat > ylim[1]) & (lat < ylim[2]))
sst_seasonality_window_plot <- gg_glyph(sub_sst_month_preds)
saveRDS(sst_seasonality_window_plot, "results/glyph/sst_seasonal_plot_window.rds")

# subset model predictions
sst_smoothed_preds_window <- subset(sst_smoothed_preds_world, (long > xlim[1]) & (long < xlim[2]) & (lat > ylim[1]) & (lat < ylim[2]))
sst_smoothed_plot_window <- gg_glyph(sst_smoothed_preds_window)
saveRDS(sst_smoothed_plot_window, "results/glyph/sst_smoothed_plot_window.rds")

sst_smoothed_preds_window2 <- subset(sst_smoothed_preds_world2, (long > xlim[1]) & (long < xlim[2]) & (lat > ylim[1]) & (lat < ylim[2]))
sub_grid <- subset(grid, (long > xlim[1]) & (long < xlim[2]) & (lat > ylim[1]) & (lat < ylim[2]) )
sst_smoothed_scaled_colour_plot_window <- gg_glyph_scale(sst_smoothed_preds_window2, sub_grid)
saveRDS(sst_smoothed_scaled_colour_plot_window, "results/glyph/sst_smoothed_scaled_colour_plot_window.rds")
