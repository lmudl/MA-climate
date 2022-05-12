# glyph plotting for sst data
# testing tidyverse
setwd("Repos/MA-climate/")
source("code/R/glyph-helpers.R")
#source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
library(GGally)
library(patchwork)
library(plyr)
library(mgcv)
library(maps)

sst <- brick("data/interim/sst/ersst_setreftime.nc", var="sst")

# vanilla plot ############
test <- raster::aggregate(sst, fact=2.5)
test <- raster::as.data.frame(test, xy = TRUE, long=TRUE)
test$layer <- as.numeric(stringr::str_replace(test$layer, "X", ""))
#test$
dim(test)
head(test)
test$layer <- as.integer(test$layer)

head(test)
colnames(test) <- c("long", "lat", "month", "sst")

head(test)

precip_gly <- glyphs(test, "long", "month", "lat", "precip")

p <- ggplot(precip_gly, ggplot2::aes(gx, gy, group = gid)) +
  add_ref_lines(precip_gly, color = "grey90") +
  add_ref_boxes(precip_gly, color = "grey90") +
  geom_path() +
  theme_bw() +
  labs(x = "", y = "")

saveRDS(p, "results/vanilla_glyph_plot_sst.rds")

# vanilla with world map ####
# and then maybe only the part from the ciemer paper#
world <- map_data("world")
p <- readRDS("results/vanilla_glyph_plot_sst.rds")
p + world

# use smaller map only
xlim <- c(-180,0)
ylim <- c(-50,40)
sub_p <- subset(p$data, (long > xlim[1]) & (long < xlim[2]) & (lat > ylim[1]) & (lat < ylim[2]))
head(p$data)
p2 <- ggplot(sub_p, aes(gx,gy, group=gid)) +
  geom_line(aes(group=gid)) +
  xlab("d") +
  ylab("da") 
p2

sub_w <- getbox(world, xlim = xlim, ylim = ylim)
map_sub_w <- list(
  geom_polygon(aes(long, lat, group = group), inherit.aes = FALSE, 
               data = sub_w, show.legend = FALSE, fill = "grey80", colour = "grey90"),
  scale_x_continuous(expand = c(0.02, 0)),
  scale_y_continuous(expand = c(0.02, 0)), 
  xlab(NULL),
  ylab(NULL))

sub_p <- glyphs(sub_p)
p3 <- ggplot(sub_p, aes(gx,gy,group=gid)) +
  map_sub_w +
  add_ref_boxes(sub_p) +
  geom_line(aes(group=gid)) +
  xlab("d") +
  ylab("e")
p3

# load and prepare sst #####
sst <- brick("data/interim/sst/ersst_setreftime.nc", var="sst")
# prepare data
sst <- raster::aggregate(sst, fact=2.5)
sst_df <- raster::as.data.frame(sst, xy = TRUE, long = TRUE)
names(sst_df) <- c("long", "lat", "layer", "sst")
sst_df <- prepare_df_glyph(sst_df)
sst_df <- na.omit(sst_df)

# plot seasonalities no world map ########
sst_seasonal_models <- plyr::dlply(dplyr::select(sst_df,long,lat,sst,year,cyc_month)
                             ,c("long", "lat"), function(df) {
                               lm(sst ~ year + factor(cyc_month), data = df)
                             })
sst_month_grid <- expand.grid(year=2017, cyc_month=1:12)

sst_month_preds <- ldply(sst_seasonal_models, function(mod) {
  sst_month_grid$pred <- predict(mod, newdata = sst_month_grid)
  sst_month_grid
})
sst_month_preds <- glyphs(sst_month_preds, "long", "cyc_month", "lat", "pred")

sst_seasonal_plot <- ggplot(sst_month_preds, aes(gx, gy, group=gid)) +
  add_ref_boxes(sst_month_preds, color = "grey90") +
  add_ref_lines(sst_month_preds, color = "grey90") +
  geom_path() + 
  theme_bw() +
  labs(x = "", y = "")

saveRDS(sst_seasonal_plot, "results/sst_seasonal_plot_no_countries.rds")

# plot seasonalities with world map ######
world <- map_data("world")

ggworld <- list(
  geom_polygon(aes(long, lat, group = group), inherit.aes = FALSE, 
               data = world, show.legend = FALSE, fill = "grey80", colour = "grey90"),
  scale_x_continuous(expand = c(0.02, 0)),
  scale_y_continuous(expand = c(0.02, 0)), 
  xlab(NULL),
  ylab(NULL))

sst_seasonal_plot_world <- ggplot(sst_month_preds, aes(gx, gy, group=gid)) +
  ggworld +
  add_ref_boxes(sst_month_preds, color = "grey90") +
  add_ref_lines(sst_month_preds, color = "grey90") +
  geom_path() + 
  theme_bw() +
  labs(x = "", y = "")

saveRDS(sst_seasonal_plot_world, "results/sst_seasonal_plot_world.rds")

# plot seasonalities with world map and with smaller window, see ciemer ######
xlim <- c(-180,0)
ylim <- c(-50,40)
sub_world <- getbox(world, xlim = xlim, ylim = ylim)
sub_ggworld <- list(
  geom_polygon(aes(long, lat, group = group), inherit.aes = FALSE, 
               data = sub_world, show.legend = FALSE, fill = "grey80", colour = "grey90"),
  scale_x_continuous(expand = c(0.02, 0)),
  scale_y_continuous(expand = c(0.02, 0)), 
  xlab(NULL),
  ylab(NULL))

sub_sst_month_preds <- subset(sst_month_preds, (long > xlim[1]) & (long < xlim[2]) & (lat > ylim[1]) & (lat < ylim[2]))

sst_seasonality_window_plot <- sub_sst_plot <- ggplot(sub_sst_month_preds, aes(gx, gy, group=gid)) +
  sub_ggworld +
  add_ref_boxes(sub_sst_month_preds, color = "grey90") +
  add_ref_lines(sub_sst_month_preds, color = "grey90") +
  geom_path() + 
  theme_bw() +
  labs(x = "", y = "")

saveRDS(sst_seasonality_window_plot, "results/sst_seasonality_window_plot.rds")

# plot seasonalitites with world and smaller window at higher resolution ####
sst <- brick("data/interim/sst/ersst_setreftime.nc", var="sst")
# note no aggregation!
e <- extent(-180,0,-50,40)
sst <- crop(sst,e)

sst_df <- raster::as.data.frame(sst, xy = TRUE, long = TRUE)
names(sst_df) <- c("long", "lat", "layer", "sst")
sst_df <- prepare_df_glyph(sst_df)
sst_df <- na.omit(sst_df)

sst_seasonal_models <- plyr::dlply(dplyr::select(sst_df,long,lat,sst,year,cyc_month)
                                   ,c("long", "lat"), function(df) {
                                     lm(sst ~ year + factor(cyc_month), data = df)
                                   })
sst_month_grid <- expand.grid(year=2017, cyc_month=1:12)

sst_month_preds <- ldply(sst_seasonal_models, function(mod) {
  sst_month_grid$pred <- predict(mod, newdata = sst_month_grid)
  sst_month_grid
})
sst_month_preds <- glyphs(sst_month_preds, "long", "cyc_month", "lat", "pred")

sst_seasonality_window_hr_plot <- ggplot(sst_month_preds, aes(gx, gy, group=gid)) +
  sub_ggworld +
  add_ref_boxes(sst_month_preds, color = "grey90") +
  add_ref_lines(sst_month_preds, color = "grey90") +
  geom_path() + 
  theme_bw() +
  labs(x = "", y = "")
saveRDS(sst_seasonality_window_hr_plot, "results/sst_seasonality_window_hr_plot.rds")



# plot deseasonalised smoothed data, with world map, small window scaled globally as done in paper wickham,#####
sst <- brick("data/interim/sst/ersst_setreftime.nc", var="sst")
# note no aggregation!
e <- extent(-180,0,-50,40)
sst <- crop(sst,e)

sst_df <- raster::as.data.frame(sst, xy = TRUE, long = TRUE)
names(sst_df) <- c("long", "lat", "layer", "sst")
sst_df <- prepare_df_glyph(sst_df)
sst_df <- na.omit(sst_df)

sst_smoothed_models <- dlply(dplyr::select(sst_df,long,lat,sst,month,cyc_month), 
                              c("long", "lat"), function(df) {
                                gam(sst ~ s(month) + factor(cyc_month), data = df)
                              })
saveRDS(sst_smoothed_models, "results/sst_smoothed_models.rds")

sst_pred_grid <- expand.grid(month = seq(972, 1403, length=50),
                             cyc_month = 1)

sst_smoothed_preds <- ldply(sst_smoothed_models, function(mod) {
  sst_pred_grid$pred <- predict(mod, newdata=sst_pred_grid)
  sst_pred_grid
})

sst_smoothed_preds <- glyphs(sst_smoothed_preds, "long", "month", "lat", "pred")

sst_smoothed_plot <- ggplot(sst_smoothed_preds, aes(gx, gy, group = gid)) + 
  sub_ggworld +
  add_ref_boxes(sst_smoothed_preds, color = "grey90") +
  add_ref_lines(sst_smoothed_preds, color = "grey90") +
  geom_path() + 
  theme_bw() +
  labs(x = "", y = "")
sst_smoothed_plot

# plot deseasonalised smoothed data, with world map, small window locally scaled as done in paper wickham ######

sst_smoothed_preds2 <- ddply(sst_smoothed_preds, c("long", "lat"), mutate,
                         pred_s = rescale01(pred),
                         pred_m = pred / max(pred),
                         max = max(pred),
                         range = diff(range(pred)))

sst_smoothed_preds2 <- glyphs(sst_smoothed_preds2, "long", "month", "lat", "pred_s")

grid <- unique(sst_smoothed_preds2[c("lat", "long", "range")])
range(grid$range)
max(grid$range)

sst_smoothed_scaled_colour_plot <- ggplot(sst_smoothed_preds2) +
  sub_ggworld +
  geom_tile(aes(long, lat, fill = range), data = grid, alpha = 0.5) + #alpha=0.5
  add_ref_boxes(sst_smoothed_preds2, color= "grey90") +
  geom_path(aes(gx, gy, group = gid)) +
  theme_bw() + 
  scale_fill_gradient("sst\nrange",
                      high = "white", low = "#3B4FB8", limits = range(grid$range),
                      breaks = seq(round(min(grid$range),2), round(max(grid$range),2), by = 1), guide = guide_colourbar(
                        direction = "horizontal", title.vjust = 0.7))

sst_smoothed_scaled_colour_plot
saveRDS(sst_smoothed_scaled_colour_plot, "results/sst_smoothed_scaled_colour_plot.rds")








# end ##############
# thoughts #####
# dimensions that play a role
# if data is aggregated or not
# how large the window is we choose to display
# so I could write a function like
# prepare data (with or without aggregation)
# and with or without cutting a smaller window out
# than there is the seasonality plots 
# and the scaled and unscaled residuals plots?


