# testing tidyverse
setwd("Repos/MA-climate/")
#source("code/R/helper-functions.R")
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
library(raster)
library(ggplot2)
library(GGally)
library(patchwork)
library(plyr)
library(mgcv)

# helpers directly from the paper climate repo####
range01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

max1 <- function(x) {
  x / max(x, na.rm = TRUE)
}
mean0 <- function(x) {
  x - mean(x, na.rm = TRUE)
}
min0 <- function(x) {
  x - min(x, na.rm = TRUE)
}


rescale01 <- function(x, xlim=NULL) {
  if (is.null(xlim)) {
    rng <- range(x, na.rm = TRUE)
  } else {
    rng <- xlim
  }
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale11 <- function(x, xlim=NULL) 2 * rescale01(x, xlim) - 1

################
#colnames(values(precip)) <- 1:length(colnames(values(precip)))
??str_replace
test <- raster::as.data.frame(precip, xy = TRUE, long=TRUE)
head(test)
test$layer <- as.numeric(stringr::str_replace(test$layer, "X", ""))

head(test)
colnames(test) <- c("long", "lat", "month", "precip")

head(test)

precip_gly <- glyphs(test, "long", "month", "lat", "precip")

p <- ggplot(precip_gly, ggplot2::aes(gx, gy, group = gid)) +
  add_ref_lines(precip_gly, color = "grey90") +
  add_ref_boxes(precip_gly, color = "grey90") +
  geom_path() +
  theme_bw() +
  labs(x = "", y = "")

p

######### now plot seasonal and deseasonalised data #####
# des_precip <- readRDS("data/processed/deseasonalised_precip.rds")
# # turn into raster
# # load precip_og
# pog <- brick("data/interim/drought/chirps_setreftime.nc")
# # directly
# # create copy of og precip
# ref <- pog
# rm(pog)
# # give the copy the deseasonalised values so we have a raster obj
# head(getValues(ref))[1:5,1:4]
# ref <- setValues(ref, des_precip)
# head(getValues(ref))[1:5,1:4]
# aggregate the copy / change resolution
# ref_test <-  raster::aggregate(ref, fact = 10)
# saveRDS(ref_test, "data/processed/deasonalised_precip_aggregated.rds")
ref_test<- readRDS("data/processed/deasonalised_precip_aggregated.rds")
dim(ref_test@data@values)
test2 <- raster::as.data.frame(ref_test, xy = TRUE, long=TRUE)
test2$layer <- as.numeric(stringr::str_replace(test2$layer, "X", ""))

head(test2)
colnames(test2) <- c("long", "lat", "month", "precip")

head(test2)

precip_gly2 <- glyphs(test2, "long", "month", "lat", "precip")

p2 <- ggplot(precip_gly2, ggplot2::aes(gx, gy, group = gid)) +
  add_ref_lines(precip_gly2, color = "grey90") +
  add_ref_boxes(precip_gly2, color = "grey90") +
  geom_path() +
  #geom_smooth() +
  theme_bw() +
  labs(x = "", y = "")

p2

p + p2
?glyphs

# prepare data #####
precip_df <- raster::as.data.frame(precip, xy = TRUE, long=TRUE)
head(precip_df)
names(precip_df) <- c("long", "lat", "layer", "precip")

layer_to_month <- function(df_col) {
  month_col <- as.numeric(stringr::str_replace(df_col, "X", ""))
  return(month_col)
}
precip_df$month <- layer_to_month(precip_df$layer)
# 
month_to_year <- function(df_col) {
  return(df_col %/% 12 + 1900)
}
precip_df$year <- month_to_year(precip_df$month)

month_to_cyc <- function(df_col) {
  cyc_col <- (df_col+1) %% 12
  cyc_col[cyc_col==0] <- 12
  return(cyc_col)
}
precip_df$cyc_month <- month_to_cyc(precip_df$month)

# plot seasonalities ####
seasonal_models <- dlply(dplyr::select(precip_df,long,lat,precip,year,cyc_month)
                         ,c("long", "lat"), function(df) {
                           lm(precip ~ year + factor(cyc_month), data = df)
                         })
saveRDS(seasonal_models, "results/seasonal_models.rds")
seasonal_models <- readRDS("results/seasonal_models.rds")

month_grid <- expand.grid(year=2017, cyc_month=1:12)
month_preds <- ldply(seasonal_models, function(mod) {
  month_grid$pred <- predict(mod, newdata = month_grid)
  month_grid
})

month_preds <- glyphs(month_preds, "long", "cyc_month", "lat", "pred")

seasonal_plot <- ggplot(month_preds, aes(gx, gy, group = gid)) + 
  add_ref_boxes(month_preds, color = "grey90") +
  add_ref_lines(month_preds, color = "grey90") +
  geom_path() + 
  theme_bw() +
  labs(x = "", y = "")
seasonal_plot
saveRDS(seasonal_plot, "results/seasonal_plot.rds")
seasonal_plot <- readRDS("results/seasonal_plot.rds")
# seasonality seems to be the same everywhere

# check values
plot(hist(month_preds$pred))
plot(density(month_preds$pred))
# check month of max
a <- (unique(month_preds$gid))

res <- c()
res2 <- c()
for(i in a) {
  b <- dplyr::filter(month_preds, gid==i)
  ind <- which.max(b$pred)
  ind2 <- which.min(b$pred)
  res <- append(res, ind)
  res2 <- append(res2, ind2)
}

plot(table(res))
# res
# 1   2   3   4   5  12 
# 127   2 304  91  85   3
plot(table(res2))
# res2
# 1   6   7   8   9  10  11  12 
# 6   1 293 191  67  43   5   6 
# is quite different
# but could also be influenced by outliers
# check ranges

month_preds2 <- ddply(month_preds, c("long", "lat"), mutate,
                         range = diff(range(pred)))
plot(hist(month_preds2$range))


# deseasonalised data, smoothing as done in paper ####
smoothed_models <- dlply(dplyr::select(precip_df,long,lat,precip,month,cyc_month), 
                         c("long", "lat"), function(df) {
                           gam(precip ~ s(month) + factor(cyc_month), data = df)
                         })
saveRDS(smoothed_models, "results/smoothed_models.rds")
smoothed_models <- readRDS("results/smoothed_models.rds")
# deseasonalised because we fit a seasonal component, here
# factor(cyc_month) but in the predictions we use the same
# month for all predictions
pred_grid <- expand.grid(month = seq(972, 1403, length=50),
                         cyc_month = 1)

smoothed_preds <- ldply(smoothed_models, function(mod) {
  pred_grid$pred <- predict(mod, newdata=pred_grid)
  pred_grid
})

smoothed_preds <- glyphs(smoothed_preds, "long", "month", "lat", "pred")

smoothed_plot <- ggplot(smoothed_preds, aes(gx, gy, group = gid)) + 
  add_ref_boxes(smoothed_preds, color = "grey90") +
  add_ref_lines(smoothed_preds, color = "grey90") +
  geom_path() + 
  theme_bw() +
  labs(x = "", y = "")
smoothed_plot
saveRDS(smoothed_plot, "results/smoothed_plot.rds")
smoothed_plot <- readRDS("results/smoothed_plot.rds")

plot(hist(smoothed_preds$pred))

# deseasonalised with scaling ####

# locally rescaled
smoothed_scaled_plot <- smoothed_plot %+% glyphs(smoothed_preds, "long", "month", "lat", "pred",
                                                 y_scale=range01)
saveRDS(smoothed_scaled_plot, "results/smoothed_scaled_plot.rds")
readRDS
# local scaling emphasises the individual shapes,
# scale is not relative, big patterns in some locations might be 
# just tiny effects.

smoothed_preds2 <- ddply(smoothed_preds, c("long", "lat"), mutate,
                         pred_s = rescale01(pred),
                         pred_m = pred / max(pred),
                         max = max(pred),
                         range = diff(range(pred)))

smoothed_preds2 <- glyphs(smoothed_preds2, "long", "month", "lat", "pred_s")

## take a look at ranges
plot(hist(smoothed_preds2$range))


## local scale line thickness
smoothed_scaled_line_plot <- ggplot(smoothed_preds2, aes(gx, gy, group = gid)) + 
  add_ref_boxes(smoothed_preds2) +
  geom_path(aes(colour = range)) +
  theme_bw() +
  scale_colour_gradient("Precipitation\nrange",
                        high = "black", low = "grey60", limits = c(0, 130),
                        breaks = seq(0, 130, by = 30), guide = guide_colourbar(
                          direction = "horizontal", title.vjust = 0.7 
                        ))
saveRDS(smoothed_scaled_line_plot, "results/smoothed_scaled_line_plot.rds")
smoothed_scaled_line_plot <- readRDS("results/smoothed_scaled_line_plot.rds")
## local scale colour

grid <- unique(smoothed_preds2[c("lat", "long", "range")])
range(grid$range)

smoothed_scaled_colour_plot <- ggplot(smoothed_preds2) + 
  geom_tile(aes(long, lat, fill = range), data = grid, alpha = 0.5) + #alpha=0.5
  geom_path(aes(gx, gy, group = gid)) +
  theme_bw() + 
  scale_fill_gradient("Precipitation\nrange",
                      high = "white", low = "#3B4FB8", limits = c(0, 130),
                      breaks = seq(0, 130, by = 30), guide = guide_colourbar(
                        direction = "horizontal", title.vjust = 0.7))
saveRDS(smoothed_scaled_colour_plot, "results/smoothed_scaled_colour_plot.rds")
smoothed_scaled_colour_plot <- readRDS("results/smoothed_scaled_colour_plot.rds")


