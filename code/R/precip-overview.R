# TODO sort plots, means, sd and densities

setwd("Repos/MA-climate/")

source("code/R/helper-functions.R")
library(raster)
library(ggplot2)

#####
# helper function

plot_summary <- function(data, summary) {
  df <- base::as.data.frame(cbind(coordinates(data), summary@data@values))
  colnames(df) <- c("Longitude", "Latitude", "val")
  plt <- ggplot(data = df, aes(x = Longitude, y = Latitude, fill = val)) +
    geom_raster(interpolate = FALSE) +
    scale_fill_gradient2(low = "red", high = "blue", midpoint = mean(df$val))
  plt
}

plot_trends <- function(data, trends) {
  df <- base::as.data.frame(cbind(coordinates(data), trends))
  colnames(df) <- c("Longitude","Latitude", "val")
  plt <- ggplot(data = df, aes(x = Longitude, y = Latitude, fill = val)) +
    geom_raster(interpolate = FALSE) +
    scale_fill_gradient2(low = "red", high = "blue", midpoint = mean(df$val))
  plt
}

get_trend <- function(vec) {
  b <- seq(length(vec))
  trend <- lm(vec ~ b)$coefficients[2]
  return(trend)
}
#####
# load data
precip_og <- brick("data/interim/drought/chirps_setreftime.nc")
#precip <- raster::aggregate(precip_og, fact = 10) # aggregate, both dim factor 10 res less
#saveRDS(precip, "data/interim/drought/chirps_setreftime_aggregated.rds")
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
dim(precip) # 18, 34, 432
m <- brick_to_matrix_wna(precip, nlayers = nlayers(precip)) # like this gives matrix, long x lat

#####
# inspect the time series of precipitation
# plot overall mean?
# plot trend after using stl?
# plot size of amplitudes in seasonal component?
r_mean <- calc(precip, mean)
plot(r_mean)
plot_summary(r_mean)
df_mean <- data.frame(means = values(r_mean))
ggplot(df_mean, aes(x = means)) + geom_density()

r_sd <- calc(precip, sd)
plot(r_sd)
plot_summary(r_sd)
df_sd <- data.frame(sd = values(r_sd))
ggplot(df_sd, aes(x = sd)) + geom_density()

#####
# trend for each time series?
# animate to show yearly means?

trends <- apply(m, 1, get_trend)
plot_trends(precip, trends)
df_trends <- data.frame(tr = trends)
ggplot(df_trends, aes(x = tr)) + geom_density()

# trends for deasonalised ts
des_prec <- readRDS("data/processed/deseasonalised_precip.rds")
dim(des_prec)
des_trends <- apply(des_prec,1,get_trend)
df_des_trends <- data.frame(tr = des_trends)
plot_trends(precip_og, des_trends)
# does not seem to make such a difference

# make plots with ggally
# load oackages load precip_og
# for spatiotemporal data, the major axes are latitude
# y_major and longitude x_major. Minor axes are time and
# some measurement x_minor, y_minor respectively
library(GGally)
data(nasa)
dim(nasa)
str(nasa)
# nasa has time, 

temp.gly <- glyphs(nasa, "long", "day", "lat", "surftemp", height = 2.5)
?glyphs
rm(temp.gly)
# create data frame for ggplot
# write own function
# or use tidyverse


precip_df <- data.frame("long" = coordinates(precip)[,1],
                        "month" = 1:ncol(values(precip)),
                        "lat" = coordinates(precip)[,2],
                        "precip" = c(values(precip)))

test <- raster::as.data.frame(precip, xy = TRUE, long=TRUE)

library("tidyverse")
test <- precip %>% raster::as.data.frame(, xy=TRUE) %>%
  rename(lon = x, lat = y) %>%
  tidyr::gather(Layer, value, -lon, -lat) %>%
  dplyr::left_join(dt, by = "Layer") %>%
  dplyr::select(lon, lat, dttm, value)

str(test)
# what about turning va
precip_og
str(test)
precip_gly <- glyphs(test, "long", "month", "lat", "precip",
                     height = 2.5)

p <- ggplot(precip_gly, ggplot2::aes(gx, gy, group = gid)) +
  add_ref_lines(precip_gly, color = "grey90") +
  add_ref_boxes(precip_gly, color = "grey90") +
  geom_path() +
  theme_bw() +
  labs(x = "", y = "")

p
str(precip_df)
prec.gly <- glyphs(precip_df, )
test <- as.data.frame(as(precip, "SpatialPixelsDataFrame"))
colnames(test) <- c("value", "x", "y")
names(test)
ggplot(data=test, aes(x=x,y=y, fill=value))
raster::as

