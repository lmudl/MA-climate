# glyph plotting for sst data
# testing tidyverse
setwd("Repos/MA-climate/")
#source("code/R/helper-functions.R")
sst <- brick("data/interim/sst/ersst_setreftime.nc", var="sst")
library(ncdf4)
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

###############
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

p
saveRDS(p, "results/vanilla_glyph_plot_sst.rds")



