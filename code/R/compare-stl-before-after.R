getwd()
setwd("Repos/MA-climate/")
library(ncdf4)
library(raster)
library(dplyr)
library(ggplot2)
source("code/R/helper-functions.R")
set.seed(1234)
# inspect random cell plot original data and des
  # sst, og and des
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
# cell_sst <- get_random_cell(sst)
r  <- raster(sst, layer = 1)
l <- length(r)
vals <- getValues(r)
not_na <- which(!is.na(vals))
rnd_cell_sst <- sample(not_na, 1)
coord_sst <- xyFromCell(r, rnd_cell_sst)

plot_cell(coord_sst)

ts_sst <- get_ts(sst, rnd_cell_sst)
plot_sst_og <- plot_stl(ts_sst) +
  ggtitle("STL for random sst cell, original data")

des_sst <- readRDS("data/processed/deseasonalised_sst.rds")
ts_des_sst <- des_sst[rnd_cell_sst,]
plot_sst_des <- plot_stl(ts_des_sst) +
  ggtitle("STL for random sst cell, deseasonalised data")


  # precip, og and des
precip <- brick("data/interim/drought/chirps_setreftime.nc")
r  <- raster(precip, layer = 1)
l <- length(r)
vals <- getValues(r)
not_na <- which(!is.na(vals))
rnd_cell_precip <- sample(not_na, 1)
coord_precip <- xyFromCell(r, rnd_cell_precip)

ts_precip <- get_ts(precip, rnd_cell_precip)
plot_precip_og <- plot_stl(ts_precip) +
  ggtitle("STL for random precip cell, original data")

des_precip <- readRDS("data/processed/deseasonalised_precip.rds")
ts_des_precip <- des_precip[rnd_cell_precip,]
plot_precip_des <- plot_stl(ts_des_precip) +
  ggtitle("STL for random precip cell, deseasonalised data")


######################################################
# inspect sst mean og and des
# og
sst <- brick_to_matrix_wna(sst, nlayers = nlayers(sst))
mean_sst <- apply(sst, 2, function(x) mean(x, na.rm=TRUE))
dim(sst)
length(mean_sst)

plot_mean_sst <- plot_stl(mean_sst) +
  ggtitle("STL for mean global sst, original data")

# des
mean_sst_des <- apply(des_sst, 2, function(x) mean(x, na.rm=TRUE))
dim(des_sst)
length(mean_sst_des)
plot_mean_sst_des <- plot_stl(mean_sst_des) +
  ggtitle("STL for mean global sst, deseasonalised data")


# inspect precup mean og and des
precip <- brick_to_matrix_wna(precip, nlayers = nlayers(precip))
mean_precip <- apply(precip, 2, mean)

plot_mean_precip <- plot_stl(mean_precip) +
  ggtitle("STL for mean precip, original data")


mean_precip_des <- apply(des_precip, 2, mean)
dim(des_precip)
length(mean_precip_des)
plot_mean_precip_des <- plot_stl(mean_precip_des) +
  ggtitle("STL for mean precip, deaseasonalised data")

#
plot_sst_og
plot_sst_des
plot_mean_sst
plot_mean_sst_des

plot_precip_og
plot_precip_des
plot_mean_precip
plot_mean_precip_des