# Deseasonalise with rtsa
# which can do stl on raster data
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
# library(devtools)
# install_github("marchtaylor/sinkr")
# remotes::install_github("ffilipponi/rtsa")
library(rtsa)
library(stringr)
library(lubridate)

ref <- "months since 1900-1-1 00:00:00"

# deseasonalise with rtsa package
# deseasonalise sst ####
# load sst
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
# ext <- raster::extent(-181,0,-89,89)
# sst <- crop(sst, ext)
tstamps <- prepare_timestamps(sst, ref_string = ref)
sst_raster_ts <- rts(sst, tstamps)
sst_mask <- prepare_mask(sst)

# TODO check variables for stl algorithm here, default should be fine
# NOTE default is "periodic" so the seasonality does not change over time
sst_stl_res <- rtsa.stl(rasterts = sst_raster_ts, rastermask = sst_mask)
saveRDS(sst_stl_res, "data/processed/rtsa_deseasonalised_sst.rds")


# deseasonalise precip ####
# load precip #
#precip <- brick("data/interim/drought/chirps_setreftime.nc")
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
tstamps <- prepare_timestamps(precip, ref)
precip_raster_ts <- rts(precip, tstamps)
precip_mask <- prepare_mask(precip)
precip_stl_res <- rtsa.stl(rasterts = precip_raster_ts, rastermask = precip_mask)
saveRDS(precip_stl_res, "data/processed/rtsa_deseasonalised_precip.rds")
