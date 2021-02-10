getwd()
setwd("MA-climate")
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)

drought_data <- nc_open("data/raw/cru/scPDSI-1901-2019.nc")
temp_data < nc_open("data/raw/hadcrut/HadCRUT.5.0.1.0.analysis.anomalies.ensemble_mean.nc")

# only run once
get_meta <- function(data, save_to){
  sink(save_to)
  print(data)
  sink()
}

#get_meta(cru_data, "meta/cru_drought_meta.txt")

