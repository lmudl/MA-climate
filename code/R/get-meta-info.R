# code for saving the metadata text file
setwd("./Mufasa/Documents/MA-climate")
getwd()
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)

# Input 
path <- "data/raw/hadsst/HadISST_sst.nc"

# load data
data <- nc_open(path)

# get meta data only run once

get_meta <- function(data, path){
  end <- tail(strsplit(path, "/")[[1]], n = 1)
  save_to <- paste0("meta/", end, "_meta.txt")
  if (!file.exists(save_to)) {
    sink(save_to)
    print(data)
    sink()
  }
  else {
    print("metadata file already exists")
  }
}

get_meta(data, path)
