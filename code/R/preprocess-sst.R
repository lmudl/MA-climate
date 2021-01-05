# preprocess sst data since they come as one file per month
# bring them all together 
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(assertthat)

# to do maybe check for each file if dims are correct?
# to do smaller the window, by using only lat lon we need
# see basic corr

preprocess_sst <- function(path_to_data) {
  file_list <- get_file_list(path_to_data)
  dims <- get_dims(path_to_data, file_list)
  full_sst <- merge_sst(path_to_data, file_list, dims)
  return(full_sst)
}

get_file_list <- function(path_to_data) {
  file_list <- list.files(path_to_data)# for testing [2000:2005]
  valid <- grep("ersst", file_list)
  file_list <- sort(file_list[valid])
  return(file_list)
}

get_dims <- function(path_to_data, file_list) {
  first <- open_file(path_to_data, file_name = file_list[1])
  dims <- c(first$dim$lat$len, first$dim$lon$len, length(file_list))
  return(dims)
}

open_file <- function(path_to_data, file_name) {
  path <- paste0(path_to_data, file_name)
  file <- nc_open(path)
  return(file)
}

merge_sst <- function(path_to_data, file_list, dims) {
  to_fill <- array(NA, dim = dims)
  j <- 1
  for (i in file_list) {
    file <- open_file(path_to_data, i)
    data <- ncvar_get(file, "sst")
    # data <- filter_data(data)
    to_fill[, , j] <- data
    j <- j + 1
  }
  return(to_fill)
}

filter_data <- function(data) {
  # lat is count from 0 to 360, steps by 2: 0 2 4...
  # coord go from 180 W to 0 
  # -> lat 180 W to 0 is entry 91 to 180
  # long is count from -80 to 80, steps by 2: -88 -86 -84...
  # coord go from -40 N to 40 S
  # -> lat -40 N to 40 S is entry 25 to 64
  data <- data[91:180, 25:64]
  return(data)
}

debug(preprocess_sst)
test <- preprocess_sst("data/raw/sst/")  

# check with basic-corr file
f <- open_file("data/raw/sst/", "ersst.v5.202012.nc")
d <- ncvar_get(f, "sst")
d[1:5, 1:5]
