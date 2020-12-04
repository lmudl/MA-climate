# https://rpubs.com/boyerag/297592
# open and work with NetCDF data
setwd("./Mufasa/Documents/MA-climate")
getwd()
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)

# read in netcdf file content
nc_data <- nc_open("data/raw/ersst.v5.185401.nc")
# save the print(nc) dump to a text file
{
  sink('ersst.v5.185401.nc_metadata.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = FALSE)
t <- ncvar_get(nc_data, "time") #here 0 bc is only first month
sst <- ncvar_get(nc_data, "sst")
fill_val <- ncatt_get(nc_data, "sst", "_FillValue") #-999
nc_close(nc_data)
sst[sst == fill_val$value] <- NA
r <- raster(t(sst), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction = "y")
plot(r)
 