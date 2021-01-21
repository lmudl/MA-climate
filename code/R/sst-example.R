# https://rpubs.com/boyerag/297592
# open and work with NetCDF data
# SST data
# from https://www.ncdc.noaa.gov/data-access/marineocean-data/extended-reconstructed-sea-surface-temperature-ersst-v5
# Coordinate Reference Systems https://rspatial.org/raster/spatial/6-crs.html
setwd("./Mufasa/Documents/MA-climate")
getwd()
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)

# read in netcdf file content
sst_data <- nc_open("data/interim/sst-anomalies.nc")
# save the print(nc) dump to a text file
# {
#   sink('sst_all_metadata.txt')
#   print(sst_data)
#   sink()
# }
# there are two variables:
# float sst [lon, lat, lev, time]
# float ssta [lon, lat, lev, time]
# sst a for anamolies
# [] contains the dimensions

# here get metadata information about file
lon <- ncvar_get(sst_data, "lon")
# verbose if TRUE progress information is printed
lat <- ncvar_get(sst_data, "lat", verbose = FALSE)
# here 0 bc is only first month of data
t <- ncvar_get(sst_data, "time") # 2003, length

# get sst data of interest (also of interest ssta)
sst <- ncvar_get(sst_data, "sst")
# 180 89 2004, lon lat time respectively, each time point is one month
dim(sst) 
# get fill value for missing values and replace with NA
fill_val <- ncatt_get(sst_data, "sst", "_FillValue") #-999
nc_close(sst_data) # close file
sst[sst == fill_val$value] <- NA
# slice sst
sst_slice <- sst[,,10]

# crs string tells how we define our geospatial grid
# proj: projection
# ellps: the ellipsoid (how earth's roundness is calculated)
# datum: datum refeers to 0,0 reference for coordinate system
#   used in the projection
# +no_defs+: ensures that no defaults are read from the defaults files. 
#   Sometimes they cause suprising problems.
# towgs84 = 0,0,0 conversion factor used if a datum conversion
#   is required
# t(sst) data needs to be transposed
r <- raster(t(sst_slice), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), 
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# and flipped data usually starts at bottom left corner
# but need try and error
r <- flip(r, direction = "y")
plot(r)
any(sst != 0)
