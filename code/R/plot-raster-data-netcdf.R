# first plot with raster and netcdf

setwd("./Mufasa/Documents/MA-climate")
getwd()
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)

data <- nc_open("data/raw/hadsst/HadISST_sst.nc")
varname <- "sst"

# get metadata and check with metadata txt information
lon <- ncvar_get(data, "longitude")
# verbose if TRUE progress information is printed
lat <- ncvar_get(data, "latitude", verbose = FALSE)
# here 0 bc is only first month of data
t <- ncvar_get(data, "time") 
# looks good

var <- ncvar_get(data, varname)
dim(var) # 360, 180, 1512
# here missing_value other _FillValue
fillvalue <- ncatt_get(data, varname, "missing_value")
nc_close(data)

var[var == fillvalue$value] <- NA
var_slice <- var[, , 1]
dim(var_slice)

r <- raster(t(var_slice), xmn = min(lon), xmx = max(lon),
            ymn = min(lat), ymx = max(lat),
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
#r <- flip(r, direction = "y")
plot(r)

#####
var[180:190,90:100,220:210]
# mean over all years a bit much but first try
try <- apply(var, c(1,2), function(x) mean(x, na.rm = TRUE))
dim(try) 

r <- raster(t(try), xmn = min(lon), xmx = max(lon),
            ymn = min(lat), ymx = max(lat),
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# r <- flip(r, direction = "y")
plot(r)


