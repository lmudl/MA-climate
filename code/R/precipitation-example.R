# precipitation-example
setwd("./Mufasa/Documents/MA-climate")
getwd()
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)

#precip_data <- nc_open("data/raw/precip.mon.total.1x1.v2018.nc")
precip_data <- nc_open("data/raw/drought/scPDSI_1901_2020.nc")
precip_data <- nc_open("data/interim/sst/had_sst_interim_selyear.nc")
precip_data <- nc_open("data/interim/drought/scpdsi19_sellonlatbox.nc")
precip_data <- nc_open("data/interim/sst/hadsst_sellonlatbox.nc")

# only run once
get_meta <- function(data, save_to){
    sink(save_to)
    print(data)
    sink()
}
# get_meta(precip_data, "precip_data_meta.txt")

# 1 variables: precipitation [lon, lat, time dimensions]

# get metadata and check with metadata txt information
lon <- ncvar_get(precip_data, "longitude")
# verbose if TRUE progress information is printed
lat <- ncvar_get(precip_data, "latitude", verbose = FALSE)
# here 0 bc is only first month of data
t <- ncvar_get(precip_data, "time") 
# looks good

precip <- ncvar_get(precip_data, "tos")
dim(precip) # 360, 180, 1512
# here missing_value other _FillValue
max(precip, na.rm = TRUE)
plot(hist(precip))
str(precip_data)
fillvalue <- ncatt_get(precip_data, "tos", "_FillValue")
nc_close(precip_data)

precip[precip == fillvalue$value] <- NA
precip_slice <- precip[, , 1000]
precip_slice[precip_slice > 100] <- NA
dim(precip_slice)
par(mar = rep(2, 4))
r <- raster(t(precip_slice), xmn = min(lon), xmx = max(lon),
            ymn = min(lat), ymx = max(lat),
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
#r <- flip(r, direction = "y")
plot(r)
#####
precip[180:190,90:100,220:210]
# mean over all years a bit much but first try
try <- apply(precip, c(1,2), function(x) mean(x, na.rm = TRUE))
dim(try) 
r <- raster(t(try), xmn = min(lon), xmx = max(lon),
            ymn = min(lat), ymx = max(lat),
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# r <- flip(r, direction = "y")
plot(r)






                                      