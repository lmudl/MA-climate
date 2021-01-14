# compute basic correlations
# load data
## load sst
## load precip
# compute correlation
setwd("./MA-climate")
getwd()
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)

# load data and prepare data
sst_file <- nc_open("data/interim/sst_all.nc")
lon_sst <- ncvar_get(sst_file, "lon")
lat_sst <- ncvar_get(sst_file, "lat", verbose = FALSE)
t_sst <- ncvar_get(sst_file, "time")
sst <- ncvar_get(sst_file, "sst")
dim(sst) # 180 89 2004
fillval_sst <- ncatt_get(sst_file, "sst", "_FillValue")
nc_close(sst_file)
sst[sst == fillval_sst$value] <- NA

precip_file <- nc_open("data/raw/precip.mon.total.1x1.v2018.nc")
lon_precip <- ncvar_get(precip_file, "lon")
lat_precip <- ncvar_get(precip_file, "lat")
t_precip <- ncvar_get(precip_file, "time")
precip <- ncvar_get(precip_file, "precip")
dim(precip) # 360 180 1512
fillval_precip <- ncatt_get(precip_file, "precip", "missing_value")
precip[precip == fillval_precip$value] <- NA

# take on slice of each and compute correlation, basic
# sst is already slice
sst_slice <- sst[,,2004]
dim(sst)
precip_slice <- precip[,,1000]
dim(precip) # 360, 180, 1512
dim(sst_slice)
dim(precip_slice) # 360, 180

# restrict to window that is of importance for us
# general window 180 W - 3 W, 40 N - 40 S
head(lon_sst);length(lon_sst) # 0,2,4, and 180
# 180 is entry 91
lon_sst[91]
# 3 not there so we take 2 is entry 2
# but with rasta it different because they count from 0 left to 360 right
# so 180 W to 2 W ist 180 to 358 entry 
lon_sst[180]
# for lat 
head(lat_sst);length(lat_sst) #-88, -86, -84 and 89
# 40 is entry 25 and 65
lat_sst[25]; lat_sst[65]

try <- sst_slice[91:180, 25:64]
r <- raster(t(try), xmn = 180, xmx = 360,
            ymn = -40, ymx = 40,
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction = "y")
plot(r)
dim(sst_slice) # 180 and 89
try[1:5,1:5]

#this looks good# amazon window 10 S - 0, 70 W - 55 W
head(lat_precip); tail(lat_precip);length(lat_precip) # 89.5, 88.5,.., -88.5, -89.5 and 180
head(lon_precip); tail(lon_precip);length(lon_precip) # 0.5, 1.5,...,358.5, 395.5 and 360
#10 S, 89.5-10=79.5 ca 80 steps, 
lat_precip[80] # 10.5
lat_precip[90] # 0.5
#70 W - 55 W
lon_precip[71] #70.5
lon_precip[55] #54.5

# no values apparently in amazonas for first time window
try2 <- precip_slice[55:71,80:90]
r <- raster(t(precip_slice), xmn = 180, xmx = 360,
            ymn = -40, ymx = 40,
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
#r <- flip(r, direction = "y")
plot(r)



