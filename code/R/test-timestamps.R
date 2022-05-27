# play around with timestamps
library(raster)
library(ncdf4)
setwd("Repos/MA-climate/")
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
?timestamp()
sst

sst <- raster("data/raw/sst/ersst/ersst.v5.188001.nc", varname = "sst")
sst
print(sst)
sst@data
v <- getValues(sst)
v
dim(v)

# units: minutes since 1880-01-01 00:00

sst <- brick("data/raw/sst/ersst.nc", varname = "sst")
print(sst)
sst <- brick

ncvar_get(sst, "time")
print(sst)
s <- nc_open("data/raw/sst/ersst.nc")
t <- ncvar_get(s, "time")

tunits <- ncatt_get(s, "time", "units")
tunits
tunits$value
t
#install.packages("RNetCDF")
RNetCDF::utcal.nc(tunits$value, t[1:10], type="c")

# how do we want the timestamps?
# months after 1900?
# lets take common time window as reference time point
# RNetCDF can give time as dataframe, nice, as string or as POSIXct

#install.packages("chron")
library(chron)

t[1:10]

# I have minutes since 1880-01-01 00:00
# I want months since "1901-1-1,00:00:00
# So is this true for the precip data?

p <- nc_open("data/raw/drought/chirps-v2.0.monthly.nc")
tp <- ncvar_get(p, "time")
tp_units <- ncatt_get(p, "time", "units")
tp_units$value

tp[1]

# how did I align originally?

# change that in time variable or do it 
# in every code directly
#
sst <- nc_open("data/raw/sst/ersst.nc")
# before used interim setreftime sst
sst$var$sst$

sst$dim$time$vals <- as.integer(sst$dim$time$vals)
ncvar_put(sst, time, as.integer(sst$dim$time$vals))
sst_time <- ncvar_get(sst, "time")



(sst_time_print <- ncatt_get(sst, "time", "units"))
sst_time

precip <- nc_open("data/interim/drought/chirps_setreftime.nc")
precip_time <- ncvar_get(precip, "time")
(precip_time_print <- ncatt_get(precip, "time", "units"))

sst_time == precip_time
tail(sst_time)
sst_time

sst$


# Summary so far old work seems good
# timestamp alignment works
# Problem only is that there are some non-integer months in sst
# timestamps

# original question
RNetCDF::utcal.nc(sst_time_print$value, tail(sst_time), type="n")
test <- "minutes since 1880-01-01 00:00"
t <- RNetCDF::utcal.nc(test, tail(sst_time), type="s")
t
typeof(t)
# 
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst@data@names
df <- raster::as.data.frame(sst)
dim(df)
tail(df)[,415:432]
?raster::as.data.frame
