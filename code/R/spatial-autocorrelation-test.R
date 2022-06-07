getwd()
setwd("Repos/MA-climate/")
library(raster)
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")

r <- raster(nrows=10, ncols=10)
values(r) <- 1:ncell(r)
plot(r)
Moran(r)
# Rook's case
f <- matrix(c(0,1,0,1,0,1,0,1,0), nrow=3)
Moran(r, f)

Geary(r)

x1 <- MoranLocal(r)

# Rook's case
x2 <- MoranLocal(r, w=f)
plot(x1)
plot(x2)

sst_m <- calc(sst, mean)
mm <-MoranLocal(sst_m, w=f)
plot(mm)

precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
precip <- brick("data/interim/drought/chirps_setreftime.nc")
precip_m <- calc(precip, mean)
pm <- GearyLocal(precip_m, w = f)
plot(pm)
