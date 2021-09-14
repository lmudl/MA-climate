# setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(dplyr)
library(ncdf4)

# load data
sst_og <- brick("data/interim/sst/ersst_setreftime.nc",
                    varname= "sst")
precip_og <- brick("data/interim/drought/chirps_setreftime.nc")

# decompose sst and save to RDS 
library(tictoc)
tic()
deseasonalised_sst <- deseasonalise_brick_wna(brick = sst_og, nlayers = nlayers(sst_og), ncells = ncell(sst_og))
saveRDS(deseasonalised_sst, file = "data/processed/deseasonalised_sst.rds")
toc() # 15.84 sec elapsed

# decompose sst and save to RDS
tic()
deseasonalised_precip <- deseasonalise_brick_wna(brick = precip_og, nlayers = nlayers(precip_og), ncells =ncell(precip_og))
saveRDS(deseasonalised_precip, file = "data/processed/deseasonalised_precip.rds")
toc() # 96.99 sec elapsed
