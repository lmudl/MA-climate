ids_cv <- 1:370
ids_eval <- 371:432
save_to <- "data/processed/"
#setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)

# raw data ####
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst <- as.matrix(sst)
sst <- add_colnames("data/interim/sst/ersst_setreftime.nc",sst)
sst <- prepare_sst(sst)
dim(sst)
anyNA(sst)
precip <- as.matrix(precip)
precip <- apply(precip, 2, mean)

testit::assert(max(ids_eval) == nrow(sst) & max(ids_eval) == length(precip))

features_cv <- sst[ids_cv,]
saveRDS(features_cv, paste0(save_to,"sst_cv.rds"))

target_cv <- precip[ids_cv]
saveRDS(target_cv, paste0(save_to, "precip_cv.rds"))

features_eval <- sst[ids_eval,]
saveRDS(features_eval, paste0(save_to, "sst_eval.rds"))

target_eval <- precip[ids_eval]
saveRDS(target_eval, paste0(save_to, "precip_eval.rds"))

# small sst window ####
sst <- brick("data/interim/sst/ersst_setreftime.nc", var = "sst")
ext <- extent(-180,0,-50,40)
# ext <- extent(-180,-10,-50,40)
small_sst <- crop(sst,ext)
# plot(small_sst[[1]])
rm(sst)
coord <- coordinates(small_sst)
cnames <- paste(coord[,1], coord[,2])
small_sst <- as.matrix(small_sst)
small_sst <- t(small_sst)
colnames(small_sst) <- cnames
small_sst <- prepare_sst(small_sst)
dim(small_sst)
anyNA(small_sst)

small_sst_cv <- small_sst[ids_cv,]
saveRDS(small_sst_cv, paste0(save_to, "small_sst_cv.rds"))

small_sst_eval <- small_sst[ids_eval,]
saveRDS(small_sst_eval, paste0(save_to, "small_sst_eval.rds"))


# anomalies ####
sst_anam <- brick("data/interim/sst/ersst_setreftime.nc", varname = "ssta")
sst_anam <- as.matrix(sst_anam)
sst_anam <- add_colnames("data/interim/sst/ersst_setreftime.nc",sst_anam)
sst_anam <- prepare_sst(sst_anam)
dim(sst_anam)
anyNA(sst_anam)

sst_anam_cv <- sst_anam[ids_cv,]
saveRDS(sst_anam_cv, "data/processed/sst_anam_cv.rds")

sst_anam_eval <- sst_anam[ids_eval,]
saveRDS(sst_anam_eval, "data/processed/sst_anam_eval.rds")

# deseasonalised ####
stl_sst <- readRDS("data/processed/rtsa_deseasonalised_sst.rds")
stl_precip <- readRDS("data/processed/rtsa_deseasonalised_precip.rds")

des_sst <- getValues(stl_sst@trend) + getValues(stl_sst@remainder)
des_precip <- getValues(stl_precip@trend) + getValues(stl_precip@remainder)

des_sst <- add_colnames("data/interim/sst/ersst_setreftime.nc",des_sst)
des_sst <- prepare_sst(des_sst)
dim(des_sst)
anyNA(des_sst)

#des_precip <- as.matrix(des_precip)
des_precip <- apply(des_precip, 2, mean)

testit::assert(max(ids_eval) == nrow(des_sst) & max(ids_eval) == length(des_precip))

des_sst_cv <- des_sst[ids_cv,]
saveRDS(des_sst_cv, paste0(save_to,"des_sst_cv.rds"))
des_precip_cv <- des_precip[ids_cv]
saveRDS(des_precip_cv, paste0(save_to, "des_precip_cv.rds"))

des_sst_eval <- des_sst[ids_eval,]
saveRDS(des_sst_eval, paste0(save_to, "des_sst_eval.rds"))
des_precip_eval <- des_precip[ids_eval]
saveRDS(des_precip_eval, paste0(save_to, "des_precip_eval.rds"))

# graph objects ####
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
graph_sst <- igraph_from_raster(sst)
saveRDS(graph_sst, paste0(save_to, "graph_sst.rds"))

ext <- extent(-180,0,-50,40)
small_sst <- crop(sst,ext)
small_graph_sst <- igraph_from_raster(small_sst)
saveRDS(small_graph_sst, paste0(save_to, "small_graph_sst.rds"))

