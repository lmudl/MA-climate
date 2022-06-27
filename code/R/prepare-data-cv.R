ids_cv <- 1:370
ids_eval <- 371:432
save_to <- "data/processed/"

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




