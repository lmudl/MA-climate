# diff time series and apply normal CV possible?
setwd("Repos/MA-climate/")
library(raster)
library(glmnet)
library(ggplot2)
source("code/R/helper-functions.R")



# load data, pprepare data
ids <- readRDS(paste0(full_save_to,"index-list.rds"))
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst <- as.matrix(sst)
sst <- add_colnames("data/interim/sst/ersst_setreftime.nc",sst)
sst <- prepare_sst(sst)
dim(sst)
anyNA(sst)
precip <- as.matrix(precip)
precip <- apply(precip, 2, mean)

sst_train <- sst[1:370,]
precip_train <- precip[3:370]

sst_train <- apply(sst_train, 2, function(x) diff(x, 2))
#precip_train <- diff(precip, 2)
sst_train <- cbind(sst_train, (seq(nrow(sst_train))+2))
mm <- (seq(nrow(sst_train))+2) %% 12
mm[mm==0] <- 12
mm <- as.factor(mm)
sst_train <- cbind(sst_train, mm)
sst_train <- data.matrix(sst_train)
#tt <- cv.glmnet(sst_train, precip_train, standardize=FALSE,)
tt_stand <- cv.glmnet(sst_train, precip_train, standardize=TRUE,
                      relax = TRUE, alignment="fraction")
plot(tt_stand)
min(tt_stand$cvm) #941.3494

sst_eval <- sst[371:432,]
sst_eval <- apply(sst_eval, 2, function(x) diff(x, 2))
sst_eval <- cbind(sst_eval, (seq(nrow(sst_eval))+2))
mm <-(seq(nrow(sst_eval))+2) %% 12
mm[mm==0] <- 12
mm <- as.factor(mm)
sst_eval <- cbind(sst_eval, mm)
sst_eval <- data.matrix(sst_eval)

precip_eval <- precip[373:432]
#precip_eval <- diff(precip_eval, 2)
preds <- predict(tt_stand, newx=sst_eval,
                 s=c("lambda.min"),
                    gamma=c("gamma.min"))
preds
comp_mse(preds, precip_eval)
plot(ts(preds))
plot(ts(precip_eval))





