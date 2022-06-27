# diff time series and apply normal CV possible?
setwd("Repos/MA-climate/")
library(raster)
library(glmnet)
library(ggplot2)
source("code/R/helper-functions.R")



# load data, pprepare data
#ids <- readRDS(paste0(full_save_to,"index-list.rds"))
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
m <- abs(min(sst_train))
sst_train <- apply(sst_train, 2, function(x) sqrt(x+m))
sst_train <- cbind(sst_train, (seq(nrow(sst_train))))
mm <- (seq(nrow(sst_train))) %% 12
mm[mm==0] <- 12
mm <- as.factor(mm)
sst_train <- cbind(sst_train, mm)
sst_train <- data.matrix(sst_train)

precip_train <- precip[1:370]

tt_stand_og <- cv.glmnet(sst_train, precip_train, standardize=TRUE,
                      relax = TRUE, alignment="fraction",
                      nfolds = 5)
plot(tt_stand_og)
min(tt_stand_og$cvm) #710

sst_eval <- sst[371:432,]
m <- abs(min(sst_eval))
sst_eval <- apply(sst_eval, 2, function(x) sqrt(x+m))
sst_eval <- cbind(sst_eval, (seq(nrow(sst_eval))))
mm <-(seq(nrow(sst_eval))) %% 12
mm[mm==0] <- 12
mm <- as.factor(mm)
sst_eval <- cbind(sst_eval, mm)
sst_eval <- data.matrix(sst_eval)

precip_eval <- precip[371:432]
#precip_eval <- diff(precip_eval, 2)
preds <- predict(tt_stand_og, newx=sst_eval,
                 s=c("lambda.min"),
                 gamma=c("gamma.min"))
#preds
comp_mse(preds, precip_eval)
plot(ts(precip_eval))
lines(ts(preds))





