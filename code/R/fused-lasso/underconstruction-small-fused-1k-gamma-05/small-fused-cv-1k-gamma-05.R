# fused cv-full
# fused lasso test with new cv method
getwd() # is MA-climate on the shell 
# but we could add a check here
# setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")

# load packages
# requiredPackages = c('raster','igraph','glmnet', 'caret')
# for(p in requiredPackages){
#   if(!require(p,character.only = TRUE)) install.packages(p)
#   library(p,character.only = TRUE)
# }
library(sp)
library(raster)
library(igraph)
library(genlasso)
#library(caret)
train_max <- 1:370
maxsteps <- 1000

# load data sst
sst <- brick("data/interim/sst/ersst_setreftime.nc", var = "sst")
ext <- extent(-180,0,-50,40)
sst <- crop(sst,ext)
# create igraph object
g <- igraph_from_raster(sst)
rm(sst)

small_sst_cv <- readRDS("data/processed/small_sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")
# drop intercept
precip_cv <- scale(precip_cv, center = TRUE, scale = FALSE)

err <- cv_for_ts(small_sst_cv, precip_cv, nfold=5, size_train=60, size_test=14, 
                 save_folder="small-fused-cv-1k-gamma-05",
                 model = "fused", graph = g, maxsteps = maxsteps,
                 gamma=1, standardize_response = TRUE)


