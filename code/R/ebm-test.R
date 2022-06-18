install.packages("interpret")
library(interpret)


# fused cv-full
# fused lasso test with new cv method
getwd() # is MA-climate on the shell 
# but we could add a check here
#setwd("Repos/MA-climate/")
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
maxsteps <- 20000

# load data sst
sst <- brick("data/interim/sst/ersst_setreftime.nc", var = "sst")
 ext <- extent(-180,0,-50,40)
 sst <- crop(sst,ext)
# create igraph object
g <- igraph_from_raster(sst)
# prepare sst for fitting
coord <- coordinates(sst)
cnames <- paste(coord[,1], coord[,2])
sst <- as.matrix(sst)
sst <- t(sst)
colnames(sst) <- cnames
#sst <- prepare_sst(sst)
sst <- sst[train_max,]


# load data precip
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")

# prepare precip for fitting
precip <- values(precip)
precip <- apply(precip, 2, mean)
precip <- precip[train_max]



###
dim(sst)
te <- na.omit(t(sst))
te <- t(te)
dim(te)
ebm <- ebm_classify(te, precip, max_rounds = 10)
#started 16:04
proba_test <- ebm_predict_proba(ebm, sst[371:432,])
ebm_show(ebm,"mpg")
?ebm_show
?ebm_classify
plot(proba_test)
proba_test
