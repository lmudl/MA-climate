# fused full
# setwd("Repos/MA-climate/")
# load err_mat, which.min
# get lambdas from the models
# BUT! lambdas are very different
# max is 49499
# rerun full model with 50.000 steps

getwd() 
#is MA-climate on the shell 
# but we could add a check here
#setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(sp)
library(raster)
library(igraph)
library(genlasso)

# testing data set until 370
train_max <- 370
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
sst <- prepare_sst(sst)
sst <- sst[1:train_max,]

# load data precip
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")

# prepare precip for fitting
precip <- values(precip)
precip <- apply(precip, 2, mean)
precip <- precip[1:train_max]

full_mod <- fusedlasso(y=precip, X=sst, graph=g,
                       verbose=TRUE, maxsteps = maxsteps)

saveRDS(full_mod, "results/CV-lasso/fused-full-20k/full-model.rds")
# run until here in 





