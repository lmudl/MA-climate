# what happens if we fit full model to only one cell
# with lasso 

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

# load data sst
sst <- brick("data/interim/sst/ersst_setreftime.nc", var = "sst")
ext <- extent(-180,0,-50,40)
sst <- crop(sst,ext)
# prepare sst for fitting
coord <- coordinates(sst)
cnames <- paste(coord[,1], coord[,2])
sst <- as.matrix(sst)
sst <- t(sst)
colnames(sst) <- cnames
sst <- prepare_sst(sst)
sst_tr <- sst[1:train_max,]

# load data precip
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")

# prepare precip for fitting
precip <- values(precip)
precip <- precip[1,]
precip_tr <- precip[1:train_max]


cell_mod <- glmnet::glmnet(sst_tr, precip_tr, nlambda = 100)
predicted <- predict(cell_mod, newx = sst[371:432,])
err_col <- apply(predicted, 2, function(x) mean((x-precip[371:432])^2))
which.min(err_col)
df <- data.frame(preds = predicted[,100], target = precip[371:432])
#dim(preds_full)
ggplot() + geom_line(data = df, mapping = aes(x=1:62, y=preds, colour = "blue")) +
  geom_line(data = df, mapping= aes(x=1:62, y=target))

