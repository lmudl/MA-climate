# Testscrip for the first fold with fused lasso
getwd() # is MA-climate on the shell 
# but we could add a check here
source("code/R/helper-functions.R")

# load packages

# requiredPackages = c('raster','igraph','glmnet', 'caret')
# for(p in requiredPackages){
#   if(!require(p,character.only = TRUE)) install.packages(p)
#   library(p,character.only = TRUE)
# }
library(raster)
library(igraph)
library(glmnet)
library(caret)

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

# load data precip
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")

# prepare precip for fitting
precip <- values(precip)
precip <- apply(precip, 2, mean)

# create timeslices
index_list <- caret::createTimeSlices(1:nrow(sst), initialWindow=60, horizon=14,
                                      skip=60+14-1)
train_id <- index_list$train$Training060
test_id <- index_list$test$Testing060

print("data prepared now fit model")
# get 1fold/ prepare data # fit igraph model
fused_mod_f1 <- fusedlasso(y=precip[train_id], X=sst[train_id,], graph=g)
# save model
print("model was fitted")
save(mod_f1, "results/CV-lasso/fused_mod_f1.rds")
# predict on test data
preds <- predict.genlasso(mod_1, Xnew=sst[test_id])
errs <- apply(preds$fit, 2, function(x) mean((x-precip[test_id])^2))
print(min(errs))


