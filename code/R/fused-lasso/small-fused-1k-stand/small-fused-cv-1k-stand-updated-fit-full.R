# fit full
# small-fused-cv-1k-stand-updated
getwd() 
#is MA-climate on the shell 
# but we could add a check here
#setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(sp)
library(raster)
library(igraph)
library(genlasso)

# path to model and modelname
model_name <- "small-fused-cv-1k-stand-updated"
save_to <- paste0("results/CV-lasso/", model_name, "/")

# settings
standardize_features <- TRUE
standardize_response <- TRUE

# load data sst
sst <- brick("data/interim/sst/ersst_setreftime.nc", var = "sst")
ext <- extent(-180,0,-50,40)
sst <- crop(sst,ext)
# create igraph object
g <- igraph_from_raster(sst)
rm(sst)


small_sst_cv <- readRDS("data/processed/small_sst_cv.rds")
small_sst_eval <- readRDS("data/processed/small_sst_eval.rds")

stand_features <- function(x_train, x_test) {
  stand_list <- list()
  mean_x_train <- apply(x_train,2, mean)
  sdn_x_train <- apply(x_train,2,sdN)
  x_train <- scale(x_train, center=mean_x_train,
                   scale=sdn_x_train)
  x_test <- scale(x_test, center=mean_x_train,
                  scale=sdn_x_train)
  stand_list$train <- x_train
  stand_list$test <- x_test
  return(stand_list)
}

if(standardize_features == TRUE) {
  stand_list <- stand_features(x_train = small_sst_cv, x_test = small_sst_eval)
  small_sst_cv <- stand_list$train
  small_sst_eval <- stand_list$test
}

precip_cv <- readRDS("data/processed/precip_cv.rds")
precip_eval <- readRDS("data/processed/precip_eval.rds")

stand_response <- function(y_train) {
  mean_y_train <- mean(y_train)
  sdn_y_train <- sdN(y_train)
  y_train <- scale(y_train, center=mean_y_train, 
                   scale=sdn_y_train)
}

if(standardize_response == TRUE) {
  precip_cv <- stand_response(precip_cv)
}


full_model <- fusedlasso(y=precip_cv, X=small_sst_cv, graph=g,
                         verbose=TRUE, maxsteps = 1000)

saveRDS(full_model, paste0(save_to, "full-model.rds"))

# plot full model
# load eval data already done
# preds_obj <- predict.genlasso(full_model, Xnew = small_sst_eval)
# preds <- preds$fit
# lambda <- preds$lambda
# 
# errors <- apply(preds, 2 function(x) comp_mse(x, precip_eval))
# # could also find out minlambda from
# # cv and save it
# min_id <- which.min(errors)



# use lambda min for now
full_model <- readRDS(paste0(save_to, "full-model.rds"))
# predict on sst_eval stand
preds_obj <- predict.genlasso(full_model, Xnew=small_sst_eval)
preds <- preds_obj$fit

unstand_predictions <- function(y_train, predictions) {
  mean_y_train <- attr(y_train,"scaled:center")
  sdn_y_train <- attr(y_train, "scaled:scale")
  predictions <- apply(predictions, 2, function(x) x*sdn_y_train + mean_y_train)
}

if(standardize_response == TRUE) {
  preds <- unstand_predictions(precip_cv, preds)
}

errors <- apply(preds, 2, function(x)comp_mse(precip_eval, x))
which.min(errors)
plot(ts(precip_eval))
lines(ts(preds[,1000]))
plot(errors)
