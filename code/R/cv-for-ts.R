# Script only for doing Cross Validation for time series
# things to consider:
# dataset used, sst and precip/spi
# if spi there are less observations because spi depends on past values
# therefore drop NAs in target and resp also observations in predictors (sst)
# model used, depending on the model we might need additional functions
# so far only normal lasso from glmnet in use
# type of cross validation uses, so far we use blocked non-overlapping 
# cross validation, additioanlly amount of overlap or classic CV could be used

# here maybe define some variables/ modes for CV/ parameters etc.
# path to target
# target <- "data/processed/deseasonalised_precip.rds"
target_path <- "data/processed/spi_3.rds"
# spi_mode, then also drop NAs from target and drop observations from sst
is_spi <- logical()
spi_window <- numeric()

set_target_vars <- function(target_path) {
  if(grepl("spi", target_path)) {
    is_spi <- TRUE
    spi_window <- as.numeric(stringr::str_extract(target_path, "\\d+"))
    return(list(is_spi, spi_window))
  } else {
    is_spi <- FALSE
    return(list(is_spi, numeric()))
  }
}
var_list <- set_target_vars(target_path)
is_spi <- var_list[[1]]
spi_window <- var_list[[2]]

# path to predictors
features_path <- "data/processed/deseasonalised_sst.rds"

# set working directory
setwd("Repos/MA-climate")

# load packages
library(raster)
library(caret)
library(glmnet)

# load sst
# rows are coord, cols are months

load_features <- function(feature_path) {
  features <- readRDS("data/processed/deseasonalised_sst.rds")
  return(features)
}
features <- load_features(feature_path)
dim(features)

# load precip or spi
# rows are coord, cols are months

load_target <- function(target_path, is_spi) {
  target <- readRDS(target_path)
  return(target)
}

target <- load_target(target_path)

# computing the spi results in "loosing" some observations, creating NA's
# we drop these columns in target nd features with the following function
trim_data <- function(data, spi_window) {
  drop <- spi_window-1
  data <- data[,-c(1:drop)]
  return(data)
}

dim(target)
dim(features)

if(is_spi) {
  target <- trim_data(target, spi_window)
  features <- trim_data(features, spi_window)
}

# compute mean of target
target <- apply(target, 2, mean)
target <- matrix(target)

#transpose sst and add coord as colnames
#transpose so that each month is one observation, usable for glmnet then
features <- add_colnames("data/interim/sst/ersst_setreftime.nc", sst = features)


########TESTING#########

# possibility 2
# apply glmnet function to lambdas directly

source("code/R/helper-functions.R")

features <- prepare_sst(features)

# test cv_for_ts 
# but only do one fold
# namely say 430-60 last 5 years we dont want do use for test/train but for validation
# then we have 370, 372/5= 74 so 74 per fold and 60 train 14 test
# so 356-370 is test, 298-355 is train

# computing indices for test runs
# years too use in validation
val_years <- 5
rows_left <- nrow(features)-val_years*12 #370
n_folds <- 5
# number of observations in each fold
obs_in_fold <- rows_left/5 # 74
n_test <- 14
# number of obs in train
n_train <- 60
start_train <- rows_left-obs_in_fold
start_test <- start_train+n_train+1
train_ind <- start_train:(start_train+n_train)
test_ind <- (start_train+n_train+1):(start_test+n_test)

features_train <- features[train,]
features_test <- features[test, ]
target_train <- target[train,]
target_test <- target[test,]

lambdas <- get_lambda_values(features[c(train,test),], target[c(train,test),])

mod <- glmnet(features_train, target_train, lambda=rev(lambdas))
plot(mod)

err <- c(NA)
for(i in 1:length(lambdas)) {
  pred <- predict(mod, newx = features_test, s = lambdas[i])
  err_i <- sum((pred-target_test)^2)
  err[i] <- err_i
}
plot(err)

#####
# run complete cross validation but without 5 last years
rows_left <- 370 # if we keep last 5 years for validation
features_cv <- features[1:rows_left, ]
target_cv <- target[1:rows_left,]


rm(features)

#TODO find out why trouble with lambda values

example_model <- cv_for_ts(features_cv, target_cv, nfold = 5, size_train = 60, size_test = 14,
                           save_folder = "spi-3")
debug(cv_for_ts)
debug(get_lambda_values)
get_lambda_values(features_cv, target_cv)

# possibility 1
# do 2 loops like in cv for ts

