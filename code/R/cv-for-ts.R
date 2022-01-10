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
target_path <- "data/processed/deseasonalised_precip.rds"
# target_path <- "data/processed/spi_3.rds"
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
# load helper functions
source("code/R/helper-functions.R")

# load sst
# rows are coord, cols are months

load_features <- function(features_path) {
  features <- readRDS(features_path)
  return(features)
}
features <- load_features(features_path)
dim(features)

# load precip or spi
# rows are coord, cols are months

load_target <- function(target_path, is_spi) {
  target <- readRDS(target_path)
  return(target)
}

target <- load_target(target_path)
dim(target)

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

features_train <- features[train_ind,]
features_test <- features[test_ind, ]
target_train <- target[train_ind,]
target_test <- target[test_ind,]

lambdas <- get_lambda_values(features[c(train_ind,test_ind),], target[c(train_ind,test_ind),])

mod <- glmnet(features_train, target_train, lambda=rev(lambdas))
plot(mod)

err <- c(NA)
for(i in 1:length(lambdas)) {
  pred <- predict(mod, newx = features_test, s = lambdas[i])
  err_i <- sum((pred-target_test)^2)/length(pred)
  err[i] <- err_i
}
plot(err)

#####
# run complete cross validation but without 5 last years
rows_left <- 370 # if we keep last 5 years for validation
features_cv <- features[1:rows_left, ]
target_cv <- target[1:rows_left,]


rm(features)

# TODO find out why trouble with lambda values


# possibility 1
# do 2 loops like in cv for ts


example_model <- cv_for_ts(features_cv, target_cv, nfold = 5, size_train = 60, size_test = 14,
                           save_folder = "cv-lasso-08-11-21")
370/5

#debug(cv_for_ts)
#debug(get_lambda_values)
get_lambda_values(features_cv, target_cv)

glmnet::plot.cv.glmnet(em)
plot(example_model[,5])

# Plot results
ids <- createTimeSlices(1:370, initialWindow=60, horizon=14,
                               skip=60+14-1)
# ids$train alle 60, für test alle 14 passt
lapply(ids$test, length)

# get lambda vec
lambdas <- readRDS("results/CV-lasso/cv-lasso-08-11-21lambda-vec.rds")
err_mat <- readRDS("results/CV-lasso/cv-lasso-08-11-21err-mat.rds")
plot(err_mat[,5])
min(err_mat[,5])
err_mat[,5]
err_mat[67,5]
lambdas[67]

# now fit a model with the lambda from the best performing model
ids$train$Training356
dim(features_cv)
mod <- glmnet(features_cv[ids$train$Training356,], target_cv[ids$train$Training356],
              lambda = lambdas[67])
nonzero <- coeffs[,1] != 0
coef_names <- names(coeffs[nonzero,1])
all_coef_names <- names(coef(mod)[,1])

# TODO how to plot that
# raster object with dim of old sst
# must contain NA's for land
# 0 for any zero coefficient
# idea: fill original old_sst vector with values from coefficient
# or get NAs from old_sst
co <- coordinates(old_sst)
names(coeffs)

# these are NA in the original sst vector
# also all NA are the same locations, make sense

# idea get original vector, give that vector names according to coordinates
# then access the values with coord names and give either 0 or NA
# for testing we can also just plot points from coordinate names
v <- (vals[,1])
length(v)
names(v) <- paste(coordinates(old_sst)[,1], coordinates(old_sst)[,2])
all_coef <- coef(mod)[-1] #without intercept
all_coef_names <- names(coef(mod)[,1])[-1] #without intercept
v[all_coef_names] <- all_coef
length(v)
is.vector(v)

grid <- matrix(v, nrow = old_sst@nrows, ncol = old_sst@ncols, byrow = TRUE)
test_raster <- raster(grid, xmn = old_sst@extent@xmin, xmx = old_sst@extent@xmax,
                     ymn = old_sst@extent@ymin, ymx = old_sst@extent@ymax,
                     crs = old_sst@crs)
df <- cbind(xyFromCell(test_raster, 1:ncell(test_raster)), values(test_raster))
df <- base::as.data.frame(df)
colnames(df) <- c("Longitude","Latitude", "Vals")
plt <- ggplot(data = df, aes(x = Longitude, y = Latitude, fill = Vals)) +
  annotation_map(map_data("world")) +
  geom_raster(interpolate = TRUE)
plt
max(df, na.rm = TRUE)
