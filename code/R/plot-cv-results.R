# plot cv results
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(caret)

# load data from cv-for-ts before running this code
# need features_cv and target_cv

# Plot results
ids <- createTimeSlices(1:370, initialWindow=60, horizon=14,
                        skip=60+14-1)
# ids$train alle 60, für test alle 14 passt
lapply(ids$train, length)
lapply(ids$test, length)

# get lambda vec
lambdas <- readRDS("results/CV-lasso/cv-lasso-08-11-21lambda-vec.rds")
err_mat <- readRDS("results/CV-lasso/cv-lasso-08-11-21err-mat.rds")


# now fit a model with the lambda from the best performing model
ids$train$Training356
dim(features_cv)
mod <- glmnet(features_cv[ids$train$Training356,], target_cv[ids$train$Training356],
              lambda = lambdas[67])
coeffs <- coef(mod)
nonzero <- coeffs[,1] != 0
coef_names <- names(coeffs[nonzero,1])
coef_names <- coef_names[-1] #w/o intercept
all_coef_names <- names(coef(mod)[,1])

# for fold 1
# which lambda is min for fold
fold <- 1
plot(err_mat[,fold])
id_min <- which.min(err_mat[,fold])
# fit model
id_fold <- ids$train$Training060
lambda_fold <- lambdas[id_min]
target_cv <- t(as.matrix(target_cv))
mod_fold <- glmnet(features_cv[id_fold,], target_cv[id_fold,],
                   lambda = lambda_fold)
# get coef
# drop intercept directly
all_coef <- coef(mod_fold)[-1,1]
nonzero_coef <- all_coef != 0
nonzero_coef_names <- names(all_coef[nonzero_coef])

# transform coefficient names
num_coef_names <- coef_names_to_numeric(nonzero_coef_names)
coef_mat <- cbind(num_coef_names, all_coef[nonzero_coef])

# plot nonzero coefficients
plot_nonzero_coefficients(coef_mat)



plot_nonzero_from_fold <- function(error_matrix, fold, cv_ids, lambdas,
                                   feature_data, target_data) {
  id_min <- which.min(error_matrix[,fold])
  ids <- cv_ids$train[[fold]]
  min_lambda <- lambdas[id_min]
  # watch out for target dimensions and that feature_data
  # is prepared f.e via prepare_sst
  mod <- glmnet(feature_data[ids,], target_data[ids],
                lambda = min_lambda)
  all_coef <- coef(mod)[-1,1]
  nonzero_coef <- all_coef != 0
  nonzero_coef_names <- names(all_coef[nonzero_coef])
  num_coef_names <- coef_names_to_numeric(nonzero_coef_names)
  coef_mat <- cbind(num_coef_names, all_coef[nonzero_coef])
  plt <- plot_nonzero_coefficients(coef_mat)
  return(plt)
}

coef_names_to_numeric <- function(coefficient_names) {
  a <- unlist(strsplit(coefficient_names, " "))
  b <- as.numeric(a)
  c <- matrix(b, ncol = 2, byrow = TRUE)
  return(c)
}

plot_nonzero_coefficients <- function(nonzero_coef) {
  # Using GGPLOT, plot the Base World Map
  mp <- NULL
  mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
  mp <- ggplot() + mapWorld
  
  #vNow Layer the cities on top
  mp <- mp + geom_point(aes(x=nonzero_coef[,1], y=nonzero_coef[,2], 
                            colour = nonzero_coef[,3]), size=3) + scale_colour_gradient2()
  mp
}

plt_fold1 <- plot_nonzero_from_fold(error_matrix = err_mat, fold = 1, 
                                    cv_ids = ids, lambdas = lambdas,
                                    feature_data = features_cv,
                                    target_data = target_cv)

plt_fold2 <- plot_nonzero_from_fold(error_matrix = err_mat, fold = 2, 
                                    cv_ids = ids, lambdas = lambdas,
                                    feature_data = features_cv,
                                    target_data = target_cv)

plt_fold3 <- plot_nonzero_from_fold(error_matrix = err_mat, fold = 3, 
                                    cv_ids = ids, lambdas = lambdas,
                                    feature_data = features_cv,
                                    target_data = target_cv)

plt_fold4 <- plot_nonzero_from_fold(error_matrix = err_mat, fold = 4, 
                                    cv_ids = ids, lambdas = lambdas,
                                    feature_data = features_cv,
                                    target_data = target_cv)

plt_fold5 <- plot_nonzero_from_fold(error_matrix = err_mat, fold = 5, 
                                    cv_ids = ids, lambdas = lambdas,
                                    feature_data = features_cv,
                                    target_data = target_cv)



debug(plot_nonzero_from_fold)

# TODO how to plot that
# raster object with dim of old sst
# must contain NA's for land
# 0 for any zero coefficient
# idea: fill original old_sst vector with values from coefficient
# or get NAs from old_sst
old_sst <-  brick("data/interim/sst/ersst_setreftime.nc",
                  varname= "sst")
co <- coordinates(old_sst)

# these are NA in the original sst vector
# also all NA are the same locations, make sense

# idea get original vector, give that vector names according to coordinates
# then access the values with coord names and give either 0 or NA
# for testing we can also just plot points from coordinate names
vals <- values(old_sst)
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
  geom_raster(interpolate = TRUE) +
  scale_fill_gradient2()

plt
max(df, na.rm = TRUE)
range(all_coef)
plot(density(all_coef))

# TODO find solution for colouring and summarise
# plotting in function

# TODO maybe just plot points and next to the points the
# values
# see plotfunction for plot random  points, yes funktioniert

plot_cell(as.matrix(df[,1:2]))
sst_cell
plot_cell(df[1,c(1,2)])
debug(plot_cell)
coef_names[1]
te <- matrix(as.numeric(unlist(strsplit(all_coef_names[1:2], " "))), ncol = 2, byrow = TRUE)
te[2,1] <- -160
plot_cell(te)

coef_names_to_numeric <- function(coefficient_names) {
  a <- unlist(strsplit(coefficient_names, " "))
  b <- as.numeric(a)
  c <- matrix(b, ncol = 2, byrow = TRUE)
  return(c)
}

# # if we use plot cell we only plot coefficient points
te <- coef_names_to_numeric(coef_names)
plot_cell(te)

te <- cbind(te, coeffs[nonzero][-1])

# https://stackoverflow.com/questions/43148657/ggplot2-geom-point-size-color-gradient
plot_nonzero_coefficients <- function(nonzero_coef) {
  # Using GGPLOT, plot the Base World Map
  mp <- NULL
  mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
  mp <- ggplot() + mapWorld
  
  #vNow Layer the cities on top
  mp <- mp + geom_point(aes(x=nonzero_coef[,1], y=nonzero_coef[,2], 
                            colour = nonzero_coef[,3]), size=3) + scale_colour_gradient2()
  mp
}

plot_nonzero_coefficients(te)
# but you can not see how large their influence is


# turn fit from error matrix in one function
err_mat




