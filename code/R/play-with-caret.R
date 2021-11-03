# Attempt to create Cross Validation method for
# regression on time series data with the preselected
# variables for lm/ lasso/ elastic net/ fused lasso
library(raster)
library(caret)
library(glmnet)
#?createTimeSlices?

setwd("Repos/MA-climate")
# Load datasetss
sst <- readRDS("data/processed/deseasonalised_sst.rds")
dim(sst) # 16020 432
precip <- readRDS("data/processed/deseasonalised_precip.rds")
dim(precip) # 61200 432
# Compute mean of precip
precip <- apply(precip, 2, mean)
precip <- matrix(precip)
# TODO think about which prediction windows etc make sense
#      also which forms of CV make sense
#   we have 432 months of data if we go for 5 fold CV
#   we have 432/5 = 86.4 months of test and
#   86.4*4 and 345.6 of training
#   but in time series framework we need, to take 
#   time dependency into account. So we might go
#   for 5 models, each fit with 74 train and 12 test
#   fold 1: month 1 until 86
#   fold 2_ month 87 until 173
#   etc..
#   in caret package we would have initial window=74
#   fixedwindow = TRUE and horizon = 12.
#   But how should we account for that we only want
#   non overlapping data?
# TODO write helper function for this or use caret
#   will return the indices for test and train
################################################################################
# Summary
# we can easily use caret package to do CV as wanted
# no reshape needed apparently
# transpose sst
# give columns names
# drop NA coordinates
# just use the matrix of sst and precip mean vector
# fit glmnet
# inspect deviance explained
# predict can easily be done make sure that NA's are
# also dropped for new x
#

# TODO use not precip but drought index as
# target
# TODO how is drught index defined
# TODO how to compute drought index
# TODO should we first compute drought index and then
# mean or other way around?
# in paper they compute first drought index and then
# the mean
# Find out values of precip
# general units: mm per month, we then averages values
min(precip) #95
max(precip) #285

add_colnames <- function(path_old_sst, sst) {
  old_sst <- brick(path_old_sst, varname = "sst")
  coord <- coordinates(old_sst)
  sst <- t(sst)
  assertthat::assert_that(dim(coord)[1] == dim(sst)[2])
  # give sst columns the coordinates as names
  cnames <- paste(coord[,1], coord[,2])
  colnames(sst) <- cnames
  return(sst)
}

#sst <- t(sst)
sst <- add_colnames("data/interim/sst/ersst_setreftime.nc", sst)
# For testing, smaller set
# sst <- sst[1:60,]
# precip <- precip[1:60]
# maybe exclude skip and make sure that there is no
# overlap
# Or if overlap = FALSE, then skip is function
# maybe later add functionality for overlap TRUE/FALSE
# for now we always say no overlap
# initialwindow+horizon-1
# if overlap = FALSE, skip is ignpred
# type could be lasso, fusedlasso etc
# but for these we can write own helper functions
# like cv_for_ts_lasso etc
# fixedWindow is always TRUE here
# so possible add-on, decide if overlap yes/no
# and which type of regression mode should we use
# like lasso or fused lasso etc.
# TODO when testing this also remember to leave some
# observations for validation set.
cv_for_ts <- function(sst, precip, nfold, size_train, size_test) {
  set.seed(1234)
  #TODO aswer question:compute precip mean here or before?
  sst <- prepare_sst(sst)
  assertthat::are_equal(nrow(sst), length(precip))
  n_row <- nrow(sst)
  obs_to_drop <- get_obs_to_drop(n_row, nfold)
  sst <- drop_obs(sst, obs_to_drop)
  precip <- drop_obs(precip, obs_to_drop)
  # now we made sure that nrow(data) %% nfold == 0
  assertthat::are_equal(size_train+size_test, nrow(sst)/nfold)
  index_list <- createTimeSlices(1:nrow(sst), initialWindow=size_train, horizon=size_test,
                                 skip=size_train+size_test-1)
  #TODO create list with glmnet objects so we can plot
  #their paths and coefficients on map
  #TODO safe index_list as well
  #TODO for each lambda
  #         for each fold
  #             fit model and predict on test, save err
  # now we have for each lambda 5 MSE
  # find best lambda
  # refit on all train+test data with lambda
  # predict on validation set and report error final
  # error final can then be compared among diff models
  lambda_vec <- get_lambda_values(sst, precip)
  err_mat <- matrix(NA, ncol = nfold, nrow = length(lambda_vec))
  for(i in 1:length(lambda_vec)) {
    for(j in 1:length(index_list$train)) {
      id_train <- unlist(index_list$train[j], use.names = FALSE)
      id_test <- unlist(index_list$test[j], use.names = FALSE)
      x_train <- sst[id_train,]
      y_train <- precip[id_train]
      x_test <- sst[id_test,]
      y_test <- precip[id_test]
      # die cross validaion is done here classically
      # does not work!!!
      trained_model <- glmnet(x_train, y_train)
      #TODO change value her for s
      predicted <- predict(trained_model, newx = x_test, s = lambda_vec[i])
      err <- mean((predicted-y_test)^2)
      err_mat[i,j] <- err
      #save(trained_model, file=paste0("results/CV-lasso/model-","lambda-",i,"fold-",j,".RData"))
    }
  }
  save(index_list, file="results/CV-lasso/index_list")
  save(lambda_vec, file="results/CV-lasso/lambda_vec")
  return(err_mat)
  # until here we keep the error for each fold
  # but with fixed regularisation
  #TODO add loop for different regularisation values
  #TODO add parameters for glmnet
  #TODO create function that computes number of observations
  #for each fold so that all windows have same number
  #of observations
  #make sure that nrow(data) %% nfold == 0
  #AND initialwindow+horizon == nrow(data)/nfold
  #TODO read about standardisation ?glmnet()
  
}

# function for dropping NA in sst
prepare_sst <- function(sst) {
  #transpose sst,rows are months, cols are coord after transposing
  #sst <- t(sst)
  #sst will be transposed before
  #drop sst info that contains NA
  old_dim <- dim(sst)
  drop <- apply(sst, 2, function(x) all(is.na(x)))
  sst <- sst[,!drop]
  new_dim <- dim(sst)
  if(old_dim[2] == new_dim[2]) message("No rows were dropped")
  return(sst)
}

# function for finding out which and how many rows to drop in the creaetimseslices
# function
get_obs_to_drop <- function(nrow, nfold) {
  if (nrow %% nfold != 0) {
    # fold is train+test
    obs_in_fold <- floor(nrow/nfold)
    obs_used <- nfold*obs_in_fold
    # drop difference in rows, drop first observations
    drop_n_first_obs <- nrow-obs_used
    return(drop_n_first_obs)
  }
}

# and then actually dropping the rows
drop_obs <- function(data, obs_to_drop) {
  if(!is.null(obs_to_drop)) {
    data <- data[-c(1:obs_to_drop),]
    return(data)
  }
  return(data)
}


# In this function we want to get the lambda values,
# that glmnet uses
# according to 
# https://stats.stackexchange.com/questions/174897/choosing-the-range-and-grid-density-for-regularization-parameter-in-lasso
# we have a formula for lambda_max 
# and min is then chosen from that lambda_max
# lambda_max is data derived meaning from the data
# we choose the lambda that forces all coefficients
# to be zero
# TODO find out how that works
# https://datascience.stackexchange.com/questions/48885/covariance-as-inner-product
# meaning compute all inner products and choose
# lambda_max as lambda_max  = max abs val(inner product)/N
get_lambda_values <- function(sst, precip){
  #TODO compute inner product with target
  # for all variables
  # target_vec %*% feature_matrix
  inner_prods <- precip %*% scale(sst, center = TRUE, scale = TRUE)
  #TODO get the max abs value
  max_inner <- max(abs(inner_prods))
  #TODO comput lambdamax
  max_lambda <- max_inner/nrow(sst)
  #TODO compute lambdamin
  min_lambda <- 0.001*max_lambda
  # TODO create vector with lambda values
  # create vector of lambdas
  # evenly spaced points on log scale between mx and mn
  lambda_vec <- exp(seq(log(min_lambda),log(max_lambda), length.out = 100))
  return(lambda_vec)
}

undebug(get_lambda_values)
l <- get_lambda_values(sst, precip)
?glmnet
glmnet(x=sst,y=precip,lambda = max(l))
ip <- precip%*%scale(sst, center = TRUE, scale = TRUE)
mx <- max(abs(ip))
mx <- mx/60

em <- cv_for_ts(sst=sst,precip=precip,nfold = 5,size_train = 68, size_test = 18)
em

#test cv_for_ts 
#but only do one fold
#namely say 432-60 last 5 years we dont want do use for test/train but for validation
#then we have 372, 372/5= 74.4 so 74 per fold and 60 train 14 test
#so 358-372 is test, 298-358 is train
train <- 298:358
test <- 359:372
sst <- add_colnames("data/interim/sst/ersst_setreftime.nc", sst)
sst <- prepare_sst(sst)
sst_train <- sst[train,]
sst_test<- sst[test,]
precip_train <- precip[train,]
precip_test <- precip[test,]

lambdas <- get_lambda_values(sst[c(train,test),], precip[c(train,test),])

# TODO
#I dont have to use two loops
#I can define get_lambdas and then for each fold 
#fit models with the whole lambda vector
#then for i in lambda pred

mod <- glmnet(sst, precip, lambda=rev(lambdas))
plot(mod)
coef(mod, )
predict

err <- c(NA)
for (i in 1:length(lambdas)) {
  pred <- predict(mod, newx = sst_test, s = lambdas[i])
  err_i <- sum((pred-precip_test)^2)
  err[i] <- err_i
}
plot(err)
p <- predict(mod,newx = sst_test, s=lambdas[1])

cbind(p,precip_test)
length(p)
length(precip_test)
#22:21 start
undebug(cv_for_ts)
#430/5

### with precip we need to drop number of observations before
# because computing the spi means we loose some observations
#TODO load rds object
spi_3 <- readRDS("data/processed/spi_3.rds")
dim(spi_3)
spi_3[1:3,1:5]
# name split after underscore, here 3, then -1 is number of
# rows to drop
# or just do it by function input
load_spi <- function(path_to_spi, spi_window) {
  spi <- readRDS(path_to_spi)
  drop <- spi_window-1
  spi <- spi[,-c(1:drop)]
  return(spi)
}

spi_3 <- load_spi("data/processed/spi_3.rds", 3)
dim(spi_3) # now only 432 before 430

#plan for cv

#Maybe###############################
# TODO give rows months and years. maybe

# function (x, sign.lambda = 1, ...) 
# {
#   cvobj = x
#   xlab = expression(Log(lambda))
#   if (sign.lambda < 0) 
#     xlab = paste("-", xlab, sep = "")
#   plot.args = list(x = sign.lambda * log(cvobj$lambda), y = cvobj$cvm, 
#                    ylim = range(cvobj$cvup, cvobj$cvlo), xlab = xlab, ylab = cvobj$name, 
#                    type = "n")
#   new.args = list(...)
#   if (length(new.args)) 
#     plot.args[names(new.args)] = new.args
#   do.call("plot", plot.args)
#   error.bars(sign.lambda * log(cvobj$lambda), cvobj$cvup, cvobj$cvlo, 
#              width = 0.01, col = "darkgrey")
#   points(sign.lambda * log(cvobj$lambda), cvobj$cvm, pch = 20, 
#          col = "red")
#   axis(side = 3, at = sign.lambda * log(cvobj$lambda), labels = paste(cvobj$nz), 
#        tick = FALSE, line = 0)
#   abline(v = sign.lambda * log(cvobj$lambda.min), lty = 3)
#   abline(v = sign.lambda * log(cvobj$lambda.1se), lty = 3)
#   invisible()
# }

glmnet::plot.cv.glmnet(em)
plot(em)
plot(em[,])



