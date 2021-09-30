# Attempt to create Cross Validation method for
# regression on time series data with the preselected
# variables for lm/ lasso/ elastic net/ fused lasso
library(raster)
library(caret)
library(glmnet)
?createTimeSlices

setwd("Repos/MA-climate")
# TODO load datasets
m_sst_des <- readRDS("data/processed/deseasonalised_sst.rds")
dim(m_sst_des) # 16020 432
m_precip_des <- readRDS("data/processed/deseasonalised_precip.rds")
dim(m_precip_des) # 61200 432
# TODO compute mean of precip
mean_precip <- apply(m_precip_des, 2, mean)
rm(m_precip_des)
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
createTimeSlices(17:32,5,3, skip = 7)
#   if we want non-overlapping fold then skip
#   should be initialwindow+horizon-1
createTimeSlices(17:32,5,3, skip = 7)
ind <- createTimeSlices(1:20,4,2, skip = 5)
#   check for any overlaps
any(duplicated(unlist(ind, use.names = FALSE)))
#   no overlaps
#   for our data, 430 works out equally for all months
#   meaning we drop the first two months of observations
#   for example
# TODO try if  partition works
test1 <- createTimeSlices(1:430,74, 12, skip=(74+12-1))
# TODO check data form needed aka data frame or the like
# load old dataset to get coordinate names for variables
old_sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
coord <- coordinates(old_sst)
dim(coord)[1] == dim(m_sst_des)[1]
# TODO give sst columns the coordinates as names
cnames <- paste(coord[,1], coord[,2])
# rows are months, cols are coord after transposing
sst <- t(m_sst_des)
rm(m_sst_des)
colnames(sst) <- cnames
dim(sst)
# TODO fit lasso on small data set
# TODO prepare data for lasso
# TODO use only 5 years of data
sst_test <- sst[1:60,]
precip_test <- mean_precip[1:60]
drop <- apply(sst_test, 2, function(x) all(is.na(x)))
sst_test <- sst_test[,!drop]
dim(sst_test)
fit_test <- glmnet(sst_test, precip_test)
plot(fit_test)
print(fit_test)
nx <- sst[61:72,]
dim(nx)
nx <- nx[,!drop]
dim(nx)

p <- predict(fit_test, newx = nx, s=0.1790)
m <- mean_precip[61:72]
sum((p-m)^2)/12
cbind(p,m)

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

# TODO find out values of precip
# general units: mm per month, we then averages values
min(mean_precip) #95
max(mean_precip) #285

# TODO write function for CV
?createTimeSlices
# make sure that sst data has colnames before starting
# with CV!

add_colnames <- function(path_old_sst, sst) {
  old_sst <- brick(path_old_sst, varname = "sst")
  coord <- coordinates(old_sst)
  assertthat::assert_that(dim(coord)[1] == dim(sst)[1])
  # give sst columns the coordinates as names
  cnames <- paste(coord[,1], coord[,2])
  
}

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
#TODO when testing this also remember to leave some
#observations for validation set.
cv_for_ts <- function(sst, precip, nfold, initialWindow, horizon) {
  set.seed(1234)
  #TODO aswer question:compute precip mean here or before?
  sst <- prepare_sst(sst)
  obs_to_drop <- get_obs_to_drop(nrow, nfold)
  sst <- drop_obs(sst, obs_to_drop)
  precip <- drop_obs(precip, obs_to_drop)
  # now we made sure that nrow(data) %% nfold == 0
  assertthat::assert_that(initialWindow+horizon==nrow(sst)/nfold)
  index_list <- createTimeSlices(1:nrow(sst), initialWindow, horizon,
                                 skip=initialwindow+horizon-1)
  err_vec <- c()
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
  for(i in 1:length(index_list$train)) {
    id_train <- unlist(index_list$train[i], use.names = FALSE)
    id_test <- unlist(index_list$test[i], use.names = FALSE)
    x_train <- sst[id_train,]
    y_train <- precip[id_train,]
    x_test <-  sst[id_test,]
    y_test <- precip[id_test,]
    # die cross validaion is done here classically
    # does not work!!!
    trained_model <- glmnet(x_train, y_train)
    #TODO change value her for s
    predicted <- predict(trained_model, newx = x_test, s = 0.17)
    err <- mean((predicted-y_test)^2)
    err_vec[i] <- err
    save(trained_model, file=paste0("results/CV-lasso/model",i,".RData"))
    save(index_list, file="results/CV-lasso/index_list")
  }
  return(err_vec)
  # until here we keep the error for each fold
  # but with fixed regularisation
  #TODO add loop for different regularisation values
  #TODO add parameters for glmnet
  #TODO create function that computes number of observations
  #for each fold so that all windows have same number
  #of observations
  #make sure that nrow(data) %% nfold == 0
  #AND initialwindow+horizon == nrow(data)/nfold
  
}

prepare_sst <- function(sst) {
  #transpose sst,rows are months, cols are coord after transposing
  sst <- t(sst)
  #drop sst info that contains NA
  drop <- apply(sst, 2, function(x) all(is.na(x)))
  sst <- sst[,!drop]
  return(sst)
}

get_obs_to_drop <- function(nrow, nfold) {
  if (nrow %% nfold != 0) {
    obs_used <- floor(nrow/nfold)
    # drop difference in rows, drop first observations
    drop_n_first_obs <- nrow-obs_used
    return(drop_n_first_obs)
  }
}

drop_obs <- function(data, obs_to_drop) {
  data <- data[-c(1:obs_to_drop),]
}


#Maybe
# TODO give rows months and years. maybe


