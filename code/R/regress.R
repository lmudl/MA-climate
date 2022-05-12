
# set working directory
setwd("Repos/MA-climate")

# load packages
library(raster)
library(caret)
library(glmnet)
# load helper functions
source("code/R/helper-functions.R")

# regress
# get data frame from sst with
# time, month of the year, location(latlon)
# but lat and long could also be 2 variables and rain could be a function
# of these two as well.
library(tidyr)
library(dplyr)
# play ########
test <- as.data.frame(head(features)[1:4,1:5])
names(test) <- as.character(names(test))
test$time <- 1:nrow(test)
test$cyc_month <- seq(1,nrow(test))
test %>% tidyr::pivot_longer(names_to = "loc", values_to = "sst")
?pivot_longer

class(features)
dim(features)
test <- as.data.frame(features)
dim(test)
test$time <- 1:432
test$cyc_mon <- rep(1:12,length.out=432)
head(test)[1:15,10985:10990]
ttest <- test[1:10,10985:10990]
n <- names(ttest)[-c(ncol(ttest),ncol(ttest)-1)]
ttest %>% pivot_longer(n, names_to = "loc", values_to = "sst") -> ttest
ttest$loc <- factor(ttest$loc)

dim(test)
tail(names(test))

n <- rev(rev(names(test))[-(1:2)])

dim(test)

test %>% pivot_longer(n, names_to = "loc", values_to = "sst") -> test

dim(test)
test$cyc_mon <- factor(test$cyc_mon)
head(test)

features <- as.data.frame(test)

# also cyc_month und time haben keinen einfluss im deseasonalised model ####
# scheint so jedenfalls

# run cv new on original data with seasonalities
target_path <- "data/interim/drought/chirps_setreftime_aggregated.rds"
target <- load_target(target_path)
features_path <- "data/interim/sst/ersst_setreftime.nc"
features <- brick(features_path, varname = "sst")
dim(features)
features <- features[[train_ind]]
features <- raster::as.data.frame(features, xy=TRUE, long = TRUE)
dim(features)
head(features)
colnames(features)

features$loc <- paste(features$x, features$y)
features$Z <- as.numeric(stringr::str_replace(features$layer, "X", ""))
features$Z <- features$Z - min(features$Z) + 1
features$Z <- as.integer(features$Z)
head(features)
features$cyc_mon <- unique(features$Z) %% 12
zeros <- which(features$cyc_mon == 0)
features$cyc_mon[zeros] <- 12
features$loc <- factor(features$loc)
head(features)

target <- getValues(target)
target <- apply(target, 2, mean)
target <- unname(target)
target <- target[1:max(features$Z)]
target <- as.data.frame(target)
names(target) <- "precip"
target$Z <- 1:max(features$Z)

aha <- merge(target, features, "Z")
aha <- na.omit(aha)
head(aha)

target <- aha$precip
features <- aha[,-2]
head(features)
length(target)
length(target)

modx <- glmnet(features[,c(1,5,6,7)], target)
plot(modx)
coefficients(modx)

# like this loc has probably no information because it is not really
# informative to have all these variables inside
# next steps: ####
#1. fit on original data, all locations as variables and add seasonality and time overall 
#2. fit on original data, all locations as variables, no seasonality no time overall
#1 and 2 can be done in one go
# fit on deseasonalised data, all locations as variables and add seasonality and time overall

# 1. #####
target_path <- "data/interim/drought/chirps_setreftime_aggregated.rds"
target <- load_target(target_path)
target <- raster::as.data.frame(target)
target <- apply(target, 2, mean)
plot(density(target))
plot(ts(target))

features_path <- "data/interim/sst/ersst_setreftime.nc"
features <- brick(features_path, varname = "sst")
coords <- coordinates(features)
cnames <- paste(coords[,1], coords[,2])
features <- as.data.frame(features) #to merge layers
features <- t(features)
features <- as.data.frame(features) #to get df, after t() is matrix, no colnames really
colnames(features) <- cnames
dim(features)
features$time <- 1:nrow(features)
features$cyc_mon <- features$time %% 12
zeros <- which(features$cyc_mon == 0)
features$cyc_mon[zeros] <- 12
features$cyc_mon <- as.factor(features$cyc_mon)
# load prepare_sst from helper-functions
features <- prepare_sst(features)

modx <- glmnet(features, target)
plot(modx)
print(modx)
plot(print(modx)[,2])
dim(coef(modx))
coef(modx)[-1,] != 0
rownames(coef(modx))[coef(modx)[,1]!= 0] ### returns nonzero coefs
cm <- coef(modx)
str(cm)

cm_nonzero_log <- apply(cm, 1, function(x) (sum(x!=0)!=0))
View((cm[cm_nonzero_log,]))

features_cv2 <- features[1:370,]
target_cv2 <- as.data.frame(target)[1:370,]
ex2 <- cv_for_ts(features_cv2, target_cv2, nfold = 5, size_train = 60, size_test = 14,
                 save_folder = "cv-lasso-og-data-12-05-22")
ex2

# plotcv results
ids <- createTimeSlices(1:370, initialWindow=60, horizon=14,
                        skip=60+14-1)
lambdas <- readRDS("results/CV-lasso/cv-lasso-og-data-12-05-22/lambda-vec.rds")
err_mat <- readRDS("results/CV-lasso/cv-lasso-og-data-12-05-22/err-mat.rds")
plot(err_mat[,1])
min(err_mat[,5])

err_mat2 <- readRDS("results/CV-lasso/cv-lasso-08-11-21err-mat.rds")
plot(err_mat2[,5])
min(err_mat2[,5])

plt_fold5 <- plot_nonzero_from_fold(error_matrix = err_mat, fold = 5, 
                                    cv_ids = ids, lambdas = lambdas,
                                    feature_data = features_cv2,
                                    target_data = target_cv2)
plt_fold1


## helpers for loading data
# check if rds or nc
library(stringr)
"rds" %in% target_path 
stringr::str_detect(target_path, ".rds")
stringr::str_detect(target_path, ".nc")

load_data <- function(data_path, varname) {
  isrds <- stringr::str_detect(data_path, ".rds")
  isnc <- stringr::str_detect(data_path, ".nc")
  if(isrds) data_loaded <- readRDS(data_path)
  if(isnc) {
    data_loaded <- brick(data_path, varname = varname)
    data_loaded <- as.data.frame(data_loaded)
  }
  if(!(isrds | isnc)) stop("data should be either rds or nc")
  return(data_loaded)
}

