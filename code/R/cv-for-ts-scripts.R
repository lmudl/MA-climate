# do analysis for different data sets and combinations
getwd()
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
library(caret)
# analysis of deseasonalised data already done see cv-lasso-08-11-21 ####
# done like in cv-for-ts

# analysis of original data alread done see cv-lasso-12-05 ####
# done in regress
target_path <- "data/interim/drought/chirps_setreftime_aggregated.rds"
features_path <- "data/interim/sst/ersst_setreftime.nc"
target <- load_data(target_path)
target <- overlay(target, fun = mean)
features <- load_data(features_path, "sst")
features_cv2 <- features[,1:370]
target_cv2 <- target[1:370]
lasso_on_og_data <- cv_for_ts(features_cv2, target_cv2, nfold = 5, size_train = 60, size_test = 14,
                 save_folder = "cv-lasso-og-data-12-05-22")
# plot results
ids <- createTimeSlices(1:370, initialWindow=60, horizon=14,
                        skip=60+14-1)
lambdas <- readRDS("results/CV-lasso/cv-lasso-og-data-12-05-22/lambda-vec.rds")
err_mat <- readRDS("results/CV-lasso/cv-lasso-og-data-12-05-22/err-mat.rds")

features_cv2 <- add_colnames(features_path, features_cv2)
features_cv2 <- prepare_sst(features_cv2)
plot_and_save_cv_results(err_mat, 5, ids, lambdas, features_cv2, target_cv2,
                         save_to = "results/CV-lasso/cv-lasso-og-data-12-05-22")

# analysis with correlation selection before now #####
target_path <- "data/processed/deseasonalised_precip.rds"
target <- load_data(target_path)
features_path <- "data/processed/deseasonalised_sst.rds"
features <- load_data(features_path)

cvec <- compute_corr(features, target, timelag = 0, cor_method = "pearson")
debug(compute_corr)
head()

sst_og <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
p <- plot_corr(cvec, 0, old_sst=sst_og, quantiles = TRUE)
p$data[p$data$Correlation!=0,]
nosigs <- p$data$Correlation==0
p$data[!nosigs,]

# idea get non-null correlation and only include these in the correlation
# maybe keep for correlation the grid and then do 
# or easier 
d <- cbind(coordinates(sst_og), p$data$Correlation)
head(d)
d <- na.omit(d)
dim(d)
d <- as.data.frame(d)
d$loc <- paste(d$x,d$y)
names(d)[3] <- "corr"
d <- d[d$corr!=0,] 
d <- as.data.frame(d)
dim(d)

dim(features)
features <- add_colnames("data/interim/sst/ersst_setreftime.nc", sst = features)
features <- prepare_sst(features)

sigs <- colnames(features) %in% d$loc
features <- features[,sigs]

features_cv3 <- features[1:370,]
target_cv3 <- as.data.frame(target)[1:370,]
target_cv3 <- unname(apply(target_cv3, 2, mean))
lasso_on_corr_des <- cv_for_ts(features_cv3, target_cv3, nfold = 5, size_train = 60, size_test = 14,
                              save_folder = "cv-lasso-on-corr-des")
debug(get_lambda_values)
debug(cv_for_ts)
