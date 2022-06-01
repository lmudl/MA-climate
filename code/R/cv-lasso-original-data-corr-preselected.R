# cv-lass-original-data-corr-preselected
getwd()
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
library(caret)
library(glmnet)

# # keep this for rtsa_deseasonalised!!
# a <- mask(features[[1]], mask = corr_raster, maskvalue = NA, updatevalue = NA)
# raster(features)

# analysis with correlation selection before now #####
target_path <- "data/interim/drought/chirps_setreftime_aggregated.rds"
features_path <- "data/interim/sst/ersst_setreftime.nc"
target <- load_data(target_path)
target <- values(target)
#target <- apply(target, 2, mean)
features <- load_data(features_path, "sst")
features <- add_colnames(features_path, features)
#features <- prepare_sst(features)
dim(target)
dim(features)
# get correlations and the significant ones only, create mask to set rest NA
cvec <- compute_corr(t(features), target, timelag = 0, cor_method = "pearson")
#cvec2 <- compute_corr(features, target, timelag = 0, cor_method = "spearman")
plot(hist(cvec))
#plot(hist(cvec2))
old_sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
# could create raster from correlation vector IMPORTANT!
# then get the coordinates that have non sig corr NA
# then add that raster as layers, drop rows that contain
# all NA, or any NA in one row, from sst
corr_grid <- matrix(cvec, nrow = old_sst@nrows, ncol = old_sst@ncols, byrow = TRUE)
corr_raster <- raster(corr_grid,
                      xmn = old_sst@extent@xmin, xmx = old_sst@extent@xmax,
                      ymn = old_sst@extent@ymin, ymx = old_sst@extent@ymax,
                      crs = old_sst@crs)
plot(corr_raster)
q <- raster::quantile(corr_raster, probs = c(0.025, 0.975), na.rm = TRUE)
setNA <- corr_raster > q[1] & corr_raster < q[2]
table(getValues(setNA))
corr_raster[setNA] <- NA
plot(corr_raster)
setNA_vec <- getValues(setNA)

features[,setNA_vec] <- NA
# maybe we can leave this out?
features <- prepare_sst(features)
features_cv4 <- features[1:370,]
target_cv4 <- as.data.frame(target)[,1:370]
target_cv4 <- unname(apply(target_cv4, 2, mean))

lasso_og_corr_pre <- cv_for_ts(features_cv4, target_cv4, nfold = 5, size_train = 60, size_test = 14,
                               save_folder = "cv-lasso-original-corr-pre")
# plot results
ids <- createTimeSlices(1:370, initialWindow=60, horizon=14,
                        skip=60+14-1)
lambdas <- readRDS("results/CV-lasso/cv-lasso-original-corr-pre/lambda-vec.rds")
err_mat <- readRDS("results/CV-lasso/cv-lasso-original-corr-pre/err-mat.rds")
plot_and_save_cv_results(err_mat, 5, ids, lambdas, features_cv4, target_cv4,
                         save_to = "results/CV-lasso/cv-lasso-original-corr-pre")






