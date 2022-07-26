setwd("Repos/MA-climate/")
library(raster)
library(glmnet)
library(ggplot2)
source("code/R/helper-functions.R")
# evaluate lasso full
# get best overall lambda from err_mat
# refit lambda model with best lambda
# predict on evaluation data
# compute mse and rmse
# plot predictions etc

# path to and save_to
save_to <- "noclust-lasso-stand"
full_save_to <- paste0("results/CV-lasso/",save_to,"/")

# load err mat, lambdas and get lambda min
err_mat <- readRDS(paste0(full_save_to,"err-mat.rds"))
lambdas <- readRDS(paste0(full_save_to,"lambda-vec.rds"))
ids <- readRDS(paste0(full_save_to,"index-list.rds"))

l_min_id <- which.min(apply(err_mat, 1, mean))
l_min <- lambdas[l_min_id]
#l_min <- exp(-1.75)

# load data for fitting and evaluating full model
sst_cv <- readRDS("data/processed/noclust_sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")
sst_eval <- readRDS("data/processed/noclust_sst_eval.rds")
precip_eval <- readRDS("data/processed/precip_eval.rds")


sst_cv <- standardize_train(sst_cv)
sst_eval <- standardize_test(sst_cv, sst_eval)

# fit full model
full_model <- glmnet(sst_cv, precip_cv, lambda=l_min,
                     standardize=FALSE)
# save full model
saveRDS(full_model, paste0(full_save_to, "full-model.rds"))

# predict on evaluation data
preds <- c(predict(full_model, newx = sst_eval))
# plot predictions against true data
ms_start <- nrow(sst_cv)+1
ms_end <- nrow(sst_cv)+nrow(sst_eval)
df <- data.frame(predictions = preds, targets = precip_eval, ids=c(ms_start:ms_end))
plt <- plot_predictions(df)
saveRDS(plt, paste0(full_save_to,"pred-plots/pred-plot-full.rds"))

# get mse on evaluation data set
comp_mse(preds, precip_eval) # 2290.062

# TESTING
# fm <- glmnet(sst_cv, precip_cv, standardize = FALSE)
# preds <- predict(fm, newx = sst_eval)
# dim(preds)
# plot(ts(preds[,64]))
# errs <- apply(preds, 2, function(x) comp_mse(x, precip_eval))
# plot(errs)
# 
# l_min
# l_min_id
# which.min(errs)
# errs[60:80]
