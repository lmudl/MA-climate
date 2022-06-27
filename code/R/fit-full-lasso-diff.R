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
save_to <- "cv-lasso-og-diff"
full_save_to <- paste0("results/CV-lasso/",save_to,"/")

# load err mat, lambdas and get lambda min
err_mat <- readRDS(paste0(full_save_to,"err-mat.rds"))
lambdas <- readRDS(paste0(full_save_to,"lambda-vec.rds"))
ids <- readRDS(paste0(full_save_to,"index-list.rds"))

l_min_id <- which.min(apply(err_mat, 1, mean))
l_min <- lambdas[l_min_id]

# load data for fitting and evaluating full model
sst_cv <- readRDS("data/processed/sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")

sst_eval <- readRDS("data/processed/sst_eval.rds")
precip_eval <- readRDS("data/processed/precip_eval.rds")

# prepare with ndiffs
ndiffs <- apply(sst_cv, 2, unitroot_ndiffs)
max_ndiffs <- max(ndiffs)
sst_cv <- apply(sst_cv, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
precip_cv <- precip_cv[-c(seq(max_ndiffs))]

sst_eval <- apply(sst_eval, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
precip_eval <- precip_eval[-c(seq(max_ndiffs))]


# fit full model
full_model <- glmnet(data.matrix(sst_cv), precip_cv, lambda=l_min,
                     standardize=FALSE)
saveRDS(full_model, paste0(full_save_to, "full-model.rds"))
# predict on evaluation data
preds <- c(predict(full_model, newx = data.matrix(sst_eval)))

# plot predictions against true data
ids_cheat <- c((371+max_ndiffs):432)
df <- data.frame(predictions = preds, targets = precip_eval, ids=ids_cheat)
# plt <- ggplot() + geom_line(data=df, mapping= aes(x=seq(length(predictions)), 
#                                                   y=predictions, col = "red")) +
#   geom_line(data=df, mapping=aes(x=seq(lengths(predictions)), y=targets)) 
plt <- plot_predictions(df)
saveRDS(plt, paste0(full_save_to,"pred-plots/pred-plot-full.rds"))

# get mse on evaluation data set
comp_mse(preds, precip_eval) # 1326.809

