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
save_to <- "cv-lasso-diff-1"
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
# ndiffs <- apply(sst_cv, 2, unitroot_ndiffs)
# max_ndiffs <- max(ndiffs)
max_ndiffs <- 1
# 
# nr <- nrow(sst_cv)
# time_ind <- seq(nr)
# fac_month <- time_ind %% 12
# fac_month[fac_month == 0] <- 12
# fac_month <- as.factor(fac_month)

sst_cv <- apply(sst_cv, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
# 
# time_ind <- time_ind[-c(seq(max_ndiffs))]
# fac_month <- fac_month[-c(seq(max_ndiffs))]
# sst_cv <- cbind(sst_cv, time_ind, fac_month)
# sst_vc <- data.matrix(sst_cv)

precip_cv <- precip_cv[-c(seq(max_ndiffs))]

# time_ind2 <- seq(nr+1,nr+nrow(sst_eval))
# fac_month2 <- time_ind2 %% 12
# fac_month2[fac_month2 == 0] <- 12
# fac_month2 <- as.factor(fac_month2)

sst_eval <- apply(sst_eval, 2, function(x) diff(x, lag=1, difference=max_ndiffs))

# time_ind2 <- time_ind2[-c(seq(max_ndiffs))]
# fac_month2 <- fac_month2[-c(seq(max_ndiffs))]
# sst_eval <- cbind(sst_eval, time_ind2, fac_month2)

precip_eval <- precip_eval[-c(seq(max_ndiffs))]


# fit full model
# l_min = exp(0)
# take smallest lambda that is in 1se of lambda min
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
comp_mse(preds, precip_eval) # 

# TESTING
full_model2 <- glmnet(sst_cv, precip_cv, lambda=lambdas,
                      standardize=FALSE, standardize.response = FALSE)
preds2 <- predict(full_model2, newx = sst_eval)
errors2 <- apply(preds2, 2, function(x) comp_mse(x, precip_eval))
wm2 <- which.min(errors2)
df2 <- data.frame(predictions = preds2[,wm2], targets = precip_eval)
plt2 <- ggplot() + geom_line(data=df2, mapping= aes(x=seq(length(predictions)), y=predictions, col = "red")) +
  geom_line(data=df, mapping=aes(x=seq(lengths(predictions)), y=targets))
plt2
plot(ts(errors2))
l_min_id
wm2

