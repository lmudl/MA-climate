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
save_to <- "cv-lasso-og-timelag-25-06-22-rm4"
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

sst_cv <- add_ts_vars(sst_cv)
cc <- complete.cases(sst_cv)
sst_cv <- sst_cv[cc,]
precip_cv <- precip_cv[cc]

sst_eval <- add_ts_vars(sst_eval)
cc <- complete.cases(sst_eval)
sst_eval <- sst_eval[cc,]
precip_eval <- precip_eval[cc]


# fit full model
full_model <- glmnet(data.matrix(sst_cv), precip_cv, lambda=l_min,
                     standardize=FALSE)
saveRDS(full_model, paste0(full_save_to, "full-model.rds"))
# predict on evaluation data
preds <- c(predict(full_model, newx = data.matrix(sst_eval)))

# plot predictions against true data
ids_cheat <- 374:432
df <- data.frame(predictions = preds, targets = precip_eval, ids=ids_cheat)
# plt <- ggplot() + geom_line(data=df, mapping= aes(x=seq(length(predictions)), 
#                                                   y=predictions, col = "red")) +
#   geom_line(data=df, mapping=aes(x=seq(lengths(predictions)), y=targets)) 
plt <- plot_predictions(df)
saveRDS(plt, paste0(full_save_to,"pred-plots/pred-plot-full.rds"))

# get mse on evaluation data set
comp_mse(preds, precip_eval) # 1326.809


