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
save_to <- "cv-lasso-standardize2"
full_save_to <- paste0("results/CV-lasso/",save_to,"/")

# load err mat, lambdas and get lambda min
err_mat <- readRDS(paste0(full_save_to,"err-mat.rds"))
lambdas <- readRDS(paste0(full_save_to,"lambda-vec.rds"))
ids <- readRDS(paste0(full_save_to,"index-list.rds"))

l_min_id <- which.min(apply(err_mat, 1, mean))
l_min <- lambdas[l_min_id]
#l_min <- exp(-1.75)

# load data for fitting and evaluating full model
sst_cv <- readRDS("data/processed/sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")

# mean_sst_cv <- apply(sst_cv,2,mean)
# sdn_sst_cv <- apply(sst_cv, 2, sdN)
# sst_cv <- scale(sst_cv, center = mean_sst_cv,
#                 scale = sdn_sst_cv)
# nonzero_sd_cols <- complete.cases(t(sst_cv))
# sst_cv <- sst_cv[, nonzero_sd_cols]

standardize_response=FALSE
if(standardize_response == TRUE) {
  mean_y_train <- mean(precip_cv)
  sdn_y_train <- sdN(precip_cv)
  precip_cv <- scale(precip_cv, center=mean_y_train, scale = sdn_y_train)
}

sst_eval <- readRDS("data/processed/sst_eval.rds")

# sst_eval <- scale(sst_eval, center = mean_sst_cv,
#                   scale = sdn_sst_cv)
# sst_eval <- sst_eval[, nonzero_sd_cols]
sst_cv <- standardize_train(sst_cv)
sst_eval <- standardize_test(sst_cv, sst_eval)

precip_eval <- readRDS("data/processed/precip_eval.rds")


# fit full model
full_model <- glmnet(sst_cv, precip_cv, lambda=l_min,
                     standardize=FALSE, standardize.response = FALSE)
# save full model
saveRDS(full_model, paste0(full_save_to, "full-model.rds"))

# predict on evaluation data
preds <- c(predict(full_model, newx = sst_eval))
if(standardize_response == TRUE) {
  preds <- preds*sdn_y_train+mean_y_train
}
# plot predictions against true data
ms_start <- nrow(sst_cv)+1
ms_end <- nrow(sst_cv)+nrow(sst_eval)
df <- data.frame(predictions = preds, targets = precip_eval, ids=c(ms_start:ms_end))
plt <- plot_predictions(df)
plt
saveRDS(plt, paste0(full_save_to,"pred-plots/pred-plot-full.rds"))

# get mse on evaluation data set
comp_mse(preds, precip_eval) # 2290.062

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
