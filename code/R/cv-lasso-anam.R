# do analysis for different data sets and combinations
getwd()
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
#library(caret)
library(glmnet)

# 
# features_cv <- sst[1:370,]
# target_cv <- precip[1:370]

model_folder <- "cv-lasso-anam"

sst_anam_cv <- readRDS("data/processed/sst_anam_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")

lasso_og <- cv_for_ts(sst = sst_anam_cv, precip = precip_cv, nfold = 5, 
                      size_train = 60, size_test = 14,
                      save_folder = model_folder,
                      model = "lasso", 
                      include_ts_vars=FALSE,
                      stand=FALSE, diff_features=FALSE, 
                      des_features=FALSE)

# plotting ####
save_to <- paste0("results/CV-lasso/", model_folder, "/")

# load the error-matrix, lambda and the model_list ####
err_mat <- readRDS(paste0(save_to,"err-mat.rds"))
lambdas <- readRDS(paste0(save_to,"lambda-vec.rds"))
ids <- readRDS(paste0(save_to, "index-list.rds"))
model_list <- load_models(paste0(save_to,"fold-models"))


plot_save_errors(err_mat = err_mat, lambdas = lambdas, save_to = save_to)
plot_coef_maps(model_list = model_list, err_mat = err_mat, save_to=save_to)
plot_predictions_best_l(err_mat = err_mat, model_list = model_list, ids = ids, 
                        features=sst_anam_cv, target=precip_cv,
                        lambdas = lambdas, save_to = save_to)

# fit full model ####
l_min_id <- which.min(apply(err_mat, 1, mean))
l_min <- lambdas[l_min_id]

# load eval data
sst_anam_eval <- readRDS("data/processed/sst_anam_eval.rds")
precip_eval <- readRDS("data/processed/precip_eval.rds")


# fit full model
full_model <- glmnet(sst_anam_cv, precip_cv, lambda=l_min,
                     standardize=FALSE)
# save full model
saveRDS(full_model, paste0(save_to, "full-model.rds"))

# predict on evaluation data
preds <- c(predict(full_model, newx = sst_anam_eval))

# plot predictions against true data
ms_start <- nrow(sst_anam_cv)+1
ms_end <- nrow(sst_anam_cv)+nrow(sst_anam_eval)
df <- data.frame(predictions = preds, targets = precip_eval, ids=c(ms_start:ms_end))
plt <- plot_predictions(df)
saveRDS(plt, paste0(save_to,"pred-plots/pred-plot-full.rds"))

# get mse on evaluation data set
comp_mse(preds, precip_eval)

# play with full model
# l_min
# full_model2 <- glmnet(sst_anam_cv, precip_cv,
#                      standardize=FALSE)
# preds2 <- predict(full_model2, newx = sst_anam_eval)
# preds2
# mse_vec <- (apply(preds2, 2, function(x) comp_mse(x, precip_eval)))
# df <- data.frame(predictions = preds, targets = precip_eval, ids=c(ms_start:ms_end))
# plt <- plot_predictions(df)
