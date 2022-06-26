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
save_to <- "cv-lasso-des"
full_save_to <- paste0("results/CV-lasso/",save_to,"/")

# load err mat, lambdas and get lambda min
err_mat <- readRDS(paste0(full_save_to,"err-mat.rds"))
lambdas <- readRDS(paste0(full_save_to,"lambda-vec.rds"))
l_min_id <- which.min(apply(err_mat, 1, mean))
l_min <- lambdas[l_min_id]

# load data, pprepare data
ids <- readRDS(paste0(full_save_to,"index-list.rds"))
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst <- as.matrix(sst)
sst <- add_colnames("data/interim/sst/ersst_setreftime.nc",sst)
sst <- prepare_sst(sst)
dim(sst)
anyNA(sst)
precip <- as.matrix(precip)
precip <- apply(precip, 2, mean)

# get evaluation data
start_eval <- max(ids$test$Testing356)+1
end_eval <- length(precip)
sst_train <- sst[1:(start_eval-1),]
precip_train <- precip[1:(start_eval)-1]
sst_eval <- sst[start_eval:end_eval,]
precip_eval <- precip[start_eval:end_eval]

sst_train <- apply(sst_train, 2, function(x) c(stl(ts(x, frequency=12), s.window = "periodic",
                                               robust = TRUE)$time.series[,"remainder"]))
sst_together <- rbind(sst_train, sst_eval)
sst_together <- apply(sst_together, 2, function(x) c(stl(ts(x, frequency=12), s.window = "periodic",
                                                     robust = TRUE)$time.series[,"remainder"]))
sst_test <- sst_together[-c(seq(nrow(sst_train))),]

# fit full model
full_model <- glmnet(data.matrix(sst_train), precip_train, lambda=l_min,
                     standardize=FALSE)
saveRDS(full_model, paste0(full_save_to, "full-model.rds"))
# predict on evaluation data
preds <- c(predict(full_model, newx = data.matrix(sst_eval)))

# plot predictions against true data
df <- data.frame(predictions = preds, targets = precip_eval, ids=c(start_eval:end_eval))
# plt <- ggplot() + geom_line(data=df, mapping= aes(x=seq(length(predictions)), 
#                                                   y=predictions, col = "red")) +
#   geom_line(data=df, mapping=aes(x=seq(lengths(predictions)), y=targets)) 
plt <- plot_predictions(df)
saveRDS(plt, paste0(full_save_to,"pred-plots/pred-plot-full.rds"))

# get mse on evaluation data set
comp_mse(preds, precip_eval) # 7618.352

# what if we fit full model on all lambdas?
# full_model2 <- glmnet(sst_train, precip_train, lambda=lambdas,
#                      standardize=FALSE)
# preds2 <- predict(full_model2, newx = sst_eval)
# errors2 <- apply(preds2, 2, function(x) comp_mse(x, precip_eval))
# wm2 <- which.min(errors)
# df2 <- data.frame(predictions = preds2[,wm2], targets = precip_eval)
# plt2 <- ggplot() + geom_line(data=df, mapping= aes(x=seq(length(predictions)), y=predictions, col = "red")) +
#   geom_line(data=df, mapping=aes(x=seq(lengths(predictions)), y=targets))
# plt2
# plot(ts(errors2))

# level of regularisation is a lot lower than found on the cross validation


# what if we fit the whole model how much lambdas do we get?
# fullfull <- glmnet(sst, precip, standardize = FALSE)
