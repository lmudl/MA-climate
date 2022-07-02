# evaluate small fused cv 1k
library(genlasso)
library(ggplot2)
source("code/R/helper-functions.R")
model_path <- "results/CV-lasso/small-fused-cv-1k-stand/"

# load small sst eval
small_sst_cv <- readRDS("data/processed/small_sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")

ids <- readRDS(paste0(model_path, "index-list.rds"))
err_mat <- readRDS(paste0(model_path, "err-mat.rds"))
mins <- apply(err_mat, 2, which.min)

# load models
model_list <- load_models(paste0(model_path, "fold-models"))


# PLOT PREDICTIONS ####
# we need model_list, 
# cv data precip and sst
# ids
# err_mat
# for each i in model_list
# load model
# get smallest lambda id
# get smallest lambda from model
# get sst_test
# get precip_test
# predict on sst_test with smallest lambda
# plot with precip_test together
# save plot
# comp mse
# idea: we could also save lambdas from models


plot_predictions_best_l_fused <- function(err_mat, model_list,
                                   ids, features, target,
                                   save_to, center_target = TRUE,
                                   standardize_features = FALSE) {
  target <- scale(target, center = TRUE, scale = FALSE)
  dir.create(paste0(save_to, "/pred-plots/"))
  for(i in seq(length(model_list))) {
    ids_i <- ids$test[[i]]
    model_i <- model_list[[i]]
    l_min_id <- which.min(err_mat[,i])
    l_min <- model_i$lambda[l_min_id]
    features_i <- features[ids_i,]
    if(standardize_features==TRUE){
      # x_train <- scale(x_train)
      # x_test <- scale(x_test)
      # https://stackoverflow.com/questions/59846325/confusion-about-standardize-option-of-glmnet-package-in-r
      features_i <- apply(features_i, 2, custom_stand)
      print("standardizped features")
    }
    target_i <- target[ids_i]
    preds <- c(predict.genlasso(model_i, Xnew=features_i,
                       lambda = l_min)$fit)
    plot_df <- data.frame(targets=target_i, predictions=preds,
                          ids=ids_i)
    plt <- plot_predictions(plot_df)
    saveRDS(plt, paste0(save_to, "/pred-plots/", "pred-plot-fold-", i,".rds"))
  }
}

plot_predictions_best_l_fused(err_mat, model_list, ids,features = small_sst_cv,
                              target = precip_cv, save_to=model_path,
                              standardize_features = TRUE)
readRDS(paste0(model_path,"/pred-plots/pred-plot-fold-5.rds"))


# plotting errors ####
plot_error_fused <- function(err_df_f, fold_i, l_min) {
  p <- ggplot(err_df_f, aes(x=log_lambda_i,
                            y=error_i)) +
    ylab(paste("MSE Fold", fold_i)) + 
    xlab(expression("log" ~ lambda)) +
    geom_point() +
    geom_vline(xintercept = l_min, linetype = "dashed", colour = "red")
  return(p)
}

#p <- plot_error_fused(err_df_f = err_df_f)

plot_all_fold_error_fused <- function(model_list, err_mat, save_to) {
  dir.create(paste0(save_to, "err-mat-plots/"))
  for(i in seq(length(model_list))) {
    model_i <- model_list[[i]]
    lambda_i <- model_i$lambda
    log_lambda_i <- log(lambda_i)
    min_err_id <- which.min(err_mat[,i])
    l_min <- log_lambda_i[min_err_id]
    err_df_f <- data.frame(error_i = err_mat[,i],
                           log_lambda_i = log_lambda_i)
    p <- plot_error_fused(err_df_f, fold_i = i, l_min = l_min)
    saveRDS(p, paste0(save_to, "/err-mat-plots/", "err-plot-fold", i, ".rds"))
  }
}

plot_all_fold_error_fused(model_list, err_mat, save_to=model_path)

plot_errline_gg_fused <- function(model_list, err_mat, save_to) {
  dir.create(paste0(save_to, "err-mat-plots/"))
  nm <- length(model_list)
  l_mat <- matrix(NA, nrow=maxsteps,ncol=nm)
  for(i in seq(nm)) {
    l_mat[,i] <- log(model_list[[i]]$lambda)
  }
  df_list <- list()
  for(i in seq(nm)) {
    df_list[[i]] <- data.frame(err = err_mat[,i], loglambda = l_mat[,i],
                               fold = i)
  }
  da <- df_list[[1]]
  for(i in seq(length(df_list)-1)) {
    da <- full_join(da, df_list[[i+1]])
  }
  p <- ggplot(data=da, aes(x=loglambda, y=err)) +
    geom_point(aes(color=factor(fold))) +
    ylab("MSE") + 
    xlab(expression("log" ~ lambda)) +
  saveRDS(p, paste0(save_to, "err-mat-plots/err-line-plot.rds"))
  return(p)
}

p <- plot_errline_gg_fused(model_list, err_mat, save_to=model_path)


p1 <- readRDS(paste0(model_path, "err-mat-plots/err-line-plot.rds"))
p2 <- readRDS("results/CV-lasso/small-fused-cv-1k/err-mat-plots/err-line-plot.rds")



# PLOT non-coefficients

m1 <- model_list[[1]]
cc <- m1$beta

# get l_min from combination
# of err_mat and lambda from model
# names we need to load the sst again 


plot_nonzero_coef_from_fold_fused <- function(model, fold_nr, err_mat, features,
                                              target, standardize_features,ids) {
  l_min <- which.min(err_mat[,fold_nr])
  all_coefs <- model$beta[,l_min]
  # ask fabi
  if(standardize_features==TRUE){
    features_i <- features[ids$train[[fold_nr]]]
    #sdny <- sdN(target)
    sdn_vec <- apply(features_i, 2, custom_stand)
    #all_coefs <- (all_coefs*sdny)/sdn_vec
    all_coefs <- all_coefs/sdn_vec
  }
  # MAYBE WE CAN DROP -1 bc we get only betas now!
  #all_coefs <- coefs[-1]
  nonzero_coefs <- all_coefs != 0
  cnames <- colnames(features)
  nonzero_coef_names <- cnames[nonzero_coefs]
  num_coef_names <- coef_names_to_numeric(nonzero_coef_names)
  coef_mat <- cbind(num_coef_names, all_coefs[nonzero_coefs])
  plt <- plot_nonzero_coefficients(coef_mat)
  return(plt)
}

plot_coef_maps_fused <- function(model_list, err_mat, save_to, features,
                                 target, standardize_features=FALSE, ids) {
  dir.create(paste0(save_to,"/coef-plots/"))
  for(i in 1:length(model_list)) {
    p <- plot_nonzero_coef_from_fold_fused(model_list[[i]], i, err_mat, features,
                                           target, standardize_features, ids)
    saveRDS(p, paste0(save_to, "/coef-plots/", "coef-plot-fold-", i,".rds"))
  }
}

plot_coef_maps_fused(model_list, err_mat, save_to=model_path, small_sst_cv,
                     target = precip_cv, standardize_features = TRUE, ids)

readRDS(paste0(model_path, "/coef-plots/coef-plot-fold-1.rds"))
