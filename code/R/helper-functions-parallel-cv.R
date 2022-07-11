
sdN=function(x){
  sigma=sqrt( (1/length(x)) * sum((x-mean(x))^2))
  return(sigma)
}

custom_stand <- function(x) {
  stand_x <- (x-mean(x))/sdN(x)
  return(stand_x)
}

cv_run_fused <- function(j, err_mat, nfold, sst, precip, index_list, save_folder, graph,
                         maxsteps, stand, standardize_features, standardize_response,
                         gamma) {
  id_train <- unlist(index_list$train[j], use.names = FALSE)
  id_test <- unlist(index_list$test[j], use.names = FALSE)
  x_train <- sst[id_train,]
  y_train <- precip[id_train]
  x_test <- sst[id_test,]
  y_test <- precip[id_test]
  
  # if(stand==TRUE){
  #   # x_train <- scale(x_train)
  #   # x_test <- scale(x_test)
  #   # https://stackoverflow.com/questions/59846325/confusion-about-standardize-option-of-glmnet-package-in-r
  #   x_train <- apply(x_train, 2, custom_stand)
  #   # or maybe scale x_test with mean and standard deviation from x_train
  #   x_test <- apply(x_test, 2, custom_stand)
  #   # y_train <- apply(y_train, 2, custom_stand)
  #   # y_test <- apply(y_train, 2, custom_stand)
  #   print("standardized features")
  # }
  if(standardize_features == TRUE) {
    mean_x_train <- apply(x_train,2, mean)
    sdn_x_train <- apply(x_train,2,sdN)
    x_train <- scale(x_train, center=mean_x_train,
                     scale=sdn_x_train)
    x_test <- scale(x_test, center=mean_x_train,
                    scale=sdn_x_train)
    print("standardized features")
  }
  if(standardize_response == TRUE) {
    mean_y_train <- mean(y_train)
    sdn_y_train <- sdN(y_train)
    y_train <- scale(y_train, center=mean_y_train, 
                     scale=sdn_y_train)
    # y_test <- scale(y_test, center=mean_y_train, 
    #                 scale=sdn_y_train)
  }
  trained_model <- fusedlasso(y=y_train, X=x_train, graph=graph,
                              verbose=TRUE, maxsteps = maxsteps,
                              gamma=gamma)
  predicted <- predict.genlasso(trained_model, Xnew=x_test)
  if(standardize_response == TRUE) {
    predicted$fit <- apply(predicted$fit, 2, function(x)  x*sdn_y_train + mean_y_train)
  }
  err_col <- apply(predicted$fit, 2, function(x) mean((x-y_test)^2))
  #err_mat[,j] <- err_col
  print(paste("finished fold", j))
  fold_model_path <- paste0("results/CV-fused/", save_folder, "/fold-models/",
                            "model-fold-",j,".rds")
  saveRDS(trained_model, file = fold_model_path)
  rm(trained_model)
  return(err_col)
}