cluster_means_list

only_get_lambdas <- function(sst, precip, index_list) {
  lambdas_mat <- matrix(ncol=100,nrow=5)
  for(j in  1:length(index_list$train)) {
    id_train <- unlist(index_list$train[j], use.names = FALSE)
    id_test <- unlist(index_list$test[j], use.names = FALSE)
    x_train <- sst[id_train,]
    y_train <- precip[id_train]
    x_test <- sst[id_test,]
    y_test <- precip[id_test]
    lambdas_mat[j,] <- get_lambda_values(x_train,y_train)
  }
  return(lambdas_mat)
}
m <- only_get_lambdas(sst, precip, index_list = ids)
dim(m)
apply(m,1,range)
range(get_lambda_values(sst, precip))


dim(sst)
length(precip)
t_sst <- sst[1:370,]
precip <- as.matrix(precip)
precip <- apply(precip, 2, mean)
length(precip)
t_pr <- precip[1:370]
tm <- cv.glmnet(x=t_sst, y=t_pr, nfolds = 10, standardize=TRUE,
                standardize.response=FALSE)
plot(tm)
preds <- predict(tm, newx=sst[371:432,], s=tm$lambda.1se)
df <- data.frame(predictions = c(preds), targets=precip[371:432], ids=371:432)
p <- plot_predictions(df)
p
get_mse_from_pred_plot(p)
tm$nzero
