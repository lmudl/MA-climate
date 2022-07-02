# check lambdas from fold for timelag model
model_folder <- "cv-lasso-og-timelag-28-06-22-lag-1-3"
save_to <- paste0("results/CV-lasso/", model_folder, "/")

# load the error-matrix, lambda and the model_list ####
err_mat <- readRDS(paste0(save_to,"err-mat.rds"))
lambdas <- readRDS(paste0(save_to,"lambda-vec.rds"))
ids <- readRDS(paste0(save_to, "index-list.rds"))
model_list <- load_models(paste0(save_to,"fold-models"))
index_list <- ids

# Data loading
sst_cv <- readRDS("data/processed/sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")


####
lambdas
f1_train <- sst_cv[ids$train$Training060]
sst_f1 <- sst_cv[ids$train$Training060, ]
precip_f1 <- precip_cv[ids$train$Training060]
get_lambda_values(sst_f1, precip_f1)

sst_f1 <- add_ts_vars(sst_f1)
cc <- complete.cases(sst_f1)
sst_f1 <- sst_f1[cc,]
precip_f1 <- precip_f1[cc]
range(get_lambda_values(sst_f1[,-2], precip_f1))
range(lambdas)


m_test <- glmnet(sst_f1, precip_f1, relax = TRUE)
m_test$relaxed

##### ndiffs ####
nfold <- length(index_list$train)
err_mat <- matrix(NA, ncol = nfold, nrow = length(lambda_vec),
                  dimnames = list(lambda_vec))
for(j in 1:length(index_list$train)) {
  id_train <- unlist(index_list$train[j], use.names = FALSE)
  id_test <- unlist(index_list$test[j], use.names = FALSE)
  x_train <- sst_cv[id_train,]
  y_train <- precip_cv[id_train]
  x_test <- sst_cv[id_test,]
  y_test <- precip_cv[id_test]
  
  ndiffs <- apply(x_train, 2, unitroot_ndiffs)
  print(max(ndiffs))
  ndiffs <- apply(x_test, 2, unitroot_ndiffs)
  print(max(ndiffs))
  ndiffs <- unitroot_ndiffs(y_train)
  print(max(ndiffs))
  ndiffs <- unitroot_ndiffs(y_test)
  print(max(ndiffs))
}
# we see that for fold 1,3 and 5 the number of differentiations needed
# is different in train and test!!! So we always gonna use 2
# [1] 1
# [1] 2
# [1] 0
# [1] 0
# [1] 1
# [1] 1
# [1] 0
# [1] 0
# [1] 2
# [1] 1
# [1] 0
# [1] 0
# [1] 1
# [1] 1
# [1] 0
# [1] 0
# [1] 1
# [1] 2
# [1] 0
# [1] 0


#### sneasonal siffs #####
for(j in 1:length(index_list$train)) {
  id_train <- unlist(index_list$train[j], use.names = FALSE)
  id_test <- unlist(index_list$test[j], use.names = FALSE)
  x_train <- sst_cv[id_train,]
  y_train <- precip_cv[id_train]
  x_test <- sst_cv[id_test,]
  y_test <- precip_cv[id_test]
  
  ndiffs <- apply(x_train, 2, unitroot_nsdiffs)
  print(max(ndiffs))
  ndiffs <- apply(x_test, 2, unitroot_nsdiffs)
  print(max(ndiffs))
  ndiffs <- unitroot_ndiffs(y_train)
  print(max(ndiffs))
  ndiffs <- unitroot_ndiffs(y_test)
  print(max(ndiffs))
}


# check on des data ####
sst_des <- readRDS("data/processed/rtsa_deseasonalised_sst.rds")
precip_des <- readRDS("data/processed/rtsa_deseasonalised_precip.rds")

sst_des_cv <- sst_des[]

sst_des <- getValues(sst_des@remainder) + getValues(sst_des@trend)
dim(sst_des)
sst_des <- sst_des[complete.cases(sst_des),]
dim(sst_des)
max(apply(sst_des, 1, unitroot_ndiffs)) # 2


# misc ####
a <- unitroot_ndiffs(sst_cv[,1000])
b <- zoo::rollmean(sst_cv[,1000], k = 4)
plot(ts(b))
plot(ts(diff(sst_cv[,1000])))
unitroot_ndiffs(b)
#
zoo::rollmean(1:10, k=3
              )

