# testing

# add_time <- function(df) {
#   rn <- rownames(df)
#   rn_str <- strsplit(rn, "X", "")
#   time_vec <- sapply(rn_str, FUN = function(x) as.integer(x[[2]]))
#   df <- cbind(time_vec, df)
#   return(df)
# }

add_time <- function(df,offs=0) {
  nr <- nrow(df)
  time_vec <- seq(nr)
  df <- cbind(time_vec, df)
  return(df)
}



add_month_fac <- function(df) {
  t_vec <- df[, "time_vec"]
  fac_month <- t_vec %% 12
  fac_month[fac_month == 0] <- 12
  fac_month <- as.factor(fac_month)
  df <- cbind(fac_month, df)
  return(df)
}

add_int <- function(df) {
  t_vec <- df[, "time_vec"]
  fac_m <- df[, "fac_month"]
  
}


sst_cv <- readRDS("data/processed/sst_cv.rds")
precip_cv <- readRDS("data/processed/precip_cv.rds")
k <- 3

sst_cv_roll <- apply(sst_cv, 2, function(x) zoo::rollmean(x, k))
precip_cv <- c(precip_cv[-c(1:(k-1))])
#precip_cv <- diff(precip_cv, 2)

# sst_cv_roll <- as.data.frame(sst_cv_roll)
sst_cv_roll <- as.data.frame(sst_cv_roll)
sst_cv_roll <- add_time(sst_cv_roll)
sst_cv_roll <- add_month_fac(sst_cv_roll)
is.factor(sst_cv_roll$time_vec)
# sst_cv_roll <- data.frame(sst_cv_roll)
#sst_cv_roll <- model.matrix(~. ,sst_cv_roll)
#sst_eval_roll <- model.frame(~ colnames(sst_eval_roll)+time_vec*fac_month ,sst_eval_roll)

test_df <- data.frame(sst_cv_roll, precip_cv)

colnames(test_df) <- c(colnames(sst_cv_roll), "precip")
test_df <- as.data.frame(test_df)
test_df$time_vec <- as.factor(test_df$time_vec)
is.factor(test_df$time_vec)
#test_m <- cv.glmnet(precip ~ paste(colnames(test_df), collapse = "+" ))
test_m <- cv.glmnet(precip ~ .)
plot(test_m)
min(test_m$cvm)

precip_eval <- readRDS("data/processed/precip_eval.rds")
sst_eval <- readRDS("data/processed/sst_eval.rds")

sst_eval_roll <- apply(sst_eval, 2, function(x) zoo::rollmean(x, k))
precip_eval <- c(precip_eval[-c(1:(k-1))])
sst_eval_roll <- add_time(sst_eval_roll)
sst_eval_roll <- add_month_fac(sst_eval_roll)

sst_eval_roll <- as.data.frame(sst_eval_roll)
#precip_eval <- diff(precip_eval, 2)
sst_eval_roll <- model.frame(~ + time_vec*fac_month ,sst_eval_roll)
ncol(sst_eval_roll)
sst_eval_roll[1:4,10988:10990]
sst_eval_roll[1:4,1:4]


preds <- predict(test_m, newx = sst_eval_roll, s=test_m$lambda.min)
#plot(ts(preds))
plot(ts(precip_eval))
lines(ts(preds))

comp_mse(preds, precip_eval)


# 
# write simple function to add trend and month
# or add diff, rollmean, factor month, trend, s


# diff precip too!!!


