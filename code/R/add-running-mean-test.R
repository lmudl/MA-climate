# how would I include timelags etc in the CV
# get train and test
# add trend
# add saisonality
# add lagged variables
# add running mean
# get lambda values before I guess?

library(zoo)

# with tidyverse cummean
# I get a vector of means
# one for each timepoint
# temp is 1,2,3,4,5,
# in data matrix then

# use tidyverse and
# https://stackoverflow.com/questions/68694115/rolling-average-1-2-and-3-lag-of-statistics-r
# to add lags and running means
# maybe first only running means?


# testing we load one fold
# load sst and precip

# do analysis for different data sets and combinations
getwd()
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
#library(caret)
library(glmnet)

# load data ####
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst <- as.matrix(sst)
sst <- add_colnames("data/interim/sst/ersst_setreftime.nc",sst)
sst <- prepare_sst(sst)
dim(sst)
anyNA(sst)
aprecip <- as.matrix(precip)
precip <- apply(precip, 2, mean)

sst  <- sst[1:370,]
precip <- precip[1:370]

# load ids for fold 1 ####
ids <- readRDS("results/CV-lasso/cv-lasso-og-data-16-06-22/index-list.rds")
ids1 <- ids$train$Training060
sst1 <- sst[ids1,]
precip1 <- precip[ids1]

# create timelag matrix test ####
library(tidyverse)
dim(sst1)
h <- head(sst1)[,1:5]
h <- as.data.frame(h)
hl <- pivot_longer(h,cols = colnames(h),
                   names_to = "location",
                   values_to = "temperature")
hl %>% group_by(location) %>%
  mutate(month = row_number(),
         rollm_2 = zoo::rollmean(x = temperature, k=2,
                                 fill = NA, align = "right")) -> hl2
hl2  %>%
  pivot_wider(names_from = location,
              values_from = c(temperature,lag1)) -> hl3           

# create timelags fold 1 ####
sst1 <- as.data.frame(sst1)
sst1 %>% pivot_longer(cols = colnames(sst1),
                      names_to = "location",
                      values_to = "temperature") %>%
  group_by(location) %>% mutate(
    rollm_2 = zoo::rollmean(x = temperature, k=2,
                            fill = NA, align = "right"),
    rollm_3 = zoo::rollmean(x = temperature, k=3,
                            fill = NA, align = "right"),
    rollm_4 = zoo::rollmean(x = temperature, k=4,
                            fill = NA, align = "right")) -> sst1b
sst1b %>% mutate(rn = row_number()) %>%
  pivot_wider(names_from = location, values_from = c(temperature, rollm_2,
                                                     rollm_3, rollm_4)) %>%
  select(-rn) -> sst1_ts
sst1_ts <- sst1_ts %>% mutate(run_month = row_number(), .before=colnames(sst1_ts)[1])
dim(sst1_ts)[2] == dim(sst1)[2]*4+1
# get factor months from "month" col check old code 
# for that
library(lubridate)
library(dplyr)
cyc_month_vec <- factor(as.character(month(ymd(010101) + 
                                             months(sst1_ts$run_month-1),
                                           label=TRUE,abbr=TRUE)))
sst1_ts <- sst1_ts %>% mutate(cyc_month = cyc_month_vec) %>%
  relocate(cyc_month, .after = run_month)

keep_vec <- complete.cases(sst1_ts)
sst1_ts_cl <- sst1_ts[keep_vec,]
precip1_cl <- precip1[keep_vec]
test_f1 <- glmnet(sst1_ts_cl, precip1_cl, standardize = FALSE)

# before prediction add new variable to sst test
add_ts_vars <- function(data_matrix) {
  df <- as.data.frame(data_matrix)
  rm(data_matrix)
  df %>% pivot_longer(cols = colnames(df),
                      names_to = "location",
                      values_to = "temperature") %>%
    group_by(location) %>% 
    mutate(
      rollm_2 = zoo::rollmean(x = temperature, k=2,
                              fill = NA, align = "right"),
      rollm_3 = zoo::rollmean(x = temperature, k=3,
                              fill = NA, align = "right"),
      rollm_4 = zoo::rollmean(x = temperature, k=4,
                              fill = NA, align = "right")) %>%
    mutate(rn = row_number()) %>%
    pivot_wider(names_from = location, 
                values_from = c(temperature, rollm_2,
                                rollm_3, rollm_4)) %>%
    select(-rn) -> df
  cnames <- colnames(df)
  df %>% mutate(run_month = row_number(), .before=cnames[1]) -> df
  cyc_month_vec <- factor(as.character(month(ymd(010101) + 
                                               months(df$run_month-1),
                                             label=TRUE,abbr=TRUE)))
  df <- df %>% mutate(cyc_month = cyc_month_vec) %>%
    relocate(cyc_month, .after = run_month)
  return(df)
} 
 
# evaluate #####

sst1_eval <- sst[ids$test$Testing060,]
sst1_eval_ts <- add_ts_vars(sst1_eval)
precip_eval <- precip[ids$test$Testing060]


# drop NA
keep_vec <- complete.cases(sst1_eval_ts)
sst1_eval_ts <- sst1_eval_ts[keep_vec,]
precip_eval_cl <- precip_eval[keep_vec]
sst1_eval_ts <- data.matrix(sst1_eval_ts)
preds <- predict(test_f1, newx = sst1_eval_ts)

errs <- apply(preds, 2, function(x) comp_mse(x, precip_eval_cl))
dim(errs)
min(errs)
which.min(errs)

plot_predictions(preds, precip_eval_cl)
plot(ts(preds[,32]))
plot(ts(precip_eval_cl))

coefs <- test_f1$beta[,32]
nz_coefs <- coefs != 0
names(nz_coefs)
"temperature"* %in% names(nz_coefs)
length(nz_coefs)
sum(stringr::str_detect(names(nz_coefs), "rollm_4"))


#if we add timelags etc

# add timelags to train features
# get complete cases
# drop non complete cases from train features
# drop non complete cases from train targets
# fit model
# add timelags to eval features
# get non complete cases from features
# drop non complete cases from from eval features
# drop non complete cases from eval targets
# predict on eval target


x_train <- add_ts_vars(x_train)
keep_vec <- complete.cases(x_train)
x_train <- x_train[keep_vec]
y_train <- y_train[keep_vec]

x_test <- add_ts_vars(x_test)
keep_vec <- complete.cases(x_test)
x_test <- x_test[keep_vec]
y_test <- y_train[keep_vec]


# what about the lambda vectors?
# lets first skip that

l1 <- get_lambda_values(sst1, precip1)
l2 <- get_lambda_values(sst1_ts_cl, precip1_cl)
range(l1)
precip1_cl %*% scale(sst1, center = TRUE, scale = TRUE)
te <- sst1_ts_cl[,1:100]
apply(te, 2, class)
class(sst1_ts_cl[,4])
is.factor(sst1_ts_cl[,3])
sum(apply(sst1_ts_cl,2, is.factor))


dim(sst1)
dim(precip1)

# round 2 ####
sst1 <- add_ts_vars(sst1)
cc <- complete.cases(sst1)
sst1 <- sst1[cc,]
precip1 <- precip1[cc]

test_m <- glmnet(sst1, precip1, standardize = FALSE)

id_test <- ids$test$Testing060
sst1_eval <- sst[id_test, ]
sst1_eval <- add_ts_vars(sst1_eval)
cc <- complete.cases(sst1_eval)
sst1_eval <- sst1_eval[cc,]

precip1_eval <- precip[id_test]
precip1_eval <- precip1_eval[cc]

preds <- predict(test_m, newx = data.matrix(sst1_eval))
errs <- apply(preds, 2, function(x) comp_mse(precip1_eval, x))
errs
