# Attempt to create Cross Validation method for
# regression on time series data with the preselected
# variables for lm/ lasso/ elastic net/ fused lasso
library(raster)
library(caret)
?createTimeSlices

setwd("Repos/MA-climate")
# TODO load datasets
m_sst_des <- readRDS("data/processed/deseasonalised_sst.rds")
dim(m_sst_des) # 16020 432
m_precip_des <- readRDS("data/processed/deseasonalised_precip.rds")
dim(m_precip_des) # 61200 432
# TODO compute mean of precip
mean_precip <- apply(m_precip_des, 2, mean)
rm(m_precip_des)
# TODO think about which prediction windows etc make sense
#      also which forms of CV make sense
#   we have 432 months of data if we go for 5 fold CV
#   we have 432/5 = 86.4 months of test and
#   86.4*4 and 345.6 of training
#   but in time series framework we need, to take 
#   time dependency into account. So we might go
#   for 5 models, each fit with 74 train and 12 test
#   fold 1: month 1 until 86
#   fold 2_ month 87 until 173
#   etc..
#   in caret package we would have initial window=74
#   fixedwindow = TRUE and horizon = 12.
#   But how should we account for that we only want
#   non overlapping data?
# TODO write helper function for this or use caret
#   will return the indices for test and train
createTimeSlices(17:32,5,3, skip = 7)
#   if we want non-overlapping fold then skip
#   should be initialwindow+horizon-1
createTimeSlices(17:32,5,3, skip = 7)
ind <- createTimeSlices(1:20,4,2, skip = 5)
#   check for any overlaps
any(duplicated(unlist(ind, use.names = FALSE)))
#   no overlaps
#   for our data, 430 works out equally for all months
#   meaning we drop the first two months of observations
#   for example
# TODO try if  partition works
test1 <- createTimeSlices(1:430,74, 12, skip=(74+12-1))
# TODO check data form needed aka data frame or the like
# load old dataset to get coordinate names for variables
old_sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
coord <- coordinates(old_sst)
dim(coord)[1] == dim(m_sst_des)[1]
# TODO give sst columns the coordinates as names
cnames <- paste(coord[,1], coord[,2])
# rows are months, cols are coord after transposing
sst <- t(m_sst_des)
rm(m_sst_des)
colnames(sst) <- cnames
dim(sst)
# TODO fit lasso on small data set
# TODO prepare data for lasso
# TODO use only 5 years of data
sst_test <- sst[1:60,]
precip_test <- mean_precip[1:60]
drop <- apply(sst_test, 2, function(x) all(is.na(x)))
sst_test <- sst_test[,!drop]
dim(sst_test)
fit_test <- glmnet(sst_test, precip_test)
plot(fit_test)
print(fit_test)
nx <- sst[61:72,]
dim(nx)
nx <- nx[,!drop]
dim(nx)

p <- predict(fit_test, newx = nx, s=0.1790)
m <- mean_precip[61:72]
sum((p-m)^2)/12
cbind(p,m)

# Summary
# we can easily use caret package to do CV as wanted
# no reshape needed apparently
# transpose sst
# give columns names
# drop NA coordinates
# just use the matrix of sst and precip mean vector
# fit glmnet
# inspect deviance explained
# predict can easily be done make sure that NA's are
# also dropped for new x
#

#Maybe
# TODO give rows months and years. maybe


