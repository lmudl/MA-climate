# originall we have
# in features 432 in target as well
# later we will use 432-370 when predicting
# BUT when we use timelags we have less training data
# since we drop the first 3 months of data


ids <- createTimeSlices(1:370, initialWindow=60, horizon=14,
                        skip=60+14-1)
ids2 <- createTimeSlices(1:3, initialWindow=60, horizon=14,
                                skip=60+14-1)
length(ids)
lapply(ids$train, length)
lapply(ids2$train, length)
lapply(ids2$test, length)

index_list <- caret::createTimeSlices(1:nrow(sst), initialWindow=size_train, horizon=size_test,
                                      skip=size_train+size_test-1)

source()
# before add_colnames sst 
  # add coordinates as colnames to the dataframe so we can plot later more easily
# cv run through again quick
# we prepare sst 
  # we drop all columns that are only NA
nrow(features)
get_obs_to_drop(432, 5)
432 %% 5
floor(429/5)*5
floor(432/5)*5
432/5
floor(432/5)*5


# i dont have to drop the na columns because they get drop anyway
# because of the timeslice creation 
# so if get_obs_to_drop > lag
# we dont have to drop 
# but here actually lag is greater so we have to drop it

# usually 370 train
# 432-370 train

# when we loose observation due to timelags
# should we drop observations from train or test
# also is it so important that are folds have same size
# or should I not case too much about a difference of some observations?
# although that might be important because of the regularisation paramater
# that the folds are of same size

# right now we choose 370 from 432
# intial thinking, more than 5 years of testing
# and we say giving 62 months for test gives the convenience that
# the Timeslices can be evenly created













