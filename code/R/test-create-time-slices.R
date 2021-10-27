#example create time slices
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
