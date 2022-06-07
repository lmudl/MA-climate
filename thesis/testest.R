getwd()
setwd("Repos/MA-climate/")
library(raster)
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst <- as.matrix(sst)
class(sst)
library(tseries)
x <- c(sst[1,])
b <- acf(x, plot = FALSE)
b$acf
sst <- na.omit(sst)
res <- apply(sst, 1, function(x) acf(x, plot = FALSE))
length(res)
res[[1]]$acf[1:4]
m <- matrix(nrow=length(res), ncol = 4)
for (i in 1:length(res)) {
  m[i,] <- res[[i]]$acf[1:4]
}
dim(m)
m
View(m)
##
test <- sst[1000:1010,1:10]
test
#install.packages("tsutils")
#library("tsutils")
lagmatrix(test,c(1,2))
?lagmatrix
