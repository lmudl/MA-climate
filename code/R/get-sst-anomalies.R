# create sst data with monthly anomalies compared to yearly means
rm(list = ls())
getwd()
setwd("MA-climate")
Sys.getenv("PATH")
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:\\cygwin64\\bin\\",sep = ";"))
library(ClimateOperators)

cdo("yearmean data/interim/sst-interim.nc data/interim/sst-yearmean.nc")
cdo("sub data/interim/sst-interim.nc data/interim/sst-yearmean.nc 
    data/interim/sst-anomalies.nc")

# check if values are meaningful
# maybe write reusable function here
# that can be imported

# checked with sst-example, looked meaningful kinda
# maybe randomly get 10 points and display their anomalies
# 

