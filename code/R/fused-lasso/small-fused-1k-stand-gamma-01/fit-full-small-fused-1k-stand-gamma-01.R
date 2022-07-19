# fit full model of small fused cv 1k
# evaluate small fused CV 1k
getwd()
#setwd("Repos/MA-climate/")
library(genlasso)
#library(ggplot2)
#library(dplyr)
source("code/R/helper-functions.R")

model_name <- "small-fused-1k-stand-gamma-01"

full_mod <- fit_full_fused(model_name)
