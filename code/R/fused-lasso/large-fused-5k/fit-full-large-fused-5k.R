# fit full model of small fused cv 1k
# evaluate small fused CV 1k
getwd()
#setwd("Repos/MA-climate/")
library(genlasso)
#library(ggplot2)
#library(dplyr)
source("code/R/helper-functions.R")

model_name <- "large-fused-5k"

full_mod <- fit_full_fused(model_name)
