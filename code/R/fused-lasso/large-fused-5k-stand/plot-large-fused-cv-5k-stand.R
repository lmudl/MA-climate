# evaluate small fused CV 1k
getwd()
#setwd("Repos/MA-climate/")
library(genlasso)
library(ggplot2)
library(dplyr)
source("code/R/helper-functions.R")

model_name <- "large-fused-5k-stand"

plot_cv_fused(model_name)
get_best_lambda_fused(model_name)
