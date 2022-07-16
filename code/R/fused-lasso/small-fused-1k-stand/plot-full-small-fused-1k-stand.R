# plot full 
library(genlasso)
library(ggplot2)
library(dplyr)
source("code/R/helper-functions.R")

model_name <- "small-fused-1k-stand"
get_best_lambda_fused(model_name)
plot_fused_full(model_name)

