library(ggplot2)
library(dplyr)
source("code/R/helper-functions.R")

model_name <- "small-fused-1k"

res_list <- get_best_lambda_fused(model_name)
