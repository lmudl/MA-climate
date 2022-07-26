# test fused all
#setwd("Repos/MA-climate")
source("code/R/helper-functions.R")
source("code/R/helper-functions-parallel-cv.R")

model_name <- "large-fused-5k-stand-gamma-01"

run_all_fused(model_name, parts = c("plot-full"))
