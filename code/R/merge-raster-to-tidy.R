library(raster)
library(dplyr)
library(ggplot2)
temp <- stack("data/interim/hadcrut-interim.nc")
drought <- stack("data/interim/cru-interim.nc")

test <- raster::subset(temp, 1:10)
# names(test)
# test <- raster::as.data.frame(test,  xy = TRUE)
# test <- head(test)
# test
# dim(temp)
merge_grids <- function(data) {
  out <- data.frame()
  for (i in seq(dim(data)[3])) {
    sub <- raster::subset(data, i)
    varname <- names(sub)
    sub <- raster::as.data.frame(sub, xy = TRUE)
    # sub <- head(sub)
    sub <- sub %>% rename(temp = varname) %>% 
      mutate(coord = paste0(x, ";", y), time = i) %>% select(temp, coord, time)
    out <- rbind(out, sub)
    print(paste(i, "out of", dim(data)[3]))
  }
  return(out)
}

library(doParallel)
registerDoParallel(5)
merge_grids_parallel <- function(data) {
  out <- foreach (i = seq(dim(data)[3]), .combine = rbind, .packages = "dplyr") %dopar% {
    sub <- raster::subset(data, i)
    varname <- names(sub)
    sub <- raster::as.data.frame(sub, xy = TRUE)
    # sub <- head(sub)
    sub <- sub %>% rename(temp = varname) %>% 
      mutate(coord = paste0(x, ";", y), time = i) %>% select(temp, coord, time)
    # print(paste(i, "out of", dim(data)[3]))
  }
  return(out)
}


system.time(merge_grids(temp)) #user 228.96, system 40.58, verstr 269
system.time(merge_grids_parallel(temp)) #user 0.82, system 0.45, verstr 30.92
out <- merge_grids(temp)
save(out, file = "data/processed/hacrut.Rda")
system.time(out_two <- merge_grids_parallel(drought))
save(out_two, file = "data/processed/cru-processed.Rda")
head(out_two)
