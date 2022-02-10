# testing tidyverse
setwd("Repos/MA-climate/")
#source("code/R/helper-functions.R")
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
library(raster)
library(ggplot2)
library(GGally)



#######
stocks <- tibble(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

gather(stocks, "stock", X, -time)

stocks
names(precip)
rownames(precip)
################
#colnames(values(precip)) <- 1:length(colnames(values(precip)))
??str_replace
test <- raster::as.data.frame(precip, xy = TRUE, long=TRUE)
test$layer <- as.numeric(stringr::str_replace(test$layer, "X", ""))

head(test)
colnames(test) <- c("long", "lat", "month", "precip")

head(test)

precip_gly <- glyphs(test, "long", "month", "lat", "precip")

p <- ggplot(precip_gly, ggplot2::aes(gx, gy, group = gid)) +
  add_ref_lines(precip_gly, color = "grey90") +
  add_ref_boxes(precip_gly, color = "grey90") +
  geom_path() +
  theme_bw() +
  labs(x = "", y = "")

p

######### now the same with the deseasonalised data
# des_precip <- readRDS("data/processed/deseasonalised_precip.rds")
# # turn into raster
# # load precip_og
# pog <- brick("data/interim/drought/chirps_setreftime.nc")
# # directly
# # create copy of og precip
# ref <- pog
# rm(pog)
# # give the copy the deseasonalised values so we have a raster obj
# head(getValues(ref))[1:5,1:4]
# ref <- setValues(ref, des_precip)
# head(getValues(ref))[1:5,1:4]
# aggregate the copy / change resolution
# ref_test <-  raster::aggregate(ref, fact = 10)
# saveRDS(ref_test, "data/processed/deasonalised_precip_aggregated.rds")
ref_test<- readRDS("data/processed/deasonalised_precip_aggregated.rds")
dim(ref_test@data@values)
test2 <- raster::as.data.frame(ref_test, xy = TRUE, long=TRUE)
test2$layer <- as.numeric(stringr::str_replace(test2$layer, "X", ""))

head(test2)
colnames(test2) <- c("long", "lat", "month", "precip")

head(test2)

precip_gly2 <- glyphs(test2, "long", "month", "lat", "precip")

p2 <- ggplot(precip_gly2, ggplot2::aes(gx, gy, group = gid)) +
  add_ref_lines(precip_gly2, color = "grey90") +
  add_ref_boxes(precip_gly2, color = "grey90") +
  geom_path() +
  geom_smooth() +
  theme_bw() +
  labs(x = "", y = "")

p2
?glyphs
# smoothing?
# local scaling?
####### local scaling ####
v <- getValues(ref_test)
dim(v)
?scale
scaled_v <- t(scale(t(v)))
dim(scaled_v)
ref_test3 <- setValues(ref_test, scaled_v)

test3 <- raster::as.data.frame(ref_test3, xy = TRUE, long=TRUE)
test3$layer <- as.numeric(stringr::str_replace(test3$layer, "X", ""))

head(test3)
colnames(test3) <- c("long", "lat", "month", "precip")

head(test3)

precip_gly3 <- glyphs(test3, "long", "month", "lat", "precip")

p3 <- ggplot(precip_gly3, ggplot2::aes(gx, gy, group = gid)) +
  add_ref_lines(precip_gly3, color = "grey90") +
  add_ref_boxes(precip_gly3, color = "grey90") +
  geom_path() +
  geom_smooth() +
  theme_bw() +
  labs(x = "", y = "")

p3
