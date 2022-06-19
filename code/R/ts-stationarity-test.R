# are the remainders stationary?

des <- readRDS("data/processed/rtsa_deseasonalised_sst.rds")
rem_sst <- des@remainder
rem_sst <- rem_sst@raster
rem_sst <- as.matrix(rem_sst)
dim(rem_sst)
library(urca)
ur.kpss(rem_sst[1000,])

library(dplyr)
library(forecast)
vec <- rem_sst[1000,]
vec %>% ur.kpss() %>% summary()
vec %>% diff() %>% ur.kpss() %>% summary()
ndiffs(vec)

vec2 <- sst[,1000]
ndiffs(vec2)

plot(ts(vec2))
plot(ts(vec2%>%diff()))

ggAcf(precip)
ggAcf(diff(diff(precip)))

tt <- apply(sst, 2, ndiffs)
table(tt)
