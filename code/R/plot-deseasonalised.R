# plot deseasonalised time series of point we originally 
# showed in the rmd markdown

# plot deseasonalised random cells
# sst
getwd()
set.seed(1234)
library(raster)
library(ggplot2)
library(dplyr)
library(ncdf4)
sst <- brick("../../data/interim/sst/ersst_setreftime.nc",
             varname= "sst")
cell <- get_random_cell(sst)
ts <- get_ts(sst, cell)
p_ts <- plot_ts(ts)
p_stl <- plot_stl(ts)
dec_sst <- readRDS("../../data/processed/deseasonalised_sst_data.rds")
dim(dec_sst)
xyfrom
set.seed(1234)
debug(get_random_cell) # we had cell 10988
p_stl_dec <- plot_stl(dec_sst[10988,])
ggsave("stl_sst_deseasonalised.jpg") #afterwards put it into plots-rmarkdown
# comparison shows that the data is deasonalised
sst_means

# precip
precip <- brick("data/interim/drought/chirps_setreftime.nc",
             varname= "precip")
cell_precip <- get_random_cell(precip)
dec_precip <- readRDS("data/processed/clean_deseasonalised_precip_data.rds")
# xyFromCell(precip, 40784) == cell_precip
ts_precip <- get_ts(precip, cell_precip)
p_stl_precip <- plot_stl(ts_precip)
p_stl_precip_dec <- plot_stl(dec_precip[40784,])

precip_means <- apply(brick_to_matrix(precip, nlayers = nlayers(precip)), 2, mean)
precip_dec_means <- apply(dec_precip, 2, mean)
plot_stl(precip_means)
plot_stl(precip_dec_means)

precip_m <- brick_to_matrixprecip



# analysis
t <- ts(ts_precip, frequency = 13)
st <- stl(t, s.window = "periodic")
res <- st$time.series[,"remainder"] + st$time.series[,"trend"]
length(st$time.series[,"trend"])
length(res)
st$time.series[,"remainder"][1]
st$time.series[,"trend"][1]
res[1] #146
plot_stl(res)

t12
12/(2*pi)
sin(2*pi*(1/1.91))

ex <- sin((1:40)*1.19) + +rnorm(1:40) + c(0.1:0.40)
ts12 <- ts(ex, frequency = 12)
stl12 <- stl(ts12, s.window = 13)
plot(stl12)




dec <- stl12$time.series[,"remainder"] + stl12$time.series[,"trend"]
dec_stl <- stl(dec, s.window = "periodic")
dec_stl$time.series
ex <- stl(dec,t.window = 12, s.window = "periodic")
ex$time.series
plot_stl(dec)

ts13 <- ts(ex, frequency = 13)
stl13 <- stl(ts12, t.window = 13, s.window = "periodic", robust = TRUE)
plot(stl12)
?stl

