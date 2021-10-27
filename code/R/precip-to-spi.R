# script/ code for turining raw precip into 
# drought index
# original paper too uses SPI
# PDF are not homogenous from month to month
# data is split into twelve series and PDF is
# fit to each series.

# test from the package
library(SPEI)
data(wichita)
?wichita
?thornthwaite

precip <- readRDS("data/processed/deseasonalised_precip.rds")
dim(precip) # 61200 432
# Compute mean of precip
precip <- apply(precip, 2, mean)
precip <- matrix(precip)

#TODO which month does precip dataset start
# lets say it starts in january
#TODO keep track of which months might be cutoff
# during creating CV time slices
#TODO read paper on chirps data set

head(wichita)
# for SPI we need one long time series
# so each row is a time series (in precip)
test <- spi(precip[1,],3)
summary(test)
names(test)
plot(test)

# first 2 values are NA since drought index relies
# on past values, for the first values they dont exist
head(test)

precip[1,1:10]

#precip to spi
#input: precip data, wanted time window
#output:spi 
#possible other parameters, distr, ref window
precip_to_spi <- function(precip_data, spi_window) {
  spi-matrix <- apply(precip_data, 1, function(x) spi(x,spi_window)$fitted)
  return(t(spi-matrix))
}

