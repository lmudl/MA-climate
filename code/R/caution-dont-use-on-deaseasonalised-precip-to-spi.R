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
setwd("Repos/MA-climate/")

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
library(foreach)

precip_to_spi <- function(precip_data, spi_window, 
                          comp_parallel, ncores) {
  if(!comp_parallel) { 
    spi_matrix <- apply(precip_data, 1, function(x) spi(x,spi_window)$fitted)
    return(t(spi_matrix))
  }
  if(comp_parallel) {
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    spi_matrix <- foreach(i=1:nrow(precip_data), .combine = rbind) %dopar% {
      c(SPEI::spi(precip_data[i,],spi_window)$fitted)
    }
    parallel::stopCluster(cl)
    return(spi_matrix)
  }
}

#parallel computing rentiert sich erst ab 100 ro
#system.time({par <- precip_to_spi(precip[1:100,], 6, parallel = c(TRUE, ncores))})
#system.time({nonpar <- precip_to_spi(precip[1:100,], 6, parallel = c(FALSE))})

# TODO save the data matrix for 3, 6, 12 spi
#system.time({spi_3 <- precip_to_spi(precip, spi_window = 3, comp_parallel = TRUE, 5)})
#saveRDS(spi_3, "data/processed/spi_3.rds")
spi_3 
#system.time({spi_6 <- precip_to_spi(precip, spi_window = 6, comp_parallel = TRUE, 5)})
#User      System verstrichen 
#77.49       22.01      460.54 
#saveRDS(spi_6, "data/processed/spi_6.rds")
#system.time({spi_12 <- precip_to_spi(precip, spi_window = 12, comp_parallel = TRUE, 5)})
#User      System verstrichen 
#70.74       20.20      411.80 
#saveRDS(spi_12, "data/processed/spi_12.rds")

#TODO load spi data frame and check how many rows are NA



