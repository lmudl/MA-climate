# precip stl and such
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(fitdistrplus)
setwd("Repos/MA-climate/")
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
dim(precip)
precip <- raster::as.data.frame(precip, long=TRUE, xy = TRUE)
precip$layer <- as.numeric(str_replace(precip$layer, "X", ""))

# 0 plot one ts
# Note we could also think that there are 432 different
# data generating processes, one for each location
# but we can also explore that by fitting models
# to the mean, to the clusters, or then for each locations

# 1 plot all ts with mean ####
temp <- precip %>% group_by(layer) %>% summarise(precip_mean = mean(value))
precip <- inner_join(precip, temp, by="layer")
precip$loc <- paste(precip$x,precip$y)
head(precip)
all_precip_ts <- ggplot() +
  geom_line(data = precip, aes(x=layer, y = value, group = loc, color="grey")) + 
  labs(y="Precipitation", x  = "Month") +
  geom_line(data = precip, aes(x=layer, y = precip_mean), color="blue") 
plt

precip_dens <-  ggplot(precip, aes(x = value)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white",
                 bins=50) +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)
precip_dens
saveRDS(precip_dens, "./results/precip_dens.rds")
# 2 plot mean ts ####
plt <- ggplot() +
  geom_line(data = precip, aes(x=layer, y = precip_mean, color="grey")) + 
  labs(y="Precipitation", x  = "Month")
plt

precip_mean_dens <- ggplot(precip, aes(x = precip_mean)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white",
                 bins=50) +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)
precip_mean_dens
length(precip$precip_mean)
plot(hist(temp$precip_mean,breaks = 100))

# 3 plot detected distributions ####
descdist(precip$value)
descdist(precip$precip_mean)

# 4 plot stl decomposition ####
mean_ts <- ts(temp$precip_mean, frequency = 12)
res <- stl(mean_ts, s.window = "periodic", robust = TRUE) %>%
  forecast::autoplot()
res

## s.window makes a difference!
res <- stl(mean_ts, s.window = 13, robust = TRUE) %>%
  forecast::autoplot()
res

## robust = TRUE/FALSE also
res <- stl(mean_ts, s.window = 13, robust = TRUE)
res_plot <- res %>%
  forecast::autoplot()
outl <- data.frame(res$time.series) %>% filter(
  remainder < quantile(remainder, 0.25) - 3*IQR(remainder) |
    remainder > quantile(remainder, 0.75) + 3*IQR(remainder)
)
which(res$time.series[,1] == outl[1,1])
boxplot(res$time.series[,"remainder"])

remainder <- res$time.series[,3]
is_out <- which(remainder < quantile(remainder, 0.25) - 3*IQR(remainder) |
  remainder > quantile(remainder, 0.75) + 3*IQR(remainder))
is_out
remainder[is_out]
boxplot(remainder,range=3)
res_df <- as.data.frame(res$time.series)
ggplot() + geom_boxplot(data = res_df, aes(y=remainder), outlier.stroke = 1)
