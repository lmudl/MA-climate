rm(list=ls())
set.seed(1234)
library(raster)
library(ggplot2)
library(dplyr)
library(ncdf4)
getwd()
# load data
precip <- readRDS("Repos/MA-climate/data/processed/decomposed_precip_data_withna.rds")
sst <- readRDS("Repos/MA-climate/data/processed/decomposed_sst_data_withna.rds")
dim(precip)
dim(sst)
# compute precip means
precip_means <- apply(precip, 2, mean)
rm(precip)
length(precip_means)
cor(sst[1,], precip_means)
length(co)
which(is.na(co))
# compute correlations
co <- apply(sst, 1, function(x) cor(x, precip_means))
any(is.na(co))
dim(co)
plot(density(co))
# create matrix/raster
old_sst <-  brick("Repos/MA-climate//data/interim/sst/ersst_setreftime.nc",
                  varname= "sst")


co <- matrix(co, nrow = old_sst@nrows, ncol = old_sst@ncols, byrow = TRUE)
l1 <- as.matrix(old_sst[[1]])
sum(which(is.na(co)) != which(is.na(l1)))
# nice both have same NAs
rara <- raster(co,
               xmn = old_sst@extent@xmin, xmx = old_sst@extent@xmax,
               ymn = old_sst@extent@ymin, ymx = old_sst@extent@ymax,
               crs = old_sst@crs)
any(is.na(as.matrix(rara[[1]])))

w <- cbind(xyFromCell(rara, 1:ncell(rara)), values(rara))
w <- as.data.frame(w)
colnames(w) <- c("lon", "lat", "corr")
head(w)

ggplot(data = w, aes(x = lon, y = lat, fill = corr)) + 
  annotation_map(map_data("world")) +
  geom_raster(interpolate = TRUE) +
  #geom_polygon(aes(fill=sst, colour = "black")) +
  scale_fill_viridis_c(option="A") +
  theme_bw() +
  coord_quickmap() 

wn <- w
wn$corr[abs(wn$corr)<0.9] <- 0

ggplot(data = wn, aes(x = lon, y = lat, fill = corr)) + 
  annotation_map(map_data("world")) +
  geom_raster(interpolate = TRUE) +
  #geom_polygon(aes(fill=sst, colour = "black")) +
  scale_fill_viridis_c(option="A") +
  theme_bw() +
  coord_quickmap() 
