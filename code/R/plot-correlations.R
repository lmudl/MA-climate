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
# compute correlations for specific time lag
length(precip_means)
test <- round(head(precip_means))
test[-c(seq(length(test),length(test)-2))]
# for sst if was vector
head(test,-1)
test[-c(1:2)]
# for sst because is matrix drop last n columns
df[1:(length(df)-5)]
m <- matrix(c(1:20), ncol = 5)
m[,1:(ncol(m)-2)]
#


# option front or back cut
# for precip front cut
# for sst back cut
cut_matrix <- function(matrix, timelag, side=c("front,back")) {
  if(side == "front") {
    matrix <- matrix[, -c(1:timelag)]
  }
  if(side == "back"){
    matrix <- matrix[, 1:(ncol(matrix)-timelag)]
  }
  return(matrix)
}

# compute correlations
# for different time lags what do we need?
# in: precip path, sst path, time lags
# out: correlation vector
# load precip and sst decomposed
# compute precip means
# remove precip data
# compute correlations with time lags
# correlations out

compute_corr <- function(dec_matrix_sst, dec_matrix_precip, timelag=0) {
  if (timelag!=0) {
    dec_matrix_sst <- cut_matrix(dec_matrix_sst, timelag, "back")
    dec_matrix_precip <- cut_matrix(dec_matrix_precip, timelag, "front")
  }
  assertthat::are_equal(ncol(dec_matrix_precip), ncol(dec_matrix_sst))
  precip_means <- apply(dec_matrix_precip, 2, mean)
  cor_vec <- apply(dec_matrix_sst, 1, function(x) cor(x, precip_means))
  return(cor_vec)
}


corr0 <- compute_corr(sst, precip,0)
# create matrix/raster
old_sst <-  brick("Repos/MA-climate//data/interim/sst/ersst_setreftime.nc",
                  varname= "sst")


# plot correlation 
# in: correlation vector, old sst data, timelag for legend
plot_corr <- function(corr_vec, old_sst, timelag,
                      quantiles = c(TRUE,FALSE)) {
  corr_grid <- matrix(corr_vec, nrow = old_sst@nrows, ncol = old_sst@ncols, byrow = TRUE)
  corr_raster <- raster(corr_grid,
                        xmn = old_sst@extent@xmin, xmx = old_sst@extent@xmax,
                        ymn = old_sst@extent@ymin, ymx = old_sst@extent@ymax,
                        crs = old_sst@crs)
  df <- cbind(xyFromCell(corr_raster, 1:ncell(corr_raster)), values(corr_raster))
  df <- base::as.data.frame(df)
  colnames(df) <- c("Longitude","Latitude", "Correlation")
  if(quantiles) {
    q <- quantile(df[,"Correlation"],probs=c(0.05,0.95), na.rm = TRUE)
    df$Correlation[df$Correlation>q["5%"] & df$Correlation<q["95%"]] <- 0
  }
  plt <- ggplot(data = df, aes(x = Longitude, y = Latitude, fill = Correlation)) +
    annotation_map(map_data("world")) +
    geom_raster(interpolate = TRUE)
    if(!quantiles) {
      plt <- plt +
        ggtitle(paste("Correlation of Sea Surface Temperature and Precipitation for Timelag",timelag))
    } else {
      plt <- plt +
        ggtitle(paste(c("Correlation of Sea Surface Temperature and Precipitation for Timelag",timelag,
                      "Values below quantiles threshold set to 0"))) 
      
    }
  plt <- plt +
    scale_fill_viridis_c(option="A") +
    theme_bw() +
    coord_quickmap() 
  return(plt)
}

compute_corr_and_plot <- function(
  dec_matrix_sst, dec_matrix_precip, old_sst,
  timelag=0,quantiles = c(TRUE,FALSE)) {
  
  corr_vec <- compute_corr(dec_matrix_sst, dec_matrix_precip, timelag)
  plt <- plot_corr(corr_vec, old_sst, timelag, quantiles)
  return(plt)                                 
}


corr0 <- compute_corr(sst, precip,0)
plot(density(na.omit(corr0)))
plot0 <- plot_corr(corr0, old_sst, 0, TRUE)
t <- compute_corr_and_plot(sst,precip,old_sst,0,FALSE)
corr1 <- compute_corr(sst, precip,1)
plot(density(na.omit(corr1)))
plot1 <- plot_corr(corr1, old_sst,1,FALSE)
plot1q <- plot_corr(corr1, old_sst, 1, TRUE)





co <- matrix(corr0, nrow = old_sst@nrows, ncol = old_sst@ncols, byrow = TRUE)
l1 <- as.matrix(old_sst[[1]])
sum(which(is.na(co)) != which(is.na(l1))) #0 
# nice both have same NAs
rara <- raster(co,
               xmn = old_sst@extent@xmin, xmx = old_sst@extent@xmax,
               ymn = old_sst@extent@ymin, ymx = old_sst@extent@ymax,
               crs = old_sst@crs)
any(is.na(as.matrix(rara[[1]])))

w <- cbind(xyFromCell(rara, 1:ncell(rara)), values(rara))
w <- as.data.frame(w, colnames = c("lon","lat", "corr"))
colnames(w) <- c("lon", "lat", "corr")
q <- quantile(w$corr, probs=c(0.005,0.995), na.rm = TRUE)
#ind <- df[,"Correlation"] >= q["5%"] | df[,"Correlation"] <= q["95%"] 
w$corr[w$corr>=q["0.5%"]&w$corr<=q["99.5%"]] <- 0
sum(na.omit(w$corr)!=0)
dim(w)
length(precip_means)
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

ggplot(data = w, aes(x = lon, y = lat, fill = corr)) + 
  annotation_map(map_data("world")) +
  geom_raster(interpolate = TRUE) +
  #geom_polygon(aes(fill=sst, colour = "black")) +
  scale_fill_viridis_c(option="A") +
  theme_bw() +
  coord_quickmap() 



# plot correlations
# in: correlation vector
# out: plot
# load old sst to create raster for corr raster
# remove old_sst
# create data frame
# use ggplot
# plot out

