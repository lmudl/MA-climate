# cluster precipitation data
# load precip
setwd("Repos/MA-climate/")
# precip <- readRDS("data/processed/deseasonalised_precip.rds")
# use raw precip
# load raw precip

source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
#library(dplyr)
#library(ncdf4)

# load data
precip <- brick("data/interim/drought/chirps_setreftime.nc")
nlayers(precip) #432
# current data resolution
res(precip) # 0.05000001, 0.05

# Step 0, change data resolution
# degrees from preprocess data  degrees = c("-72,-55,-9,0"))
dim(precip) # 61200 432, meaning we have 61200 cells for 432 months
# ? how to get new resolution when we dont have 2d x times strucure any more

precip <- raster::aggregate(precip, fact = 10) # aggregate, both dim factor 10 res less
dim(precip) # 18, 34, 432
m <- brick_to_matrix_wna(precip, nlayers = nlayers(precip)) # like this gives matrix, long x lat
# with nlayers 2 gives ncell x months

# now that we have the data matrix with less resolution
# we can start clustering

# 1st vanilla kmeans clustering

dim(cor(m)) #432 432, we have to transpose when doing cor
dim(cor(t(m))) #612 612 

# kmeans clustering
?kmeans

plot_kmeans <- function(data, kmeans_solution) {
  df <- base::as.data.frame(cbind(coordinates(data), kmeans_solution$cluster))
  colnames(df) <- c("Longitude","Latitude", "Cluster")
  df$Cluster <- factor(df$Cluster)
  plt <- ggplot(data = df, aes(x = Longitude, y = Latitude, fill = Cluster)) +
    annotation_map(map_data("world")) +
    geom_raster(interpolate = TRUE)
  plt
}

undebug(plot_kmeans)
c1 <- kmeans(m, 3)
p1 <- plot_kmeans(precip, c1)

c2 <- kmeans(m, 4)
c3 <- kmeans(m, 5)
c4 <- kmeans(m, 6)
c5 <- kmeans(m, 7)

c1$tot.withinss
c2$tot.withinss
c3$tot.withinss
c4$tot.withinss
c5$tot.withinss

c200 <- kmeans(m, 200)
p200 <- plot_kmeans(precip, c200)

plot_kmeans(precip, c5)

cx <- kmeans(m,50)
px <- plot_kmeans(precip, cx)
#######################
set.seed(123)

k_values <- seq(10,100,5)

multi_kmeans_wss <- function(data, k_values) {
  wss_values <- c()
  for(i in k_values) {
    wss_values <- append(wss_values, kmeans_wss(data,i))
  }
  return(wss_values)
}

wss_values <- multi_kmeans_wss(m, k_values)

plot(k_values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


#######################
# use kmeans with correlation as distance

tm <- t(m)
cor_tm <- cor(tm)
dim(cor_tm)

cc1 <- kmeans(cor_tm, 3)
plot_kmeans(precip, test)

c_wss_val <- multi_kmeans_wss(cor_tm, k_values)

plot(k_values,c_wss_val,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

cc15 <- kmeans(cor_tm, 15)
plot_kmeans(precip, cc15)

#######################

#######################
#TODO idea: use long and lat and ts as variables
#yeeaaaaah

#TODO look at all timeseries
#TODO look at mean precipitation and plot (again)
#TODO summarise the timeseries and use summary
# statistics for clustering, mean rain value,
# amplitude of periodicy, steepness of trend etc.
#TODO smooth ts before clustering?, use stl()?

# kmedoids in R 
###########################
install.packages("cluster")
library(cluster)
test <- pam(m, )

set.seed(123)

# function to compute total within-cluster sum of square 
wss_pam <- function(k) {
  pam(m, k, metric = "euclidean", stand = FALSE,
      nstart = 10, keep.diss = FALSE, keep.data = FALSE)$silinfo$avg.width
}
test <- wss_pam(2)
# Compute and plot wss for k = 1 to k = 15
k.values_pam <- seq(1:10)
wss_values_pam <- c(NA)

test_function <- function(k.values_pam, wss_values_pam) {
  wss_values_pam <- c()
  for(i in k.values_pam) {
    wss_values_pam <- append(wss_values_pam, wss_pam(i))
  }
}
test_function(k.values_pam, wss_values_pam)
pam_best <- which.max(wss_values_pam)
wss_values_pam <- append(wss_values_pam,0.20)

plot(k.values_pam, wss_values_pam,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

plot(1:10, wss_values_pam, type= "h", main = "pam() clustering assessment",
     xlab= "k  (# clusters)", ylab = "average silhouette width")
axis(1, pam_best, paste("best",pam_best,sep="\n"), col = "red", col.axis = "red")

pam_1 <-
  
  
###########################

# only 2 first years of data
cor_mat <- cor(t(m))
dist_mat <- as.dist(cor_mat)
dim(dist_mat)
hc <- hclust(dist_mat, method = "average")
plot(hc)

# with heatmaps
library(pheatmap)
cols_cor <- cor(m, method = "pearson")
rows_cor <- cor(t(m), method = "pearson")

pheatmap(m, scale = "column",
         clustering_distance_cols = as.dist(1-cols_cor),
         clustering_distance_rows = as.dist(1-rows_cor))

# TODO how to make sense of that? Need to learn



dim(precip)
precip[1:5,1:4]
cor(precip[1,], precip[2,])
install.packages("dtw")
library(dtw)
dist(rbind(precip[1,], precip[2,]), method = "DTW")
dist(precip[1:50,], method = "DTW")

install.packages("TSclust")
library(TSclust)
get_dis_mat <- function(dataset, method) {
}

dim(cor(precip[1:5,1:4]))
?cor
m <- matrix(rbind(c(1:4),c(2,4,6,8)), nrow = 2)
n <- matrix(rbind(c(1,2,3,4), c(4,3,2,1)), nrow = 2)
cor(n)

# only 2 first years of data
cor_mat <- cor(t(precip[1:50,1:24]))
dist_mat <- as.dist(cor_mat)
dim(dist_mat)
hc <- hclust(dist_mat, method = "average")
plot(hc)





# Step 1, compute dissimilarity matrix
# possibility 1 use classic correlation
# possibility 2 use dynamic time warping
# possibility + weight similiarity based on distance
#   so, similarity 1 and 3 will be divided by their distance

# Step 2, clustering
# f.e. kmeans
?kmeans()
# kmeans applicable directly to time series data



