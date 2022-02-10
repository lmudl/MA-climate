# cluster precipitation data
# load precip
setwd("Repos/MA-climate/")
# precip <- readRDS("data/processed/deseasonalised_precip.rds")
# use raw precip
# load raw precip

source("code/R/helper-functions.R")
library(raster)
library(ggplot2)
library(cluster)
#library(dplyr)
#library(ncdf4)
k_values <- seq(10,100,5)

# load data #####
# precip <- brick("data/interim/drought/chirps_setreftime.nc")
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
nlayers(precip) # 432
# current data resolution
res(precip) # 0.05000001, 0.05

# Step 0, change data resolution
# degrees from preprocess data  degrees = c("-72,-55,-9,0"))
dim(precip) # 61200 432, meaning we have 61200 cells for 432 months
# ? how to get new resolution when we dont have 2d x times strucure any more

#precip <- raster::aggregate(precip, fact = 10) # aggregate, both dim factor 10 res less
dim(precip) # 18, 34, 432
m <- brick_to_matrix_wna(precip, nlayers = nlayers(precip)) # like this gives matrix, long x lat
# with nlayers 2 gives ncell x months

# now that we have the data matrix with less resolution
# we can start clustering

# 1st vanilla kmeans clustering #####

dim(cor(m)) #432 432, we have to transpose when doing cor
dim(cor(t(m))) #612 612 

## kmeans clustering, helpers ####

plot_kmeans <- function(data, kmeans_solution) {
  df <- base::as.data.frame(cbind(coordinates(data), kmeans_solution$cluster))
  colnames(df) <- c("Longitude","Latitude", "Cluster")
  df$Cluster <- factor(df$Cluster)
  plt <- ggplot(data = df, aes(x = Longitude, y = Latitude, fill = Cluster)) +
    annotation_map(map_data("world")) +
    geom_raster(interpolate = TRUE)
  plt
}

kmeans_res <- function(data, k) {
  res <- kmeans(data, k, nstart = 10)
  wss <- res$tot.withinss
  iter <- res$iter
  return(list("wss"=wss,"iter"=iter))
}

multi_kmeans_wss <- function(data, k_values) {
  wss_values <- c()
  iter_values <- c()
  for(i in k_values) {
    res_i <- kmeans_res(data,i)
    wss_values <- append(wss_values, res_i$wss)
    iter_values <- append(iter_values, res_i$iter)
  }
  return(list("wss"=wss_values, "iter"=iter_values))
}
## plot clusters #####
set.seed(1234)
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
## compute cluster wss and check iter #####
set.seed(1234)
multi_res <- multi_kmeans_wss(getValues(precip), k_values)
wss_values <- multi_res$wss
iter_values <- multi_res$iter #all smaller 10,fine

plot(k_values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

## gap statistic kmeans vanilla #####
set.seed(1234)
# old gap stat with more B and K.max
# saveRDS(gap_stat, file = "results/gap_stat.RDS")
# gap_stat <- readRDS("results/gap_stat.RDS")

# gap_stat_b20 <- clusGap(getValues(precip), FUN = kmeans, nstart = 10,
#                     K.max = 20, B = 20, iter.max = 30)
# tibshirani wrote 20 simulations is enough?
# saveRDS(gap_stat_b20, file = "results/gap_stat_b20.rds")
gap_stat_b20 <- readRDS("results/gap_stat_b20.rds")
#install.packages("factorextra")
kmeans_gap_plot <- factoextra::fviz_gap_stat(gap_stat_b20)

set.seed(1234)
# kmeans_gap_b50 <- clusGap(getValues(precip), FUN = kmeans, nstart = 10,
#                          K.max = 20, B = 50, iter.max = 30)
# saveRDS(kmeans_gap_b50, file = "results/kmeans_gap_b50.rds")
kmeans_gap_b50 <- readRDS("results/kmeans_gap_b50.rds")

kmeans_gap_b50_plot <- factoextra::fviz_gap_stat(kmeans_gap_b50)
saveRDS(kmeans_gap_b50_plot, "results/clustering/kmeans_gap_b50_plot.rds")

# for 20 and 50 stable 

## get optimal k by hand #####
tab <- as.matrix(gap_stat$Tab)
length(tab[,"gap"])
gk1 <- tab[-1,"gap"]

d <- tab[,"gap"] - tab[,"SE.sim"]
d <- d[1]

g <- tab[,"gap"]
g <- g[-length(g)]

which.max(d - g)

which.min(diff(g))
diff(g) # choose first with neg diff, here 41
# because none satisfies the prerequisite that it
# drops under one SE difference the next step

## silhouette#####
factoextra::fviz_nbclust(m, kmeans, method = "silhouette")

## wss#####
factoextra::fviz_nbclust(m, kmeans, method = "wss")
?fviz_nbclust

# kmeans on normalised data ######
# normalise data, center and scale
# base::scale() scales columns so we need to transpose?
# really? actually no I think!
scaled_precip <- scale(getValues(precip), center = TRUE, scale = TRUE)
dim(scaled_precip)

set.seed(1234)
scaled_res <- multi_kmeans_wss(scaled_precip, k_values)
scaled_wss <- scaled_res$wss
scaled_iter <- scaled_res$iter


plot(k_values, scaled_wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

## gap statistic kmeans normalised ####
# set.seed(1234)
# nstart 10, K.max = 100 (20 besser?), B=100 (20 besser?)
## my_kmeans <- function(x,k) list(cluster=kmeans(x,k, iter.max = 30, nstart=10))
# norm_kmean_gap_stat <- clusGap(scaled_precip, FUN = kmeans, B = 20, K.max = 20, nstart = 10,
#                                 iter.max = 30)
# saveRDS(norm_kmean_gap_stat,"results/norm_kmean_gap_stat.rds")
norm_kmean_gap <- readRDS("results/norm_kmean_gap_stat.rds")
norm_kmean_gap_plot <- factoextra::fviz_gap_stat(norm_kmean_gap)
saveRDS(norm_kmean_gap_plot, "results/clustering/norm_kmean_gap_plot.rds")
# set.seed(1234)
# norm_kmean_gap_b50 <- clusGap(scaled_precip, FUN = kmeans, B = 50, K.max = 20, nstart = 10,
#                               iter.max = 30)
# saveRDS(norm_kmean_gap_b50, "results/norm_kmean_gap_b50.rds")
norm_kmean_gap_b50 <- readRDS("results/norm_kmean_gap_b50.rds")
norm_kmean_gap_b50_plot <- factoextra::fviz_gap_stat(norm_kmean_gap_b50)

# on normalised pretty stable too

# kmeans parrallelised#################
## register cores ####
require(doParallel)
cores <- makeCluster(detectCores(), type='PSOCK')
system <- Sys.info()['sysname']
cl <- NULL
if (system == 'Windows') {
  cl <- makeCluster(getOption('cl.cores', cores))
  registerDoParallel(cl)
  registerDoSEQ()
  on.exit(stopCluster(cl))
} else {
  options('mc.cores' = cores)
  registerDoParallel(cores)
}
## clusGAPKB ####

test_gap <- clusGapKB(scaled_precip, FUNcluster = my_kmeans, K.max = 30, B = 20)

## ?use kmeans with correlation as distance########

tm <- t(getValues(precip))
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



# TODs ######################
#TODO idea: use long and lat and ts as variables
#yeeaaaaah

#TODO look at all timeseries
#TODO look at mean precipitation and plot (again)
#TODO summarise the timeseries and use summary
# statistics for clustering, mean rain value,
# amplitude of periodicy, steepness of trend etc.
#TODO smooth ts before clustering?, use stl()?


# kmedoids helpers #####
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


?pam

# kmedoids more analysis ####
# pam gives possibility to standardise before compute euclidean
# distance

# eucl distance is dissimilarity
# small values, small dissimilarity, large similarity
# see ESL p 504, first standardize then eucl dist is corr

# we could compute cor once for unnormalised and once
# for normalised data
set.seed(1234)
# med_gap <- clusGap(scaled_precip, FUN = pam, K.max = 20,
#                    B = 20, nstart = 10, keep.diss = FALSE,
#                    keep.data = FALSE, do.swap = FALSE)
#saveRDS(med_gap, "results/gap_stat_kmedoid.rds")

# med_gap_b20 <- clusGap(getValues(precip), FUN = pam, K.max = 20,
#                    B = 20, nstart = 10, keep.diss = FALSE,
#                    keep.data = FALSE, do.swap = FALSE,
#                    stand = TRUE)
# saveRDS(med_gap_b20, "results/med_gap_b20.rds")
med_gap_b20 <- readRDS("results/med_gap_b20.rds")
med_gap_b20_plot <- factoextra::fviz_gap_stat(med_gap_b20,
                                              maxSE = list(method="Tibs2001SEmax"))

set.seed(1234)
# HERE 
# med_swap_gap_b20 <- clusGap(getValues(precip), FUN = pam, K.max = 20,
#                          B = 20 , nstart = 10, keep.diss = FALSE,
#                          keep.data = FALSE, do.swap = TRUE,
#                          stand = TRUE)
# saveRDS(med_swap_gap_b20, file = "results/med_swap_gap_b20.rds")
med_swap_gap_b20 <- readRDS("results/med_swap_gap_b20.rds")
med_swap_gap_b20_plot <- factoextra::fviz_gap_stat(med_swap_gap_b20)

#start 11:07, 11:48

set.seed(1234)
# med_gap_b50 <- clusGap(getValues(precip), FUN = pam, K.max = 20,
#                        B = 50, nstart = 10, keep.diss = FALSE,
#                        keep.data = FALSE, do.swap = FALSE,
#                        stand = TRUE)
# saveRDS(med_gap_b50, "results/med_gap_b50.rds")
med_gap_b50 <- readRDS("results/med_gap_b50.rds")
med_gap_b50_plot <- factoextra::fviz_gap_stat(med_gap_b50,
                                              maxSE = list(method="Tibs2001SEmax"))

# Do at home
set.seed(1234)
med_swap_gap_b50 <- clusGap(getValues(precip), FUN = pam, K.max = 20,
                            B = 50, nstart = 10, keep.diss = FALSE,
                            keep.data = FALSE, do.swap = TRUE,
                            stand = TRUE)
saveRDS(med_swap_gap_b50, "results/clustering/med_swap_gap_b50.rds")
med_swap_gap_b50_plot <- factoextra::fviz_gap_stat(med_swap_gap_b50, maxSE = list("Tibs2001SEmax"))
saveRDS(med_swap_gap_b50_plot, "results/clustering/med_swap_gap_b50_plot.rds")

# results also depend on choice of maxSE method!!
# do.swap also indicates that we cant find cluster
# like this
# so try out with pca


# PCA #####
# https://statologie.de/hauptkomponentenanalyse-r/
res <- prcomp(getValues(precip), scale = TRUE)

res$rotation <- -1*res$rotation
res$rotation
res$x <- -1*res$x
head(res$x)
var_exp <- res$sdev^2 / sum(res$sdev^2)
var_exp_plot <- var_exp[1:30]
scree_plot_pca <- qplot(c(1:30), var_exp_plot) + 
  geom_line() + 
  xlab(" Principal Component") + 
  ylab(" Variance Explained") +
  ggtitle(" Scree Plot") +
  ylim(0, 1)
saveRDS(scree_plot_pca, "results/clustering/scree_plot_pca.rds")

# used first 6 main components
dim(res$x)
sum(var_exp[1:3])
var_exp
main <- res$x[, 1:3]

set.seed(1234)
k_values <- c(1:20)
multi_res_pca <- multi_kmeans_wss(main, k_values)
wss_pca <- multi_res_pca$wss

plot(k_values, wss_pca,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# fviz_nbclust(main, FUNcluster = kmeans, k.max = 10)
# k <- fviz_nbclust(main, FUNcluster = kmeans,
#                  method = "gap_stat", k.max = 10)
# l <- fviz_nbclust(main, FUNcluster = pam,
#                   method = "gap_stat", k.max = 10)

# kmean gap statistic after pcr #####
set.seed(1234)
pca_km_gap <- clusGap(res$x[,1:5], FUN = kmeans, B = 50, K.max = 20, nstart = 10,
                      iter.max = 30)
saveRDS(pca_km_gap, "results/pca_km_gap.RDS")
p <- factoextra::fviz_gap_stat(pca_km_gap)

set.seed(1234)
pca_large <- clusGap(res$x[,1:5], FUN = kmeans, B = 100, K.max = 20, nstart = 10,
                      iter.max = 30)
(pl <- factoextra::fviz_gap_stat(pca_large))

set.seed(1234)
pca_six <-  clusGap(res$x[,1:6], FUN = kmeans, B = 50, K.max = 20, nstart = 10,
                    iter.max = 30)
ps <- factoextra::fviz_gap_stat(pca_six)

# influence different seeds? #####
set.seed(1233)
pca_six_b <- clusGap(res$x[,1:6], FUN = kmeans, B = 50, K.max = 20, nstart = 10,
                    iter.max = 30)
psb <- factoextra::fviz_gap_stat(pca_six_b)

set.seed(1234)
pca_six_large <- clusGap(res$x[,1:6], FUN = kmeans, B = 100, K.max = 20, nstart = 10,
                         iter.max = 30)
psl <- factoextra::fviz_gap_stat(pca_six_large)

# influence stand? ####
set.seed(1234)
pca_stand_gap <- clusGap(scale(res$x[,1:5]), FUN = kmeans, B = 50, K.max = 20, nstart = 10,
                      iter.max = 30)
p_stand <- factoextra::fviz_gap_stat(pca_stand_gap)

set.seed(1234)
pca_stand_large_gap <- clusGap(scale(res$x[,1:5]), FUN = kmeans, B = 100, K.max = 20, nstart = 10,
                         iter.max = 30)
p_stand_l <- factoextra::fviz_gap_stat(pca_stand_large_gap)

# influence gap pca after pca ####
set.seed(1234)
# set.seed(12345)
pca_after_pca <- clusGap(res$x[,1:5], FUN=kmeans,B=100, K.max = 20, nstart = 10,
                         iter.max = 30)
pca_double_plot <- factoextra::fviz_gap_stat(pca_after_pca)
set.seed(1234)
no_double_pca <-  clusGap(res$x[,1:5], FUN=kmeans,B=100, K.max = 20, nstart = 10,
                          iter.max = 30, spaceH0 = "original")
no_double_pca_plot <- factoextra::fviz_gap_stat(no_double_pca)
# pca_double_plot2 <- factoextra::fviz_gap_stat(pca_after_pca)





# influence number of pca components ####
set.seed(1234)
# from 3 to 5
clus_3 <- clusGap(res$x[,1:3], FUN=kmeans,B=100, K.max = 20, nstart = 10,
        iter.max = 30)
pc3_gap_plot <- factoextra::fviz_gap_stat(clus_3)
saveRDS(pc3_gap_plot, "results/clustering/pc3_gap_plot.rds")
set.seed(1234)
clus_4 <- clusGap(res$x[,1:4], FUN=kmeans,B=100, K.max = 20, nstart = 10,
                  iter.max = 30)
pc4_gap_plot <- factoextra::fviz_gap_stat(clus_4)
saveRDS(pc4_gap_plot, "results/clustering/pc4_gap_plot.rds")

factoextra::fviz_gap_stat(clus_4)
set.seed(1234)
clus_5 <- clusGap(res$x[,1:5], FUN=kmeans,B=100, K.max = 20, nstart = 10,
                  iter.max = 30)
pc5_gap_plot <- factoextra::fviz_gap_stat(clus_5)
saveRDS(pc5_gap_plot, "results/clustering/pc5_gap_plot.rds")
# seems that seeds can change the results a bit
# but for 4 it seems stable NOT
# 4321 gives different results than 1234 as seed
# what happens when we increase number of simulations?
# setseed1234 and dim 4 i like though

# how would function for this look like? #####
# cluster alg, stand, seed, data, k.max, seed, save result etc.

# pam after pca
set.seed(1234)
pam_pca <- clusGap(res$x[,1:3], FUN = pam, K.max = 20,
                   B = 50, nstart = 10, keep.diss = FALSE,
                   keep.data = FALSE, do.swap = TRUE,
                   stand = TRUE)


# with coordinates addded as variable#####
test <- cbind(coordinates(precip), getValues(precip))
resa <- prcomp(test, scale = TRUE)

var_expa <- resa$sdev^2 / sum(resa$sdev^2)
var_expa <- var_expa[1:30]
qplot(c(1:30), var_expa) + 
  geom_line() + 
  xlab(" Principal Component") + 
  ylab(" Variance Explained") +
  ggtitle(" Scree Plot2") +
  ylim(0, 1)




# andere Möglichkeit, aber verzerrt wsl die Werte

testb <- cbind(coordinates(precip), scaled_precip)

resb <- prcomp(testb, scale = FALSE)

var_expb <- resb$sdev^2 / sum(resb$sdev^2)
var_expb <- var_expb[1:30]
qplot(c(1:30), var_expb) + 
  geom_line() + 
  xlab(" Principal Component") + 
  ylab(" Variance Explained") +
  ggtitle(" Scree Plot3") +
  ylim(0, 1)


# depr ###########################

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



