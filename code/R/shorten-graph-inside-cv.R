setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(raster)
library(igraph)
getwd()
# lets do this for fold one ####
ids <- readRDS("results/CV-fused/large-fused-20k/index-list.rds")
sst <- readRDS("data/processed/sst_cv.rds")


f1_sst <- sst[ids$train$Training060,]
f1_sst <- standardize_train(f1_sst)
table(round(attr(f1_sst, "scale"),1))


sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst <- calc(sst, custom_stand)
sst <- calc(sst, sdN)
plot_summary(sst)
any(is.na(getValues(sst)))


# lets say I get from  ####
ids <- readRDS("results/CV-fused/large-fused-20k/index-list.rds")

sst_og <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst <- readRDS("data/processed/sst_cv.rds")
f1_sst <- sst[ids$train$Training060,]
mean_vec <- apply(f1_sst, 2, mean)
sdn_vec <- apply(f1_sst, 2, sdN)
stand_f1_sst <- scale(f1_sst, center = mean_vec,
                      scale = sdn_vec)
# stand_f1_sst <- standardize_train(f1_sst)


dim(f1_sst)
dim(stand_f1_sst)

# we drop around 800 variables
igraph_from_raster(f1_sst)
debug(igraph_from_raster)
class(sst_og)
class(sst)
which(is.na(stand_f1_sst))

# if there are new vertices to be dropped
# take existing graph and drop the vertices too
# so from the fold i get new cols that have NA
# then I just drop from the old graph the vertices?
g <- readRDS("data/processed/graph_sst.rds")
plot(g, vertex.size = 0.001, edge.width=0.00001, labels=NULL)


# so from sst with NA i create graph # 
# from sst w/o NA i put data on the graph
# when now other data from 
vec <- apply(stand_f1_sst, 2, function(x) all(is.na(x)))
length(vec)
sum(vec)
g2 <- delete_vertices(g, vec) 
vcount(g)
vcount(g2)
plot(g, vertex.size = 0.001, edge.width=0.00001, vertex.label=NA)
plot(g2, vertex.size = 0.001, edge.width=0.00001, vertex.label=NA)

x_train <- standardize_train(x_train)
x_test <- standardize_test(x_train, x_test)
is.na(attr(alt, "scale"))



     