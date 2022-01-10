# Plan for clustering

# PRIO 1!
# TODO compare gap statistics

# TODO study time series more in detail
# TODO helper function for plots of clustering in general
# TODO add longitude latitude as variable
# TODO summarise time series with mean etc and add as variables


# kmeans, kmedoid, hierarchical clustering, agglomerative, each with 3 links
# dtw and the other fancy one too

# load data
# turn into matrix
# aggregate

# kmeans
# TODO answer question: when do cluster do not form spatial area anymore
# TODO kmeans based on correlation distance, NOTE transpose before cor()
# TODO answer question: which metric to minimise?
# TODO answer question: should we standardise?
# TODO decide which kmeans plots to show:
#   which number of clustersfor the map?
#   which sequence for total within clusters sum of squares
#   maybe 1:10, 10:100 (10,20,30,..)
# TODO kmeans with adding longitude and latitude as variables

# NOTE nstart nstart option attempts multiple initial configurations and 
#   reports on the best one. For example, adding nstart=25 will generate 25 
#   initial random centroids and choose the best one for the algorithm.
# https://datascience.stackexchange.com/questions/11485/k-means-in-r-usage-of-nstart-parameter
# https://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/
# helper function plot_kmeans
# kmeans for different clusters
# kmeans with corrlation as distance

