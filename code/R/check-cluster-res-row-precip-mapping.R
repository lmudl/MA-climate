kmeans_result
dim(precip)
# 18*34 also lat mal long, zeilen mal spalten,
# starts top left goes top right and then
# goes down each row for the clusters.
m <- matrix(kmeans_result$cluster, nrow=18, ncol=34, byrow = TRUE)
View(m)

v <- getValues(precip)
dim(v)

# we assign the rows to the clusters.
rownames(v)
?getValues
# rows representing cells, columns representing layers
coordinates(precip)
# this matrix starts top right goes top left, than goes one row down
# one latitude down
v <- raster::as.data.frame(precip, xy=TRUE)
dim(v)
head(v)[,1:10]

# Summary why clustering annotation works
# when we do raster::as.data.frame(precip, xy = TRUE)
# we see that the coordinates for the rows run over the x why they keep the y
# fixed. X is longitude, y is latitude.
# So it goes from top right to top left and then to the next row below.
# We can see from the kmenas result cluster plot that it works for the
# the cluster results the same way.
# If we fill the kmeans result cluster vector to a matrix byrow TRUE
# we obtain the same result as in the cluster plot




