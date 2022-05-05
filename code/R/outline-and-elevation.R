library(raster)
library(ggplot2)
library(elevatr)
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
km_pca_result <- readRDS("results/clustering/km_pca_result.rds")
df <- as.data.frame(cbind(coordinates(precip), cluster = km_pca_result$cluster))
r <- rasterFromXYZ(df)
pp <- rasterToPolygons(r, dissolve = TRUE)
outline <- fortify(pp)
saveRDS(outline, "results/clustering/km_cluster_outline.rds")
# only outlines
ggplot(df, aes(x, y)) +
  coord_fixed() +
  geom_path(aes(x = long, y = lat, group = group), data = outline, 
            size=1.5, col="white")

# together
elevation <- get_elev_raster(precip, z = 1)
elevation <- crop(elevation, precip)
elevation <- raster::as.data.frame(elevation, xy=TRUE)
names(elevation)[3] <- "elev"
elev_and_cluster_outline_plot <- ggplot() + 
  geom_raster(data=elevation, aes(x=x, y=y, fill=elev)) +
  scale_fill_viridis_c() +
  geom_path(aes(x = long, y = lat, group = group), data = outline, 
            size=1.5, col="white") +
  coord_quickmap() 
saveRDS(elev_and_cluster_outline_plot, "results/clustering/elev_and_cluster_outline_plot.rds")



