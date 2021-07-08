getwd()
setwd("Repos/MA-climate/")

# also leveltplot() for mean_test and mean_test2

# for the shapes
# http://thematicmapping.org/downloads/world_borders.php
shp_file <- "data/shape-files/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp"
world_shape <- read_sf(shp_file)
outline <- as(st_geometry(world_shape), Class = "Spatial")
plot(outline, col="gray80", lwd=1)

pproj <- proj4string(outline)
test_proj <- spTransform(mean_test2,proj)


library(raster)
test <- brick("data/interim/sst/ersst_setreftime.nc",
              varname= "ssta")
mean_test <- calc(test, fun=mean,na.rm=TRUE)
plot(mean_test)

test2 <- brick("data/interim/drought/chirps_setreftime.nc")
mean_test2 <- calc(test2, fun=mean, na.rm=TRUE)
plot(mean_test2)


library(tabularaster)
mt2 <- as_tibble(mean_test2, xy = TRUE)

#now we can use
plot(outline)
plot(mt2$x,mt2$y)

ggplot(mt2) + geom_raster(aes(fill=cellvalue))
ggplot(mt2, aes(x=x, y=y, fill=factor(cellvalue))) +
  geom_raster(interpolate = TRUE) +
  scale_fill_viridis_d(option="A") +
  theme_bw() +
  coord_quickmap() 

gplot(mean_test2) + geom_tile(aes(fill=cellvalue), alpha = 0.8)

ggplot(mean_test2)

gplot(mean_test) + geom_tile(aes(fill=value))+scale_fill_gradient(low="white", high="blue") +
  coord_map()
gplot(mean_test2) + geom_tile(aes(fill=cellvalue))+scale_fill_gradient(low="white", high="blue") +
  coord_map()

ggplot(data=world) +
  geom_sf(aes(fill=mean_test2)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

#####################

pr <- proj4string(outline)
mt2 <- projectRaster(raster(mean_test2), crs = pr)

plot(outline)
points(mean_test2@data@values)

library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data=world) +
  geom_sf(aes(fill=mean_test2@data@values))+
  scale_fill_viridis_c(option="plasma")

# rasterVis plot
# load packages  
library(sf) 
library(ncdf4)
library(raster)
library(rasterVis)
library(RColorBrewer)

mapTheme <- rasterTheme(region = rev(brewer.pal(10, "RdBu")))
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)

plt <- levelplot(mean_test2, margin = F, at=cutpts, cuts=11, pretty=TRUE, par.settings = mapTheme,
                 main="January temperature")
plt + layer(sp.lines(outline, col="black", lwd=1.0))

