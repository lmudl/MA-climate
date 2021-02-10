library(raster)
library(dplyr)
library(ggplot2)
#data <- stack("data/raw/hadcrut/HadCRUT.5.0.1.0.analysis.anomalies.ensemble_mean.nc")
#data@layers
data <- stack("data/interim/hadcrut-interim.nc")
data <- raster::subset(data, 1)
dim(data)
#data <- raster::subset(data, grep("2003.06.", names(data), value = TRUE))
df <- as.data.frame(data, xy = TRUE)
str(df)

df <- df %>% rename(
  temp = names(df)[3],
  longitude = x,
  latitude= y
)
df

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
# use the jet colormap
# limits orientated on limits of temp
ggplot() +
  geom_raster(data = df, aes(x=longitude, y=latitude, fill=temp)) + 
  scale_fill_gradientn(colors = jet.colors(7), limits = c(-4, 6)) + 
  borders() +
  coord_quickmap()

sub <- raster::subset(data, grep('1980.06.', names(data), value = T))
range(df$temp, na.rm = TRUE)
range(df$temp)
max(df$temp, na.rm = TRUE)

# select only ocean
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_ocean.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
oceans <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_ocean",
                  stringsAsFactors=FALSE, verbose=FALSE)

test.points <- data.frame(lon = df$longitude, lat = df$latitude)
coordinates(test.points) <- ~lon+lat
proj4string(test.points) <- CRS(proj4string(oceans))
vec <- is.na(over(test.points, oceans)$featurecla)
df$temp[vec] <- NA
# looks not so nice but it's o for the start i guess

