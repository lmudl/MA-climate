library(raster)
library(dplyr)
library(ggplot2)
data <- stack("data/raw/cru/scPDSI-1901-2019.nc")
data
dim(data)
data <- raster::subset(data, grep("2003.06.", names(data), value = TRUE))
df <- as.data.frame(data, xy = TRUE)
str(df)

df <- df %>% rename(
  drought = X2003.06.01,
  longitude = x,
  latitude= y
)
df %>% filter(latitude < 0 & -9 < latitude & -72 < longitude & longitude < - 55) -> df

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
# use the jet colormap
# limits orientated on limits of temp
ggplot() +
  geom_raster(data = df, aes(x=longitude, y=latitude, fill=drought)) + 
  scale_fill_gradientn(colors = jet.colors(7), limits = c(-4, 6)) + 
  borders() +
  coord_quickmap()

