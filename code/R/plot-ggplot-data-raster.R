# code for exampkle plots with raster and ggplot directly

library(raster)
library(dplyr)
library(ggplot2)

# input
# path <- "data/raw/hadsst/HadISST_sst.nc"
path <- "data/raw/sst/HadSST.nc"
# var_type <- "sst"
var_type <- "tos"
missing_type <- "_FillValue"
# load data
data <- stack(path)
# first infos about dimenstion, lat orientation etc,
data
dim(data)

# choose random layer
set.seed(420)
l <- sample(1:dim(data)[3], 1)
slice <- raster::subset(data, l)
df <- as.data.frame(slice, xy = TRUE)
head(df)
tail(df)

#rename variables
df <- df %>% rename(
  longitude = x,
  latitude= y
)
names(df)[3] <- var_type
head(df)

# get missing info
info <- nc_open(path)
na_val <- ncatt_get(info, var_type, missing_type)$value
vec <- df[,var_type] == na_val
df[,var_type] <- replace(df[,var_type], vec, NA)

#still weird values?
plot(density(df[,var_type], na.rm = TRUE))
# set value <-100 to NA
vec <- df[,var_type] < +100
df[,var_type] <- replace(df[,var_type], vec, NA)
plot(density(df$tos, na.rm = TRUE))

# df %>% filter(latitude < 0 & -9 < latitude & -72 < longitude & longitude < - 55) -> df
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
# use the jet colormap
# limits orientated on limits of temp
ggplot() +
  geom_raster(data = df, aes_string(x=quote(longitude), y=quote(latitude), fill=var_type)) + 
  scale_fill_gradientn(colors = jet.colors(7), limits = c(-33, 33)) + 
  borders(database = "world") +
  coord_quickmap()
plot(hist(df$tos))
head(df)
