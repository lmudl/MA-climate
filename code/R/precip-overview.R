# TODO sort plots, means, sd and densities
# 


# inspect the time series of precipitation
# plot overall mean?
# plot trend after using stl?
# plot size of amplitudes in seasonal component?
apply(m, 1, mean)

test <- apply(precip@data@values, 1, mean)
test[1]
r_mean[1,1]

r_mean <- calc(precip, mean)
plot(r_mean)

r_sd <- calc(precip, sd)
plot(r_sd)

# development of yearly mean over time?
library(stlplus)
stl(m[1,])

plot(m[1,])
plot(ts(m[1,]))

###

plot_summary <- function(data, summary) {
  df <- base::as.data.frame(cbind(coordinates(data), summary@data@values))
  colnames(df) <- c("Longitude","Latitude", "val")
  plt <- ggplot(data = df, aes(x = Longitude, y = Latitude, fill = val)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_gradient2(low = "red", high = "blue", midpoint = mean(df$val))
  plt
}

plot_summary(precip, r_mean)
plot_summary(precip, r_sd)
####
a <- density(r_mean)
a <- data.frame(a$x)
ggplot(a, aes(x = a.x)) + geom_density()
plot(density(r_mean))

df <- data.frame(means = values(r_mean))
ggplot(df, aes(x = means)) + geom_density()

df <- data.frame(sd = values(r_sd))
ggplot(df, aes(x = sd)) + geom_density()
####

max(values(r_mean))
min(values(r_mean))


