# glyph helpers

# helpers directly from the paper climate repo####
range01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

max1 <- function(x) {
  x / max(x, na.rm = TRUE)
}
mean0 <- function(x) {
  x - mean(x, na.rm = TRUE)
}
min0 <- function(x) {
  x - min(x, na.rm = TRUE)
}


rescale01 <- function(x, xlim=NULL) {
  if (is.null(xlim)) {
    rng <- range(x, na.rm = TRUE)
  } else {
    rng <- xlim
  }
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale11 <- function(x, xlim=NULL) 2 * rescale01(x, xlim) - 1

getbox <- function (map, xlim, ylim) {
  # identify all regions involved
  small <- subset(map, (long > xlim[1]) & (long < xlim[2]) & (lat > ylim[1]) & (lat < ylim[2]))
  regions <- unique(small$region)
  small <- subset(map, region %in% regions)	
  
  # now shrink all nodes back to the bounding box
  small$long <- pmax(small$long, xlim[1])
  small$long <- pmin(small$long, xlim[2])
  small$lat <- pmax(small$lat, ylim[1])
  small$lat <- pmin(small$lat, ylim[2])
  
  # Remove slivvers
  small <- ddply(small, "group", function(df) {
    if (diff(range(df$long)) < 1e-6) return(NULL)
    if (diff(range(df$lat)) < 1e-6) return(NULL)
    df
  })
  
  small
}

# helpers, self-written 
layer_to_month <- function(df_col) {
  month_col <- as.numeric(stringr::str_replace(df_col, "X", ""))
  return(month_col)
}
#precip_df$month <- layer_to_month(precip_df$layer)
# 
month_to_year <- function(df_col) {
  return(df_col %/% 12 + 1900)
}
#precip_df$year <- month_to_year(precip_df$month)

month_to_cyc <- function(df_col) {
  cyc_col <- (df_col+1) %% 12
  cyc_col[cyc_col==0] <- 12
  return(cyc_col)
}
#precip_df$cyc_month <- month_to_cyc(precip_df$month)

prepare_df_glyph <- function(df) {
  df$month <- layer_to_month(df$layer)
  df$month <- as.integer(df$month)
  df$year <- month_to_year(df$month)
  df$cyc_month <- month_to_cyc(df$month)
  return(df)
}

