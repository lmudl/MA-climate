# fit seasonal models
fit_seasonal_model <- function(sst_df) {
  mods <- plyr::dlply(dplyr::select(sst_df,long,lat,sst,year,cyc_month)
                      ,c("long", "lat"), function(df) {
                        lm(sst ~ year + factor(cyc_month), data = df)
                      })
  return(mods)
}

# prepare grids
create_month_grid <- function(sst_df) {
  grid <- expand.grid(year=max(sst_df$year)+1, cyc_month=1:12)
}

# get predictions
get_preds <- function(models, grid) {
  ldply(models, function(mod) {
    grid$pred <- predict(mod, newdata = grid)
    grid
  })
}

# ggplot seasonalities
world <- map_data("world")
ggworld <- list(
  geom_polygon(aes(long, lat, group = group), inherit.aes = FALSE, 
               data = world, show.legend = FALSE, fill = "grey80", colour = "grey90"),
  scale_x_continuous(expand = c(0.02, 0)),
  scale_y_continuous(expand = c(0.02, 0)), 
  xlab(NULL),
  ylab(NULL))


gg_glyph <- function(preds, worldmap = FALSE) {
  p <- ggplot(preds, aes(gx, gy, group=gid)) +
    add_ref_boxes(preds, color = "grey90") +
    add_ref_lines(preds, color = "grey90") +
    geom_path() + 
    theme_bw() +
    labs(x = "", y = "")
  if(worldmap==TRUE) p <- p + ggworld
  return(p)
}

# get smoothed models
fit_smoothed_models <- function(sst_df) {
  smoothed_models <- dlply(dplyr::select(sst_df,long,lat,sst,month,cyc_month), 
                           c("long", "lat"), function(df) {
                             gam(sst ~ s(month) + factor(cyc_month), data = df)
                           })
  return(smoothed_models)
}

# create smooth grid
create_smooth_grid <- function(sst_df) {
  grid <- expand.grid(month = seq(min(sst_df$month), max(sst_df$month)),
                      length = 50, cyc_month = 1)
}

# scale predictions globally
scale_preds_globally <- function(preds) {
  scaled_preds <- ddply(preds, c("long", "lat"), mutate,
                        pred_s = rescale01(pred),
                        pred_m = pred / max(pred),
                        max = max(pred),
                        range = diff(range(pred)))
  return(scaled_preds)
}

# plot scaled 

gg_glyph_scale <- function(preds, grid){
  p <- ggplot(preds) +
    geom_tile(aes(long, lat, fill = range), data = grid, alpha = 0.5) + #alpha=0.5
    add_ref_boxes(preds, color= "grey90") +
    geom_path(aes(gx, gy, group = gid)) +
    theme_bw() + 
    scale_fill_gradient("sst\nrange",
                        high = "white", low = "#3B4FB8", limits = range(grid$range),
                        breaks = seq(round(min(grid$range),2), round(max(grid$range),2), by = 1), guide = guide_colourbar(
                          direction = "horizontal", title.vjust = 0.7))
  return(p)
}
