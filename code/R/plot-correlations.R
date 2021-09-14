rm(list=ls())
set.seed(1234)
library(raster)
library(ggplot2)
#library(ggtext)
library(dplyr)
library(ncdf4)

getwd()
setwd("Repos/MA-climate/")
# load data
precip <- readRDS("data/processed/clean_deseasonalised_precip_data.rds")
sst <- readRDS("data/processed/clean_deseasonalised_sst_data.rds")
dim(precip)
dim(sst)

# option front or back cut
# for precip front cut
# for sst back cut
cut_matrix <- function(matrix, timelag, side=c("front,back")) {
  if(side == "front") {
    matrix <- matrix[, -c(1:timelag)]
  }
  if(side == "back"){
    matrix <- matrix[, 1:(ncol(matrix)-timelag)]
  }
  return(matrix)
}

# compute correlations
# for different time lags what do we need?
# in: precip path, sst path, time lags
# out: correlation vector
# load precip and sst decomposed
# compute precip means
# remove precip data
# compute correlations with time lags
# correlations out

compute_corr <- function(dec_matrix_sst, dec_matrix_precip, timelag=0) {
  if (timelag!=0) {
    dec_matrix_sst <- cut_matrix(dec_matrix_sst, timelag, "back")
    dec_matrix_precip <- cut_matrix(dec_matrix_precip, timelag, "front")
  }
  assertthat::are_equal(ncol(dec_matrix_precip), ncol(dec_matrix_sst))
  precip_means <- apply(dec_matrix_precip, 2, mean)
  cor_vec <- apply(dec_matrix_sst, 1, function(x) cor(x, precip_means))
  return(cor_vec)
}


#corr0 <- compute_corr(sst, precip,0)
# create matrix/raster
old_sst <-  brick("data/interim/sst/ersst_setreftime.nc",
                  varname= "sst")


# plot correlation 
# in: correlation vector, old sst data, timelag for legend
plot_corr <- function(corr_vec, old_sst, timelag,
                      quantiles = c(TRUE,FALSE)) {
  corr_grid <- matrix(corr_vec, nrow = old_sst@nrows, ncol = old_sst@ncols, byrow = TRUE)
  corr_raster <- raster(corr_grid,
                        xmn = old_sst@extent@xmin, xmx = old_sst@extent@xmax,
                        ymn = old_sst@extent@ymin, ymx = old_sst@extent@ymax,
                        crs = old_sst@crs)
  df <- cbind(xyFromCell(corr_raster, 1:ncell(corr_raster)), values(corr_raster))
  df <- base::as.data.frame(df)
  colnames(df) <- c("Longitude","Latitude", "Correlation")
  if(quantiles) {
    q <- quantile(df[,"Correlation"],probs=c(0.025,0.975), na.rm = TRUE)
    df$Correlation[df$Correlation>q["2.5%"] & df$Correlation<q["97.5%"]] <- 0
  }
  plt <- ggplot(data = df, aes(x = Longitude, y = Latitude, fill = Correlation)) +
    annotation_map(map_data("world")) +
    geom_raster(interpolate = TRUE)
  if(!quantiles) {
    plt <- plt +
      ggtitle(paste("Correlation of Sea Surface Temperature and Precipitation for Timelag",timelag))
  } 
  if(quantiles) {
    plt <- plt +
      ggtitle(paste0("Correlation of Sea Surface Temperature and Precipitation for Timelag"," ",timelag,
                      ",\nValues between quantiles threshold set to 0"))
    
  }
  plt <- plt +
    scale_fill_viridis_c(option="A") +
    theme_bw() +
    coord_quickmap() 
  return(plt)
}

compute_corr_and_plot <- function(
  dec_matrix_sst, dec_matrix_precip, old_sst,
  timelag=0,quantiles = c(TRUE,FALSE)) {
  
  corr_vec <- compute_corr(dec_matrix_sst, dec_matrix_precip, timelag)
  plt <- plot_corr(corr_vec, old_sst, timelag, quantiles)
  return(plt)                                 
}


plot_density <- function(corr_vec, timelag) {
  df <- data.frame(corr_vec)
  plt <- ggplot(df, aes(x=corr_vec)) + geom_density() + ggtitle(paste("Density Plot of Correlations with timelag",timelag))
  plt
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#aspect_ratio <- 1
corr0_vec <- compute_corr(sst, precip,0)
dens0 <- plot_density(corr0_vec, timelag = 0)
#ggsave("plots/dens0.jpg",dens0)
corr0 <- plot_corr(corr0_vec, old_sst, timelag = 0, quantiles = FALSE)
#ggsave("plots/corr0.jpg", corr0)
corr0q <- plot_corr(corr0_vec, old_sst, timelag = 0, quantiles = TRUE)
#ggsave("plots/corr0q.jpg", corr0q)

library(jpeg)
library(grid)
library(gridExtra)
p1 <- readJPEG("plots/dens0.jpg")
p2 <- readJPEG("plots/corr0.jpg")
p3 <- readJPEG("plots/corr0q.jpg")
grid.arrange(rasterGrob(p1),
             rasterGrob(p2),
             rasterGrob(p3), ncol = 1)

multiplot("",dens0,"", cols = 3) + plot_layout
grid

par(mfrow=c(3,1))
dens0
corr0
corr0q
ggsave("plots/testmultiple.png")

layout(matrix(c(1,1,1), 3,1, byrow = TRUE))
par(mfrow=c(3,1))
require(gridExtra)
grid.arrange(dens0,corr0,corr0q, ncol = 1,
             widths=c(0.5,1,1))
grid.arrange(NA, dens0, NA,
             NA, corr0, NA,
             NA, corr0q, NA, ncol = 3)
grid.arrange(grobs = c(dens0, corr0,corr0q),
             widths = c()
             layout_matrix = rbind)
g0 <- ggplotGrob(dens0)
ggplotGrob(0)
grid.arrange(grobs = c(g0),
             widths = c(1,1.1,1),
             layout_matrix = matrix(c(NULL,1,NULL), nrow = 1))


p1 <- plot_grid(plot_density(corr0),rel_widths = c(0.25))
plot_corr(corr0, old_sst,0,FALSE)
plot_corr(corr0, old_sst,0,TRUE)
?grid.arrange



#install.packages("cowplot")
library(cowplot)
plot_grid(dens0, corr0, corr0q, ncol = 1, 
          rel_heights = c(0.75,1,1))

p1 <- plot_grid(NULL,dens0,NULL, rel_widths = c(1,1.1,1),nrow=1)
plot_col1 <- plot_grid(p1, corr0,corr0q, ncol = 1)

create_plot_col <- function(dens_plot, corr_plot, corrq_plot) {
  p1 <- plot_grid(NULL,dens_plot, NULL, rel_widths = c(1,1.1,1), nrow = 1)
  plot_col <- plot_grid(p1, corr_plot, corrq_plot, ncol = 1)
  return(plot_col)
}

plot_col0 <- create_plot_col(dens0, corr0, corr0q)

corr3_vec <- compute_corr(sst, precip, 3)
dens3 <- plot_density(corr3_vec, timelag = 3)
ggsave("plots/dens3.jpg")
corr3 <- plot_corr(corr3_vec,old_sst,3,FALSE)
ggsave("plots/corr3.jpg")
corr3q <- plot_corr(corr3_vec,old_sst,3,TRUE)
#ggsave("plots/corr3q.jpg")

#plotcol3 <- create_plot_col(dens3, corr3, corr3q)
ggsave("plots/coltest.jpg")
corr6 <- compute_corr(sst, precip, 6)
dens6 <- plot_density(corr6, timelag = 6)
ggsave("plots/dens6.jpg")
plot_corr(corr6,old_sst,6,FALSE)
ggsave("plots/corr6.jpg")
plot_corr(corr6,old_sst,6,TRUE)
ggsave("plots/corr6q.jpg")

corr9 <- compute_corr(sst, precip, 9)
dens9 <- plot_density(corr9, timelag = 9)
ggsave("plots/dens9.jpg")
plot_corr(corr9,old_sst,9,FALSE)
ggsave("plots/corr9.jpg")
plot_corr(corr9,old_sst,9,TRUE)
ggsave("plots/corr9q.jpg")

corr12 <- compute_corr(sst, precip, 12)
dens12 <- plot_density(corr12, timelag = 12)
ggsave("plots/dens12.jpg")
plot_corr(corr12,old_sst,12,FALSE)
ggsave("plots/corr12.jpg")
plot_corr(corr12,old_sst,12,TRUE)
ggsave("plots/corr12q.jpg")





#####

df <- data.frame(corr0)
head(df)
p <- ggplot(df, aes(x=corr0)) + geom_density()
plot(density(na.omit(corr0)))
plot0 <- plot_corr(corr0, old_sst, 0, TRUE)
t <- compute_corr_and_plot(sst,precip,old_sst,0,FALSE)
corr1 <- compute_corr(sst, precip,1)
plot(density(na.omit(corr1)))
plot1 <- plot_corr(corr1, old_sst,1,FALSE)
plot1q <- plot_corr(corr1, old_sst, 1, TRUE)





