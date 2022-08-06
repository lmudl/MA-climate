# helper functions
################## Meta Data ######################################
# needs ncdf4
# In: f.e. path <- "data/raw/hadsst/HadISST_sst.nc"
# out: text file with metadata 
# load data data <- nc_open(path)
# get meta data only run once

get_meta <- function(data, path){
  end <- tail(strsplit(path, "/")[[1]], n = 1)
  save_to <- paste0("meta/", end, "_meta.txt")
  if (!file.exists(save_to)) {
    sink(save_to)
    print(data)
    sink()
  }
  else {
    print("metadata file already exists")
  }
}

################## For Plots ###################################################
# in: brick object
# out: random gridcell that is not on land
#      x is longitude, y is latitude

get_random_cell <- function(brick) {
  # get one layer as a raster object
  r <- raster(brick, layer = 1)
  l <- length(r)
  vals <- getValues(r)
  not_na <- which(!is.na(vals))
  rnd_cell <- sample(not_na, 1)
  lonlat <- xyFromCell(r, rnd_cell)
  return(lonlat)
}

# in: lonlat info of sea grid point
#     class is matrix,array
# out: a ggplot of point on map
plot_cell <- function(cell) {
  # Using GGPLOT, plot the Base World Map
  mp <- NULL
  mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
  mp <- ggplot() + mapWorld
  
  #vNow Layer the cities on top
  mp <- mp + geom_point(aes(x=cell[,1], y=cell[,2]),color="blue", size=3)
  mp
}

# in: dataset and respective cellpoint
# out: ggplot of time series data for cellpoint
get_ts <- function(data,cell) {
  vals <- c(extract(data,cell))
  return(vals)
}

# in: time series of data
# out: ggplot of time series
plot_ts <- function(ts) {
  df <- as.data.frame(ts)
  df$month <- 1:length(ts)
  ggplot(df, aes(x=month, y=ts)) + geom_point() +
    geom_line() + geom_smooth()
  
}

# in: vector of time series values
# out: stl plot of time series
plot_stl <- function(values) {
  time_series <- ts(values, frequency = 12)
  time_series %>% stl(t.window = 12, s.window = "periodic", robust = TRUE) %>% 
    forecast::autoplot()
}

# in: vector to plot, timelag for title
# out: ggplot of density
plot_density <- function(corr_vec, timelag) {
  df <- data.frame(corr_vec)
  plt <- ggplot(df, aes(x=corr_vec)) + geom_density() + 
    ggtitle(paste("Density Plot of Correlations with timelag",timelag))
  plt
}

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
    scale_fill_gradient2() +
    theme_bw() +
    coord_quickmap() 
  return(plt)
}


# plot the denstiy from a raster object ignoring spatial and temporal
# dependencies
# in: raster object
# out: ggplot of the values density
plot_dens_gg <- function(raster_obj) {
  df <- raster::as.data.frame(raster_obj, long = TRUE)
  if(anyNA(df)) df <- na.omit(df)
  p <- ggplot(df, aes(x=value)) +
    geom_histogram(aes(y = ..density..),
                   colour = 1, fill = "white",
                   bins=50) +
    geom_density(lwd = 1, colour = 4,
                 fill = 4, alpha = 0.25)
  return(p)
}



# plot a summary done by raster calc function, f.e plot(precip, calc(precip, mean))
# in: here, precip <- aggregate::precip() and the summary computed by calc()
# out: plot showing the summary on a map
plot_summary <- function(summary) {
  df <- base::as.data.frame(cbind(coordinates(summary), getValues(summary)))
  colnames(df) <- c("Longitude","Latitude", "val")
  var <- attr(summary,"var")
  if(var == "precip") {
    plt <- ggplot(data = df, aes(x = Longitude, y = Latitude, fill = val)) +
      geom_raster(interpolate = FALSE) +
      scale_fill_gradient2(low = "brown", high = "dark green", midpoint = mean(df$val, na.rm = TRUE))
  }
  if(var == "sst") {
    plt <- ggplot(data = df, aes(x = Longitude, y = Latitude, fill = val)) +
      geom_raster(interpolate = FALSE) +
      scale_fill_gradient2(low = "blue", high = "red", midpoint = mean(df$val, na.rm = TRUE))
  }
  return(plt)
}

# rather use the trendslope from the stl deseasonalised data
# get the trends of raster time series by computing a linear model w/ intercept
# to be applied in a apply function call, f.e. trends <- apply(m, 1, get_trend)
# in: vector/ row of a matrix that consists row is lot/lan col is month
# out: the trend over the respective time series

get_trend <- function(vec) {
  if(all(is.na(vec))) return(NA)
  b <- seq(length(vec))
  trend <- lm(vec ~ b)$coefficients[2]
  return(trend)
}

# plot the trends of the data after computing it from the data
# in: data for example: aggregate::precip() and the trends computed by get_trend
# out: plot showing the trend values on a map
plot_trends <- function(data, trends, var) {
  df <- base::as.data.frame(cbind(coordinates(data), trends))
  colnames(df) <- c("Longitude","Latitude", "val")
  if(var == "precip") {
    plt <- ggplot(data = df, aes(x = Longitude, y = Latitude, fill = val)) +
      geom_raster(interpolate = FALSE) +
      scale_fill_gradient2(low = "brown", high = "dark green", midpoint = mean(df$val, na.rm = TRUE))
  }
  if(var == "sst") {
    plt <- ggplot(data = df, aes(x = Longitude, y = Latitude, fill = val)) +
      geom_raster(interpolate = FALSE) +
      scale_fill_gradient2(low = "blue", high = "red", midpoint = mean(df$val, na.rm = TRUE))
  }
  return(plt)
}

# helper function for the mon plots
# create a indices matrix to plot all Januaries all Februaries etc together
# in: raster object
# out: indices matrix with 12 rows in the first row all Januaries,..
create_ind_mat <- function(raster_obj) {
  ind_mat <- matrix(NA, nrow = 12, ncol = nlayers(raster_obj)/12)
  for(i in 1:12) {
    ind_mat[i,] <- seq(i, nlayers(raster_obj), 12)
  }
  return(ind_mat)
}

add_var_as_attr <- function(object, variable) {
  attr(object, "var") <- variable
  return(object)
}

# creates a list which contains 3 lists with each 12 plots
# first list for loc means for each month
# second list for loc sds for each month
# third list for loc trends for each month
# Note trend list is commented out since we rather plot
# the trends from the deseasonalised data
# in: raster object
# out: list with 1. list of month mean 2. list of month sd
mon_plots <- function(raster_obj, variable) {
  ind_mat <- create_ind_mat(raster_obj)
  plot_list_m <- list()
  plot_list_sd <- list()
  plot_list_trend <- list()
  pvals <- getValues(raster_obj)
  for(i in 1:nrow(ind_mat)) {
    mon <- subset(raster_obj, ind_mat[i,])
    mon_means <- calc(mon, mean)
    mon_sd <- calc(mon, sd)
    mon_trend <- calc(mon, get_trend)
    
    mon_means <- add_var_as_attr(mon_means, variable)
    mon_sd <- add_var_as_attr(mon_sd, variable)
    mon_trend <- add_var_as_attr(mon_trend, variable)
    
    m_plot <- plot_summary(mon_means) + ggtitle(paste(month.name[i])) +
      theme(#axis.text.x=element_blank(), #remove x axis labels
        #axis.ticks.x=element_blank(), #remove x axis ticks
        #axis.text.y=element_blank(),  #remove y axis labels
        #axis.ticks.y=element_blank(), #remove y axis ticks
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=5),
      )
    m_plot$labels$fill <- "Mean"
    sd_plot <- plot_summary(mon_sd)  + ggtitle(paste(month.name[i])) +
      #ggtitle(paste("Means for month", i)) +
      theme(#axis.text.x=element_blank(), #remove x axis labels
        #axis.ticks.x=element_blank(), #remove x axis ticks
        #axis.text.y=element_blank(),  #remove y axis labels
        #axis.ticks.y=element_blank(), #remove y axis ticks
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=5),
      )
    sd_plot$labels$fill <- "SD"
    trend_plot <- plot_summary(mon_trend) + #ggtitle(paste("trend for month", i)) +
      #ggtitle(paste("Means for month", i)) +
      theme(#axis.text.x=element_blank(), #remove x axis labels
        #axis.ticks.x=element_blank(), #remove x axis ticks
        #axis.text.y=element_blank(),  #remove y axis labels
        #axis.ticks.y=element_blank(), #remove y axis ticks
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=5)
      )
    plot_list_m[[i]] <- m_plot
    plot_list_sd[[i]] <- sd_plot
    plot_list_trend[[i]] <- trend_plot
  }
  return(list(plot_list_m, plot_list_sd, plot_list_trend))
}


###############For Decomposition################################################

# create timestamps that are the first day of a month, from a raster obj
# in: raster object and a reference string
# out: timestamps as Dates
prepare_timestamps <- function(raster_obj, ref_string) {
  cnames <- as.integer(as.numeric(str_replace(names(raster_obj), "X", "")))
  ref_s <- RNetCDF::utcal.nc(ref_string, cnames, type ="s")
  tstamps_og <- as.Date(ref_s)
  # change vector so that
  # its always the first of the month
  l <- length(tstamps_og)
  start <- ceiling_date(tstamps_og[1], "month")
  end <- ceiling_date(tstamps_og[l], "month")
  tstamps <- seq(start, end, by = "month")
  return(tstamps)
}

# create a mask that can be used by the rtsa.stl function
# in: raster object
# out: raster mask with 1 for nonNA and 0 for NA
prepare_mask <- function(raster_obj) {
  mask <- raster_obj[[1]]
  names(mask) <- "mask"
  values(mask) <- 1
  mask[which(is.na(getValues(raster_obj[[1]])))] <- 0
  return(mask)
}

# in: brick object
# out: matrix with dim ncell x nlayers
brick_to_matrix_wna <- function(brick_object, nlayers) {
  # as.matrix comes here from raster, so raster::as.matrix()
  m <- as.matrix(brick_object[[1:nlayers]])
  return(m)
}

# in: matrix of ncell X nlayers
# out: list of stl decomposed time series
#      length of list equal ncell input
# frequency 12 monthly observations
# s.window, window used for smoothing
decompose_matrix_wna <- function(matrix_with_ts, ncells) {
  out <- apply(matrix_with_ts[1:ncells,], 1, function(x)
    if (any(is.na(x))) {
      NA
    } else {
      stl(ts(x, frequency = 12), s.window = "periodic")
    }
  )
  return(out)
}


# in: list of decomposed time series via stl
# out: matrix that only contains remainder and trend
get_remainder_and_trend_wna <- function(list_of_decomposed_ts) {
  l <- length(list_of_decomposed_ts)
  out <- matrix(NA, nrow = l, ncol = nrow(list_of_decomposed_ts[[1]]$time.series))
  for (i in 1:l) {
    if (is.na(list_of_decomposed_ts[[i]][1])) {
      out[i,] <- NA
    } else {
      out[i,] <- c(list_of_decomposed_ts[[i]]$time.series[,"remainder"]) +
        c(list_of_decomposed_ts[[i]]$time.series[,"trend"])
    }
  }
  return(out)
}

# in: brick object, number of layers and cells to decompose
# out: matrix with trend and remainder time series
deseasonalise_brick_wna <- function(brick, nlayers, ncells) {
  ts_matrix <- brick_to_matrix_wna(brick_object = brick, nlayers = nlayers)
  dec_list <- decompose_matrix_wna(matrix_with_ts = ts_matrix, ncells = ncells)
  r_and_t <- get_remainder_and_trend_wna(list_of_decomposed_ts = dec_list)
  return(r_and_t)
}
#######################For clustering###########################################
# Taken from https://github.com/kevinblighe/clusGapKB/blob/master/clusGapKB.R
clusGapKB <- function (
    x,
    FUNcluster,
    K.max,
    B=100,
    d.power=1,
    verbose=interactive(),
    ...)
{
  stopifnot(is.function(FUNcluster), length(dim(x))==2, K.max>=2, (n<-nrow(x))>=1, ncol(x)>=1)
  
  if (B != (B. <- as.integer(B)) || (B <- B.) <= 0) {
    stop("'B' has to be a positive integer")
  }
  
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  
  ii <- seq_len(n)
  
  # KB
  ############
  # create a function that will accept a parallel job
  if (Sys.info()['sysname'] == 'Windows') {
    funParallel <- parallel::parLapply
  } else {
    funParallel <- parallel::mclapply
  }
  ############
  
  W.k <- function(X, kk) {
    clus <- if (kk > 1) {
      FUNcluster(X, kk, ...)$cluster
    } else {
      rep.int(1L, nrow(X))
    }
    
    0.5 * sum(vapply(split(ii, clus), function(I) {
      xs <- X[I, , drop = FALSE]
      sum(dist(xs)^d.power/nrow(xs))}, 0))
  }
  
  logW <- E.logW <- SE.sim <- numeric(K.max)
  
  if (verbose) {
    cat("Clustering k = 1,2,..., K.max (= ", K.max, "): .. ", sep = "")
  }
  
  # KB
  ############
  # original code:
  #for (k in 1:K.max) logW[k] <- log(W.k(x, k))
  # modified code:
  if (Sys.info()['sysname'] == 'Windows') {
    logW <- unlist(funParallel(cl, 1:K.max, function(k) log(W.k(x, k))))
  } else {
    logW <- unlist(funParallel(1:K.max, function(k) log(W.k(x, k))))
  }
  ############
  
  if (verbose) {
    cat("done\n")
  }
  
  xs <- scale(x, center = TRUE, scale = FALSE)
  m.x <- rep(attr(xs, "scaled:center"), each = n)
  V.sx <- svd(xs, nu = 0)$v
  rng.x1 <- apply(xs %*% V.sx, 2, range)
  logWks <- matrix(0, B, K.max)
  
  if (verbose) {
    cat("Bootstrapping, b = 1,2,..., B (= ", B, ")  [one \".\" per sample]:\n", sep = "")
  }
  
  for (b in 1:B) {
    z1 <- apply(rng.x1, 2, function(M, nn) runif(nn, min=M[1], max=M[2]), nn=n)
    
    z <- tcrossprod(z1, V.sx) + m.x
    
    # KB
    ############
    # original code:
    #for (k in 1:K.max) {
    #	logWks[b, k] <- log(W.k(z, k))
    #}
    # modified code:
    if (Sys.info()['sysname'] == 'Windows') {
      tmplogWks <- unlist(funParallel(cl, 1:K.max, function(k) log(W.k(z, k))))
    } else {
      tmplogWks <- unlist(funParallel(1:K.max, function(k) log(W.k(z, k))))
    }
    logWks[b,1:K.max] <- tmplogWks
    ############
    
    if (verbose) {
      cat(".", if (b%%50 == 0) paste(b, "\n"))
    }
  }
  
  if (verbose && (B%%50 != 0)) {
    cat("", B, "\n")
  }
  
  E.logW <- colMeans(logWks)
  SE.sim <- sqrt((1 + 1/B) * apply(logWks, 2, var))
  
  structure(
    class="clusGap",
    list(
      Tab=cbind(logW, E.logW, gap=E.logW-logW, SE.sim),
      n=n,
      B=B,
      FUNcluster=FUNcluster))
}


#######################For Correlation Analysis##################################

# option front or back cut
# for precip front cut
# for sst back cut
# in: the matrix to cut, the timelag to apply, the side from which to cut
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
compute_corr <- function(dec_matrix_sst, dec_matrix_precip, timelag=0,
                         cor_method="pearson") {
  if (timelag!=0) {
    dec_matrix_sst <- cut_matrix(dec_matrix_sst, timelag, "back")
    dec_matrix_precip <- cut_matrix(dec_matrix_precip, timelag, "front")
  }
  assertthat::are_equal(ncol(dec_matrix_precip), ncol(dec_matrix_sst))
  precip_means <- apply(dec_matrix_precip, 2, mean)
  cor_vec <- apply(dec_matrix_sst, 1, function(x) cor(x, precip_means, 
                                                      method = cor_method))
  return(cor_vec)
}

#######################For Time Series CV##################################

load_data <- function(data_path, varname) {
  isrds <- stringr::str_detect(data_path, ".rds")
  isnc <- stringr::str_detect(data_path, ".nc")
  if(isrds) data_loaded <- readRDS(data_path)
  if(isnc) {
    data_loaded <- brick(data_path, varname = varname)
    data_loaded <- as.data.frame(data_loaded)
  }
  if(!(isrds | isnc)) stop("data should be either rds or nc")
  return(data_loaded)
}


add_colnames <- function(path_old_sst, sst) {
  old_sst <- brick(path_old_sst, varname = "sst")
  coord <- coordinates(old_sst)
  sst <- t(sst)
  assertthat::assert_that(dim(coord)[1] == dim(sst)[2])
  # give sst columns the coordinates as names
  cnames <- paste(coord[,1], coord[,2])
  colnames(sst) <- cnames
  return(sst)
}

# function for dropping NA in sst
prepare_sst <- function(sst) {
  #transpose sst,rows are months, cols are coord after transposing
  #sst <- t(sst)
  #sst will be transposed before
  #drop sst info that contains NA
  old_dim <- dim(sst)
  drop <- apply(sst, 2, function(x) all(is.na(x)))
  sst <- sst[,!drop]
  new_dim <- dim(sst)
  if(old_dim[2] == new_dim[2]) message("No rows were dropped")
  return(sst)
}

# function for finding out which and how many rows to drop in the creaetimseslices
# function
get_obs_to_drop <- function(nrow, nfold) {
  if (nrow %% nfold != 0) {
    # fold is train+test
    obs_in_fold <- floor(nrow/nfold)
    obs_used <- nfold*obs_in_fold
    # drop difference in rows, drop first observations
    drop_n_first_obs <- nrow-obs_used
    return(drop_n_first_obs)
  }
}

# and then actually dropping the rows
drop_obs <- function(data, obs_to_drop) {
  if(!is.null(obs_to_drop)) {
    if(length(dim(data))==2)
      data <- data[-c(1:obs_to_drop),]
    if(is.null(dim(data)))
      data <- data[-c(1:obs_to_drop)]
    return(data)
  }
  return(data)
}

# Create TimeSlices function directly taken from the caret package
# on the server it was quite difficult to install caret, so I
# take the function directly for the scripts there
createTimeSlices <- function (y, initialWindow, horizon = 1, fixedWindow = TRUE, 
                              skip = 0) 
{
  stops <- seq(initialWindow, (length(y) - horizon), by = skip + 
                 1)
  if (fixedWindow) {
    starts <- stops - initialWindow + 1
  }
  else {
    starts <- rep(1, length(stops))
  }
  train <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
  test <- mapply(seq, stops + 1, stops + horizon, SIMPLIFY = FALSE)
  nums <- gsub(" ", "0", format(stops))
  names(train) <- paste("Training", nums, sep = "")
  names(test) <- paste("Testing", nums, sep = "")
  out <- list(train = train, test = test)
  out
}


# In this function we want to get the lambda values,
# that glmnet uses
# according to 
# https://stats.stackexchange.com/questions/174897/choosing-the-range-and-grid-density-for-regularization-parameter-in-lasso
# we have a formula for lambda_max 
# and min is then chosen from that lambda_max
# lambda_max is data derived meaning from the data
# we choose the lambda that forces all coefficients
# to be zero
# TODO find out how that works
# https://datascience.stackexchange.com/questions/48885/covariance-as-inner-product
# meaning compute all inner products and choose
# lambda_max as lambda_max  = max abs val(inner product)/N
get_lambda_values <- function(sst, precip){
  #TODO compute inner product with target
  # for all variables
  # target_vec %*% feature_matrix
  # drop non-numeric variables for lambda computing
  # numerics <- sapply(sst, class) == "numeric"
  numerics <- unname(apply(sst, 2, class) == "numeric")
  sst <- as.matrix(sst[,numerics])
  inner_prods <- precip %*% scale(sst, center = TRUE, scale = TRUE)
  #TODO get the max abs value
  inner_prods <- na.omit(c(inner_prods))
  max_inner <- max(abs(inner_prods))
  #TODO comput lambdamax
  max_lambda <- max_inner/nrow(sst)
  #TODO compute lambdamin
  min_lambda <- 0.00001*max_lambda
  # TODO create vector with lambda values
  # create vector of lambdas
  # evenly spaced points on log scale between mx and mn
  lambda_vec <- rev(exp(seq(log(min_lambda),log(max_lambda), length.out = 100)))
  return(lambda_vec)
}

add_ts_vars <- function(data_matrix) {
  df <- as.data.frame(data_matrix)
  rm(data_matrix)
  df %>% pivot_longer(cols = colnames(df),
                      names_to = "location",
                      values_to = "temperature") %>%
    group_by(location) %>% 
    # mutate(
    #   rollm_2 = zoo::rollmean(x = temperature, k=2,
    #                           fill = NA, align = "right"),
    #   rollm_3 = zoo::rollmean(x = temperature, k=3,
    #                           fill = NA, align = "right"),
    #   rollm_4 = zoo::rollmean(x = temperature, k=4,
    #                           fill = NA, align = "right")) %>%
    mutate(
      lag1 = dplyr::lag(x=temperature, n=1),
      lag2 = dplyr::lag(x=temperature, n=2),
      lag3 = dplyr::lag(x=temperature, n=3)#,
      #rollm_4 = zoo::rollmean(x = temperature, k=4,
      #                        fill = NA, align = "right")
    ) %>%
    mutate(rn = row_number()) %>%
    # pivot_wider(names_from = location, 
    #             values_from = c(temperature, rollm_2,
    #                             rollm_3, rollm_4)) %>%
    pivot_wider(names_from = location, values_from = c(temperature, lag1,
                                                       lag2, lag3)) %>% #, rollm_4)) %>%
    select(-rn) -> df
  cnames <- colnames(df)
  df %>% mutate(run_month = row_number(), .before=cnames[1]) -> df
  cyc_month_vec <- factor(as.character(month(ymd(010101) + 
                                               months(df$run_month-1),
                                             label=TRUE,abbr=TRUE)))
  df <- df %>% mutate(cyc_month = cyc_month_vec) %>%
    relocate(cyc_month, .after = run_month)
  return(df)
} 

# helper function for diff data
diff_data <- function(df, ndiff) {
  df <- apply(df, 2, function(x) diff(x, lag = 1, ndiff))
  return(df)
}

cut_data <- function(df, ndiff) {
  df <- df[-c(seq(ndiff))]
}


# For testing, smaller set
# sst <- sst[1:60,]
# precip <- precip[1:60]
# maybe exclude skip and make sure that there is no
# overlap
# Or if overlap = FALSE, then skip is function
# maybe later add functionality for overlap TRUE/FALSE
# for now we always say no overlap
# initialwindow+horizon-1
# if overlap = FALSE, skip is ignpred
# type could be lasso, fusedlasso etc
# but for these we can write own helper functions
# like cv_for_ts_lasso etc
# fixedWindow is always TRUE here
# so possible add-on, decide if overlap yes/no
# and which type of regression mode should we use
# like lasso or fused lasso etc.
# TODO when testing this also remember to leave some
# observations for validation set.
# cv_for_ts <- function(sst, precip, nfold, size_train, size_test, save_folder) {
#   set.seed(1234)
#   # TODO answer question:compute precip mean here or before?
#   sst <- prepare_sst(sst)
#   assertthat::are_equal(nrow(sst), length(precip))
#   n_row <- nrow(sst)
#   obs_to_drop <- get_obs_to_drop(n_row, nfold)
#   sst <- drop_obs(sst, obs_to_drop)
#   precip <- drop_obs(precip, obs_to_drop)
#   # now we made sure that nrow(data) %% nfold == 0
#   assertthat::are_equal(size_train+size_test, nrow(sst)/nfold)
#   index_list <- createTimeSlices(1:nrow(sst), initialWindow=size_train, horizon=size_test,
#                                         skip=size_train+size_test-1)
#   #TODO create list with glmnet objects so we can plot
#   #their paths and coefficients on map
#   #TODO safe index_list as well
#   #TODO for each lambda
#   #         for each fold
#   #             fit model and predict on test, save err
#   # now we have for each lambda 5 MSE
#   # find best lambda
#   # refit on all train+test data with lambda
#   # predict on validation set and report error final
#   # error final can then be compared among diff models
#   lambda_vec <- get_lambda_values(sst, precip)
#   err_mat <- matrix(NA, ncol = nfold, nrow = length(lambda_vec))
#   #for(i in 1:length(lambda_vec)) {
#   for(j in 1:length(index_list$train)) {
#     id_train <- unlist(index_list$train[j], use.names = FALSE)
#     id_test <- unlist(index_list$test[j], use.names = FALSE)
#     x_train <- sst[id_train,]
#     y_train <- precip[id_train]
#     x_test <- sst[id_test,]
#     y_test <- precip[id_test]
#     # die cross validaion is done here classically
#     # does not work!!!
#     # trained_model <- glmnet(x_train, y_train)
#     trained_model <- glmnet(x_train, y_train, lambda=lambda_vec, standardize=FALSE)
#     #TODO change value her for s
#     predicted <- predict(trained_model, newx = data.matrix(x_test), s = lambda_vec)
#     err_col <- apply(predicted, 2, function(x) mean((x-y_test)^2))
#     err_mat[,j] <- err_col
#     #save(trained_model, file=paste0("results/CV-lasso/model-","lambda-",i,"fold-",j,".RData"))
#     #print(paste("lambda", i, "fold", j))
#     print(paste("finished fold", j))
#   }
#   #print(paste("finished lambda", i))
#   #}
#   print("finished fitting")
#   dir.create(paste0("results/CV-lasso/",save_folder))
#   index_list_path <- paste0("results/CV-lasso/", save_folder, "/index-list.rds")
#   lambda_vec_path <- paste0("results/CV-lasso/", save_folder, "/lambda-vec.rds")
#   err_mat_path <- paste0("results/CV-lasso/", save_folder, "/err-mat.rds")
#   saveRDS(index_list, file = index_list_path)
#   saveRDS(lambda_vec, file = lambda_vec_path) 
#   saveRDS(err_mat, file = err_mat_path)
#   return(err_mat)
#   # until here we keep the error for each fold
#   # but with fixed regularisation
#   #TODO add loop for different regularisation values
#   #TODO add parameters for glmnet
#   #TODO create function that computes number of observations
#   #for each fold so that all windows have same number
#   #of observations
#   #make sure that nrow(data) %% nfold == 0
#   #AND initialwindow+horizon == nrow(data)/nfold
#   #TODO read about standardisation ?glmnet()
#   
# }

cv_lasso <- function(sst, precip, index_list, save_folder, include_ts_vars, stand,
                     diff_features, des_features, standardize_features,
                     standardize_response, diff_n, stand_then_diff, center_response=FALSE,
                     boxcox_response = FALSE,
                     log_response = FALSE) {
  # dir.create(paste0("results/CV-lasso/", save_folder))
  # dir.create(paste0("results/CV-lasso/", save_folder, "/fold-models"))
  lambda_vec <- get_lambda_values(sst, precip)
  nfold <- length(index_list$train)
  err_mat <- matrix(NA, ncol = nfold, nrow = length(lambda_vec),
                    dimnames = list(lambda_vec))
  for(j in 1:length(index_list$train)) {
    id_train <- unlist(index_list$train[j], use.names = FALSE)
    id_test <- unlist(index_list$test[j], use.names = FALSE)
    x_train <- sst[id_train,]
    y_train <- precip[id_train]
    x_test <- sst[id_test,]
    y_test <- precip[id_test]
    save_path <- paste0 ("results/CV-lasso/", save_folder, "/")
    
    if(log_response == TRUE) {
      y_train <- log(y_train)
    }
    
    if(boxcox_response == TRUE) {
      lb <- BoxCox.lambda(y_train)
      y_train <- BoxCox(y_train, lb)
    }
    # if(stand_then_diff == TRUE) {
    #   x_train <- standardize_train(x_train)
    #   x_test <- standardize_test(x_train, x_test)
    #   
    #   max_ndiffs <- diff_n[2]
    #   x_train <- apply(x_train, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
    #   y_train <- y_train[-c(seq(max_ndiffs))]
    #   
    #   x_test <- apply(x_test, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
    #   y_test <- y_test[-c(seq(max_ndiffs))]
    #   print("differentiated features")
    # }
    if(include_ts_vars == TRUE) {
      x_train <- add_ts_vars(x_train)
      keep_vec <- complete.cases(x_train)
      x_train <- x_train[keep_vec,]
      y_train <- y_train[keep_vec]
      
      x_test <- add_ts_vars(x_test)
      keep_vec <- complete.cases(x_test)
      x_test <- x_test[keep_vec,]
      y_test <- y_train[keep_vec]
      
      x_train <- data.matrix(x_train)
      # x_train <- scale(x_train)
      # x_t1 <- x_test[,-2]
      # x_t2 <- scale(x_t1)
      # x_test <- cbind(x_test[,2], x_t2)
      # 
      # lambda_vec <- get_lambda_values(x_train, y_train)
      # lambda_fold_path <- paste0("results/CV-lasso/", save_folder, "/fold-models/",
      #                            "lambda-vec-fold", j, ".rds")
      # saveRDS(lambda_vec, file=lambda_fold_path)
    }
    # if(diff_features == TRUE) {
    #   # lasso og diff does not have fac_month and time_ind code
    #   # between ## are new
    #   # ndiffs <- apply(x_train, 2, unitroot_ndiffs)
    #   # max_ndiffs <- max(ndiffs)
    #   max_ndiffs <- 1
    #   ##
    #   nr <- nrow(x_train)
    #   time_ind <- seq(nr)
    #   fac_month <- time_ind %% 12
    #   fac_month[fac_month == 0] <- 12
    #   fac_month <- as.factor(fac_month)
    #   ##
    #   x_train <- apply(x_train, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
    #   
    #   ##
    #   time_ind <- time_ind[-c(seq(max_ndiffs))]
    #   fac_month <- fac_month[-c(seq(max_ndiffs))]
    #   x_train <- cbind(x_train, time_ind, fac_month)
    #   x_train <- data.matrix(x_train)
    #   ##
    #   # y_train <- y_train[-c(seq(max_ndiffs))]
    #   y_train <- diff(y_train, max_ndiffs)
    #   
    #   ##
    #   time_ind2 <- seq(nr+1,nr+nrow(x_test))
    #   fac_month2 <- time_ind2 %% 12
    #   fac_month2[fac_month2 == 0] <- 12
    #   fac_month2 <- as.factor(fac_month2)
    #   ##
    #   
    #   x_test <- apply(x_test, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
    #   ##
    #   time_ind2 <- time_ind2[-c(seq(max_ndiffs))]
    #   fac_month2 <- fac_month2[-c(seq(max_ndiffs))]
    #   x_test <- cbind(x_test, time_ind2, fac_month2)
    #   ##
    #   
    #   # y_test <- y_test[-c(seq(max_ndiffs))]
    #   y_test <- diff(y_test, max_ndiffs)
    #   
    #   # add_time <- function(df) {
    #   #   rn <- rownames(df)
    #   #   s <- strsplit(rn, "X", "")
    #   #   time_vec <- sapply(s, FUN = function(x) as.integer(x[[2]]))
    #   #   df <- cbind(time_vec, df)
    #   #   return(df)
    #   # }
    #   # 
    #   # add_month_fac <- function(df) {
    #   #   t_vec <- df[, "time_vec"]
    #   #   fac_month <- t_vec %% 12
    #   #   fac_month[fac_month == 0] <- 12
    #   #   fac_month <- as.factor(fac_month)
    #   #   df <- cbind(fac_month, df)
    #   #   return(df)
    #   # }
    #   # 
    # }
    if(diff_n[1] == TRUE) {
      # ndiffs <- apply(features_i, 2, unitroot_ndiffs)
      # max_ndiffs <- max(ndiffs)
      max_ndiffs <- diff_n[2]
      x_train <- apply(x_train, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
      y_train <- y_train[-c(seq(max_ndiffs))]
      id_train <- id_train[-c(seq(max_ndiffs))]
      
      x_test <- apply(x_test, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
      y_test <- y_test[-c(seq(max_ndiffs))]
      id_test <-  id_test[-c(seq(max_ndiffs))]
      print("differentiated features")
    }
    if(des_features == TRUE) {
      #stop("under construction deseasonalised")
      # x_train <- apply(x_train, 2, function(x) c(stl(ts(x, frequency=12), s.window = "periodic",
      #                                                  robust = TRUE)$time.series[,"remainder"]))
      # x_together <- rbind(x_train,x_test)
      # x_together <- apply(x_together, 2, function(x) c(stl(ts(x, frequency=12), s.window = "periodic",
      #                                               robust = TRUE)$time.series[,"remainder"]))
      
      
      # x_train <- apply(x_train, 2, function(x) c(stl(ts(x, frequency=12), s.window = "periodic",
      #                                                robust = TRUE)$time.series[,"remainder"]))
      # x_together <- rbind(x_train, x_test)
      # x_together <- apply(x_together, 2, function(x) c(stl(ts(x, frequency=12), s.window = "periodic",
      #                                                      robust = TRUE)$time.series[,"remainder"]))
      # x_test <- x_together[-c(seq(nrow(x_train))),]
      
      x_train <- deseas_train(x_train)
      x_test <- deseas_test(x_train, x_test)
      
    }
    if(standardize_features == TRUE) {
      # mean_x_train <- apply(x_train,2, mean)
      # sdn_x_train <- apply(x_train,2,sdN)
      # x_train <- scale(x_train, center=mean_x_train,
      #                  scale=sdn_x_train)
      # x_test <- scale(x_test, center=mean_x_train,
      #                 scale=sdn_x_train)
      # nonzero_sd_cols <- complete.cases(t(x_train))
      # x_train <- x_train[,nonzero_sd_cols]
      # x_test <- x_test[,nonzero_sd_cols]
      
      
      x_train <- standardize_train(x_train)
      x_test <- standardize_test(x_train, x_test)
      # drop variables with 0 variance
      
      print("standardized features")
    }
    if(standardize_response == TRUE) {
      mean_y_train <- mean(y_train)
      sdn_y_train <- sdN(y_train)
      y_train <- scale(y_train, center=mean_y_train, 
                       scale=sdn_y_train)
      # y_test <- scale(y_test, center=mean_y_train, 
      #                 scale=sdn_y_train)
    }
    if(center_response == TRUE) {
      mean_y_train <- mean(y_train)
      y_train <- scale(y_train, center = mean_y_train,
                       scale = FALSE)
    }

    
    trained_model <- glmnet(data.matrix(x_train), y_train, lambda = lambda_vec,
                            standardize=FALSE, standardize.response = FALSE)
    #TODO change value her for s
    predicted <- predict(trained_model, newx = data.matrix(x_test), s = lambda_vec)
    if(standardize_response == TRUE) {
      predicted <- apply(predicted, 2, function(x)  x*sdn_y_train + mean_y_train)
    }
    if(center_response == TRUE) {
      predicted <- apply(predicted, 2, function(x)  x + mean_y_train)
    }
    if(boxcox_response == TRUE) {
      predicted <- apply(predicted, 2, function(x) InvBoxCox(x, lambda = lb))
    }
    if(log_response == TRUE) {
      predicted <- apply(predicted, 2, exp)

    }
    err_col <- apply(predicted, 2, function(x) mean((x-y_test)^2))
    err_mat[,j] <- err_col
    id_min_l <- which.min(err_col)
    attr(err_col, "id_min_l") <- id_min_l
    attr(err_col, "min_l") <- lambda_vec[id_min_l]
    
    fold_model_path <- paste0(save_path, "fold-models/",
                              "model-fold-",j,".rds")
    saveRDS(trained_model, file = fold_model_path)
    
    predictions_path <- paste0(save_path, "/fold-predictions/",
                               "predictions-fold", j, ".rds")
    saveRDS(predicted, file = predictions_path)
    
    fe_plot <- plot_fold_errors_lasso(err_col, lambda_vec, j)
    saveRDS(fe_plot, paste0(save_path,"/err-mat-plots/","err-plot-fold-",j,".rds"))
    rm(fe_plot)
    
    fp_plot <- plot_fold_preds_lasso(err_col, trained_model, x_train, y_train, id_train)
    saveRDS(fp_plot, paste0(save_path, "pred-plots/fit-plot-fold-", j, ".rds"))
    rm(fp_plot)
    
    tp_plot <- plot_test_preds_lasso(err_col, predicted, y_test, id_test)
    saveRDS(tp_plot, paste0(save_path, "pred-plots/pred-plot-fold-", j, ".rds"))
    rm(tp_plot)
    
    co_plot <- plot_nonzero_coef_lasso(trained_model, err_col)
    saveRDS(co_plot, paste0(save_path, "coef-plots/coef-plot-fold-", j, ".rds"))
    
    print(paste("finished fold", j))
    
  }
  
  lambda_vec_path <- paste0("results/CV-lasso/", save_folder, "/lambda-vec.rds")
  saveRDS(lambda_vec, file = lambda_vec_path) 
  return(err_mat)
}

sdN=function(x){
  sigma=sqrt( (1/length(x)) * sum((x-mean(x))^2))
  return(sigma)
}

custom_stand <- function(x) {
  stand_x <- (x-mean(x))/sdN(x)
  return(stand_x)
}


cv_fused_lasso <- function(sst, precip, index_list, save_folder, graph,
                           maxsteps, stand, standardize_features, standardize_response,
                           gamma, parallelize, center_response) {
  # dir.create(paste0("results/CV-lasso/", save_folder))
  # dir.create(paste0("results/CV-lasso/", save_folder, "/fold-models"))
  # maxsteps <- maxsteps
  nfold <- length(index_list$train)
  err_mat <- matrix(NA, ncol = nfold, nrow = maxsteps)
  if(parallelize == FALSE) {
    source("code/R/helper-functions-parallel-cv.R")
    for(j in 1:length(index_list$train)) {
      err_mat[,j] <- cv_run_fused(j, err_mat, nfold, sst, precip, index_list, save_folder, graph,
                                  maxsteps, stand, standardize_features, standardize_response,
                                  gamma, center_response)
    }
    return(err_mat)
  }
  if(parallelize == TRUE) {
    num_cores <- parallel::detectCores()
    #num_cores <- 5
    doParallel::registerDoParallel(num_cores)
    print(paste("detected and registered", num_cores, "cores"))
    library(foreach)
    err_mat <- foreach(j = 1:length(index_list$train),
                       .packages="genlasso", .combine = "cbind") %dopar% {
                         source("code/R/helper-functions-parallel-cv.R")
                         print(paste("start job", j))
                         cv_run_fused(j = j, err_mat, nfold, sst, precip, index_list, save_folder, graph,
                                      maxsteps, stand, standardize_features, standardize_response,
                                      gamma,center_response)
                       }
    doParallel::stopImplicitCluster()
    return(err_mat)
  }
}

cv_for_ts <- function(sst, precip, nfold = 5, size_train = 60, size_test = 14, save_folder,
                      model = "lasso", graph = NULL, maxsteps=100, include_ts_vars=FALSE,
                      stand=FALSE, diff_features=FALSE, des_features=FALSE,
                      standardize_features=FALSE, standardize_response=FALSE,
                      gamma=0, parallelize = FALSE, diff_n=FALSE,
                      stand_then_diff = FALSE, center_response = FALSE,
                      boxcox_response = FALSE,
                      log_response = FALSE) {
  a <- Sys.time()
  if(model == "fused" & is.null(graph)){
    stop("for the fused LASSO a graph object is needed")
  }
  if(model == "lasso" & !is.null(graph)) {
    warning("graph object will be ignored for the lasso")
  }
  set.seed(1234)
  # TODO answer question:compute precip mean here or before?
  # sst <- prepare_sst(sst)
  assertthat::are_equal(nrow(sst), length(precip))
  n_row <- nrow(sst)
  obs_to_drop <- get_obs_to_drop(n_row, nfold)
  sst <- drop_obs(sst, obs_to_drop)
  precip <- drop_obs(precip, obs_to_drop)
  # now we made sure that nrow(data) %% nfold == 0
  assertthat::are_equal(size_train+size_test, nrow(sst)/nfold)
  index_list <- createTimeSlices(1:nrow(sst), initialWindow=size_train, horizon=size_test,
                                 skip=size_train+size_test-1)
  #TODO create list with glmnet objects so we can plot
  #their paths and coefficients on map
  #TODO safe index_list as well
  #TODO for each lambda
  #         for each fold
  #             fit model and predict on test, save err
  # now we have for each lambda 5 MSE
  # find best lambda
  # refit on all train+test data with lambda
  # predict on validation set and report error final
  # error final can then be compared among diff models
  #for(i in 1:length(lambda_vec)) {
  # dir.create(paste0("results/CV-lasso/", save_folder))
  # dir.create(paste0("results/CV-lasso/", save_folder, "/fold-models"))
  if(model == "lasso") {
    dir.create(paste0("results/CV-lasso/", save_folder))
    dir.create(paste0("results/CV-lasso/", save_folder, "/fold-models"))
    dir.create(paste0("results/CV-lasso/", save_folder, "/pred-plots"))
    dir.create(paste0("results/CV-lasso/", save_folder, "/fold-predictions"))
    dir.create(paste0("results/CV-lasso/", save_folder, "/err-mat-plots"))
    dir.create(paste0("results/CV-lasso/", save_folder, "/coef-plots"))
    
    err_mat <- cv_lasso(sst, precip, index_list, save_folder, include_ts_vars, stand,
                        diff_features, des_features,
                        standardize_features = standardize_features,
                        standardize_response = standardize_response, diff_n = diff_n,
                        stand_then_diff = stand_then_diff, center_response = center_response,
                        boxcox_response = boxcox_response,
                        log_response = log_response)
    print("finished fitting")
    index_list_path <- paste0("results/CV-lasso/", save_folder, "/index-list.rds")
    # lambda_vec_path <- paste0("results/CV-lasso/", save_folder, "/lambda-vec.rds")
    err_mat_path <- paste0("results/CV-lasso/", save_folder, "/err-mat.rds")
    saveRDS(index_list, file = index_list_path)
    # saveRDS(lambda_vec, file = lambda_vec_path) 
    saveRDS(err_mat, file = err_mat_path)
    lambda_vec <- readRDS(paste0("results/CV-lasso/", save_folder, "/lambda-vec.rds"))
    loglambdas <- log(lambda_vec)
    ss <-  paste0("results/CV-lasso/", save_folder)
    plot_errbar_gg(err_mat, loglambdas, save_to = ss)
    
    b <- Sys.time()
    print(a-b)
    return(err_mat)
  }
  if(model == "fused") {
    dir.create(paste0("results/CV-fused/", save_folder))
    dir.create(paste0("results/CV-fused/", save_folder, "/fold-models"))
    err_mat <- cv_fused_lasso(sst = sst, precip = precip, index_list = index_list, 
                              save_folder = save_folder,  graph = graph, 
                              maxsteps = maxsteps, stand=stand,
                              standardize_features = standardize_features,
                              standardize_response = standardize_response,
                              gamma=gamma,
                              parallelize = parallelize,
                              center_response = center_response)
    index_list_path <- paste0("results/CV-fused/", save_folder, "/index-list.rds")
    #lambda_vec_path <- paste0("results/CV-lasso/", save_folder, "/lambda-vec.rds")
    err_mat_path <- paste0("results/CV-fused/", save_folder, "/err-mat.rds")
    saveRDS(index_list, file = index_list_path)
    #saveRDS(lambda_vec, file = lambda_vec_path) 
    saveRDS(err_mat, file = err_mat_path)
    b <- Sys.time()
    print(a-b)
    return(err_mat)
  }
  #save(trained_model, file=paste0("results/CV-lasso/model-","lambda-",i,"fold-",j,".RData"))
  #print(paste("lambda", i, "fold", j))
  #print(paste("finished lambda", i))
  #}
  
  # until here we keep the error for each fold
  # but with fixed regularisation
  #TODO add loop for different regularisation values
  #TODO add parameters for glmnet
  #TODO create function that computes number of observations
  #for each fold so that all windows have same number
  #of observations
  #make sure that nrow(data) %% nfold == 0
  #AND initialwindow+horizon == nrow(data)/nfold
  #TODO read about standardisation ?glmnet()
  
}
################# get best lambda fused ########################################
get_pr <- function(x,y, pred_seq) {
  if(any(x=="Inf")) {
    idx <- which(x=="Inf")
    x <- x[-idx]
    y <- y[-idx]
  }
  ll <- loess(y~x)
  pr <- predict(ll, newdata = pred_seq, span = 0.05)
  pr
  return(pr)
}

get_best_lambda_fused <- function(conf) {
  err_line_plot <- readRDS(paste0("results/CV-fused/", conf$save_folder,"/err-mat-plots/err-line-plot.rds"))
  c <- ggplot_build(err_line_plot)
  rm(err_line_plot)
  dat <- c$data
  df <- dat[[1]]
  df %>% group_by(group) %>% summarise(xmin = min(x), xmax = max(x)) -> dx 
  common_range <- c(max(dx$xmin), min(dx$xmax))
  pred_seq <- seq(common_range[1], common_range[2], length.out = nrow(df)/length(unique(df$group)))
  newdf <- df %>% group_by(group) %>% mutate(k = get_pr(x,y, pred_seq=pred_seq), lambdas = pred_seq)
  v_df <- newdf  %>% group_by(lambdas) %>% mutate(mm = mean(k))
  log_lambda_min <- v_df$lambdas[which.min(v_df$mm)]
  plt <- ggplot() +
    geom_line(data = newdf, aes(x, y, color = factor(group))) +
    geom_line(data = v_df, aes(lambdas, mm)) +
    ylab("MSE") + 
    xlab(expression("log" ~ lambda))
  res_list <- list(lambda_min = exp(log_lambda_min), err_plot = plt)
  saveRDS(res_list, paste0("results/CV-fused/", conf$save_folder, "/best-lambda-res.rds"))
  return(res_list)
}



############ Fit/ Plot full models #############################################
# LASSO 
fit_eval_full_lasso <- function(save_to, sst_cv_path, precip_cv_path,
                                sst_eval_path, precip_eval_path,
                                standardize_response, center_response,
                                standardize_features, diff_n,
                                des_features=FALSE,
                                boxcox_response = FALSE,
                                log_response = FALSE) {
  # path to and save_to
  full_save_to <- paste0("results/CV-lasso/",save_to,"/")
  
  # load err mat, lambdas and get lambda min
  err_mat <- readRDS(paste0(full_save_to,"err-mat.rds"))
  lambdas <- readRDS(paste0(full_save_to,"lambda-vec.rds"))
  ids <- readRDS(paste0(full_save_to,"index-list.rds"))
  
  l_min_id <- which.min(apply(err_mat, 1, mean))
  l_min <- lambdas[l_min_id]
  #l_min <- exp(-1.75)
  
  # load data for fitting and evaluating full model
  x_train <- readRDS(sst_cv_path)
  y_train <- readRDS(precip_cv_path)
  
  x_test <- readRDS(sst_eval_path)
  y_test <- readRDS(precip_eval_path)
  
  # TODO diff features
  # TODO maybe timelags
  # deseas 
  if(log_response == TRUE) {
    y_train <- log(y_train)
  }
  
  if(boxcox_response == TRUE) {
    lb <- BoxCox.lambda(y_train)
    y_train <- BoxCox(y_train, lb)
  }
  
  if(des_features == TRUE) {
    #stop("under construction deseasonalised")
    # x_train <- apply(x_train, 2, function(x) c(stl(ts(x, frequency=12), s.window = "periodic",
    #                                                  robust = TRUE)$time.series[,"remainder"]))
    # x_together <- rbind(x_train,x_test)
    # x_together <- apply(x_together, 2, function(x) c(stl(ts(x, frequency=12), s.window = "periodic",
    #                                               robust = TRUE)$time.series[,"remainder"]))
    
    
    # x_train <- apply(x_train, 2, function(x) c(stl(ts(x, frequency=12), s.window = "periodic",
    #                                                robust = TRUE)$time.series[,"remainder"]))
    # x_together <- rbind(x_train, x_test)
    # x_together <- apply(x_together, 2, function(x) c(stl(ts(x, frequency=12), s.window = "periodic",
    #                                                      robust = TRUE)$time.series[,"remainder"]))
    # x_test <- x_together[-c(seq(nrow(x_train))),]
    
    x_train <- deseas_train(x_train)
    x_test <- deseas_test(x_train, x_test)
    
  }
  
  if(diff_n[1] == TRUE) {
    # ndiffs <- apply(features_i, 2, unitroot_ndiffs)
    # max_ndiffs <- max(ndiffs)
    max_ndiffs <- diff_n[2]
    x_train <- apply(x_train, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
    y_train <- y_train[-c(seq(max_ndiffs))]
    id_train <- id_train[-c(seq(max_ndiffs))]
    
    x_test <- apply(x_test, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
    y_test <- y_test[-c(seq(max_ndiffs))]
    id_test <-  id_test[-c(seq(max_ndiffs))]
    print("differentiated features")
  }
  
  if(standardize_features == TRUE) {
    x_train <- standardize_train(x_train)
    x_test <- standardize_test(x_train, x_test)
  }
  
  if(standardize_response == TRUE) {
    mean_y_train <- mean(y_train)
    sdn_y_train <- sdN(y_train)
    y_train <- scale(y_train, center=mean_y_train, scale = sdn_y_train)
  }
  
  if(center_response == TRUE) {
    mean_y_train <- mean(y_train)
    y_train <- scale(y_train, center = mean_y_train,
                     scale = FALSE)
  }
  
  # fit full model
  full_model <- glmnet(x_train, y_train, lambda=lambdas,
                       standardize=FALSE, standardize.response = FALSE)
  
  
  # save full model
  saveRDS(full_model, paste0(full_save_to, "full-model.rds"))
  
  # predict on evaluation data
  preds <- predict(full_model, newx = x_test)
  if(standardize_response == TRUE) {
    preds <- apply(preds, 2, function(x) x*sdn_y_train + mean_y_train)
  }
  if(center_response == TRUE) {
    preds <- apply(preds, 2, function(x) x + mean_y_train)
  }
  if(boxcox_response == TRUE) {
    preds <- apply(preds, 2, function(x) InvBoxCox(x, lambda = lb))
  }
  if(log_response == TRUE) {
    preds <- apply(preds, 2, exp)
  }
  ms_start <- nrow(x_train)+1
  ms_end <- nrow(x_train)+nrow(x_test)
  df <- data.frame(predictions = preds[,l_min_id], targets = y_test, ids=c(ms_start:ms_end))
  plt <- plot_predictions(df)
  mse_l_min <- comp_mse(preds[,l_min_id], y_test)
  attr(plt, "mse_full_model") <- mse_l_min
  attr(plt, "l_min") <- l_min
  saveRDS(plt, paste0(full_save_to,"pred-plots/pred-plot-full.rds"))
  
  
  all_errors <- apply(preds, 2, function(x) comp_mse(x, y_test))
  wm <- which.min(all_errors)
  l_opt <- (lambdas[wm])
  df2 <- data.frame(predictions = preds[,wm], targets = y_test, ids=c(ms_start:ms_end))
  plt2 <- plot_predictions(df2)
  attr(plt, "l_opt") <- l_opt
  saveRDS(plt2, paste0(full_save_to, "pred-plots/best-pred-plot-full.rds"))
  
  df3 <- data.frame(loglambdas = log(lambdas), errs = all_errors)
  plt3 <- ggplot(df3, aes(x = loglambdas, y=errs)) + 
    geom_vline(xintercept = log(l_min), linetype = "dashed", colour = "red") + geom_point() +
    geom_vline(xintercept = log(l_opt), linetype = "dashed", colour = "blue")          
  saveRDS(plt3, paste0(full_save_to, "full-model-all-errors-plot.rds"))
  
  pp <- predict(full_model, newx = x_train)
  dd <- data.frame(predictions = pp[,l_min_id], targets = y_train, ids = 1:nrow(x_train))
  plt4 <- plot_predictions(dd)
  saveRDS(plt4, paste0(full_save_to, "pred-plots/fitted-preds-plot-full.rds"))
  
  sum_list <- list(mse_l_min = mse_l_min, l_min = l_min)
  saveRDS(sum_list, paste0(full_save_to, "mse-list.rds"))
  # mse_l_min <- comp_mse(preds[,l_min_id], y_test)
  # summ_full_model <- list(mse_l_min = mse_l_min, l_min = l_min)
  
  lil_helper <- "helper"
  attr(lil_helper, "id_min_l") <- l_min_id
  coef_plot <- plot_nonzero_coef_lasso(full_model, lil_helper)
  saveRDS(coef_plot, paste0(full_save_to, "coef-plots/coef-plot-full.rds"))
  
}


# FUSED 
fit_full_fused <- function(conf) {
  # model_path <- paste0("results/CV-fused/", model_name, "/")
  # path_config <- paste0("code/R/fused-lasso/", model_name, "/config-", model_name, ".yml")
  # conf <- config::get(file = path_config)
  model_path <-  paste0("results/CV-fused/", conf$save_folder, "/")
  
  sst_cv_path <- conf$features_cv_path
  precip_cv_path <- conf$target_cv_path
  
  center_response <- conf$center_response
  standardize_response <- conf$standardize_response
  standardize_features <- conf$standardize_features
  
  sst_cv <- readRDS(sst_cv_path)
  precip_cv <- readRDS(precip_cv_path)
  
  if(standardize_response == TRUE) {
    precip_cv <- custom_stand(precip_cv)
  }
  if(conf$small) {
    g <- readRDS("data/processed/small_graph_sst.rds")
  } 
  if(!conf$small) {
    g <- readRDS("data/processed/graph_sst.rds")
  }
  if(conf$noclust) {
    g <- readRDS("data/processed/noclust_graph_sst.rds")
  }
  
  if(standardize_features == TRUE) {
    sst_cv <- standardize_train(sst_cv)
    zerostd <- which(attr(sst_cv, "scale") == 0)
    g <- delete_vertices(g, zerostd)
    print(vcount(g) == ncol(sst_cv))
    print("standardized features, cutted graph nodes with zero variance")
  }
  if(center_response == TRUE) {
    mean_precip_cv <- mean(precip_cv)
    precip_cv <- scale(precip_cv, center = mean_precip_cv,
                     scale = FALSE)
  }
  full_mod <- fusedlasso(y = precip_cv, X = sst_cv, graph = g,
                         verbose = TRUE, maxsteps = conf$maxsteps,
                         gamma = conf$gamma)
  
  dir.create(model_path)
  saveRDS(full_mod, paste0(model_path, "full-model.rds"))
  print("saved model")
  
  return(full_mod)
  
}

check_and_iterate_fused <- function(conf){
  # model_path <- paste0("results/CV-fused/", model_name, "/")
  # path_config <- paste0("code/R/fused-lasso/", model_name, "/config-", model_name, ".yml")
  # conf <- config::get(file = path_config)
  model_path <- paste0("results/CV-fused/", conf$save_folder, "/")
  
  full_model <- readRDS(paste0(model_path, "full-model.rds"))
  best_l <- readRDS(paste0(model_path, "best-lambda-res.rds"))
  best_l <- best_l$lambda_min
  if(length(best_l) == 0) {
    s <- "testing, best_l is zero"
    return(s)
  }
  
  if(best_l < min(full_model$lambda) & !(best_l > max(full_model$lambda))) {
    print(paste("best lambda from cv is", best_l, "lowest lambda from full model is",
                min(full_model$lambda)))
    print(paste("iterate full model", conf$maxsteps, " more"))
    full_model <- iterate(full_model, moresteps = conf$maxsteps,
                          minlam = best_l, verbose = TRUE)
    attr(full_model, "iterated") <- TRUE
    saveRDS(full_model, paste0(model_path, "full-model.rds"))
  }
  else {
    print("no iteration needed")
  }
}

### misc rndm / pipeline##################################################################

# standardizing
standardize_train <- function(df_train) {
  mean_vec <- apply(df_train, 2, mean)
  sdn_vec <- apply(df_train, 2, sdN)
  df_train <- scale(df_train, center = mean_vec,
                    scale = sdn_vec)
  nonzero_sd_cols <- complete.cases(t(df_train))
  df_train <- df_train[, nonzero_sd_cols]
  attr(df_train, "center") <- mean_vec
  attr(df_train, "scale") <- sdn_vec
  attr(df_train, "nonzero_sd_cols") <- nonzero_sd_cols
  return(df_train)
}

standardize_test <- function(df_train_stand, df_test) {
  mean_vec <- attr(df_train_stand, "center")
  sdn_vec <- attr(df_train_stand, "scale")
  df_test <- scale(df_test, center = mean_vec,
                   scale = sdn_vec)
  nonzero_sd_cols <- attr(df_train_stand, "nonzero_sd_cols")
  df_test <- df_test[, nonzero_sd_cols]
  return(df_test)
}

# deseasonalising
get_seas <- function(x) {
  stl_res <- stl(ts(x, frequency=12), s.window = 49,#37
                 robust = TRUE) #immer robust true, besser
  # stl_res <- stl(ts(x, frequency=12), s.window = "periodic",
  #                robust = TRUE)
  # what about log transforming and s.window="periodic"?
  stl_seas <- stl_res$time.series[,"seasonal"]
  return(stl_seas)
}


deseas_train <- function(x_train) {
  seas_train <- apply(x_train, 2, get_seas)
  x_train <- x_train - seas_train
  attr(x_train,"seas_train") <- seas_train
  return(x_train)
}

deseas_test <- function(deseas_x_train, x_test) {
  seas_train <- attr(deseas_x_train, "seas_train")
  n_needed <- nrow(x_test)
  start_id <- nrow(seas_train)-11
  end_id <- nrow(seas_train)
  last_val_mat <- seas_train[start_id:end_id,]
  rep_val_mat <- apply(last_val_mat, 2, function(x) rep_len(x,n_needed))
  x_test <- x_test - rep_val_mat
  return(x_test)
}


# differenciating


################################################################################

plot_fused_full <- function(conf) {
  # model_path <- paste0("results/CV-fused/", model_name, "/")
  model_path <- paste0("results/CV-fused/", conf$save_folder, "/")
  full_model <- readRDS(paste0(model_path, "full-model.rds"))
  # path_config <- paste0("code/R/fused-lasso/", model_name, "/config-", model_name, ".yml")
  # conf <- config::get(file = path_config)
  
  sst_eval_path <- conf$features_eval_path
  precip_eval_path <- conf$target_eval_path
  
  sst_eval <- readRDS(sst_eval_path)
  precip_eval <- readRDS(precip_eval_path)
  
  center_response <- conf$center_response
  standardize_features <- conf$standardize_features
  standardize_response <- conf$standardize_response
  
  if(conf$small) {
    g <- readRDS("data/processed/small_graph_sst.rds")
  } 
  if(!conf$small) {
    g <- readRDS("data/processed/graph_sst.rds")
  }
  if(conf$noclust) {
    g <- readRDS("data/processed/noclust_graph_sst.rds")
  }
  
  best_l_res <- readRDS(paste0(model_path, "best-lambda-res.rds"))
  best_l_cv <- best_l_res$lambda_min
  # TODO
  # Standardize_features
  sst_cv <- readRDS(conf$features_cv_path)
  if(standardize_features == TRUE) {
    sst_cv <- standardize_train(sst_cv)
    sst_eval <- standardize_test(sst_cv, sst_eval)
    zerostd <- which(attr(sst_cv, "scale") == 0)
    graph <- delete_vertices(g, zerostd)
    print(vcount(graph) == ncol(sst_cv))
    print(ncol(sst_cv) == ncol(sst_eval))
    print("standardized features, cutted graph nodes with zero variance")
  }
  # diff
  # des
  # time vars
  preds <- predict.genlasso(full_model, Xnew = sst_eval)
  attr(preds, "best_l") <- best_l_cv
  #dim(preds$fit)
  if(center_response == TRUE) {
    precip_cv_path <- conf$target_cv_path
    precip_cv <- readRDS(precip_cv_path)
    mean_y_train <- mean(precip_cv)
    preds$fit <- apply(preds$fit, 2, function(x)  x + mean_y_train)
  }
  if(standardize_response == TRUE) {
    precip_cv_path <- conf$target_cv_path
    precip_cv <- readRDS(precip_cv_path)
    sdn_y_train <- sdN(precip_cv)
    mean_y_train <- mean(precip_cv)
    preds$fit <- apply(preds$fit, 2, function(x)  x*sdn_y_train + mean_y_train)
  }
  saveRDS(preds, paste0(model_path, "all_preds_full_model.rds"))
  print("saved all predictions")
  errors <- apply(preds$fit, 2, function(x) comp_mse(x, precip_eval))
  df <- data.frame(errors = errors, lambdas = log(preds$lambda))
  err_line_plot_full <- ggplot(df, aes(x=lambdas, y=errors)) + 
    ylab(paste("MSE full model")) +
    xlab(expression("log" ~ lambda)) +
    geom_point()
  saveRDS(err_line_plot_full, paste0(model_path,"err-mat-plots/error-line-plot-full.rds"))
  print("plotted err-line")
  rm(err_line_plot_full)
  
  # plot predictions best l
  
  if(best_l_cv < min(full_model$lambda)) {
    print("current min lambda is larger than CV min lambda, choose current min lambda")
    best_l <- min(full_model$lambda)
    h <- which.min(full_model$lambda)
  } else {
    best_l <- best_l_cv
    h <- max(which(full_model$lambda <  best_l_cv))
  }
  
  preds_bl <- predict.genlasso(full_model, Xnew = sst_eval, lambda=best_l)
  if(standardize_response == TRUE) {
    preds_bl$fit <- preds_bl$fit*sdn_y_train + mean_y_train
  }
  if(center_response == TRUE) {
    preds_bl$fit <- preds_bl$fit + mean_y_train
  }
  pnames <- names(precip_eval)
  ids <- as.integer(sub("X", "", pnames))
  plot_df <- data.frame(targets = precip_eval, ids = ids, predictions = c(preds_bl$fit))
  pred_plt <- plot_predictions(plot_df)
  attr(pred_plt, "mse") <- comp_mse(precip_eval, preds_bl$fit)
  saveRDS(pred_plt, paste0(model_path, "pred-plots/pred-plot-full.rds"))
  print("plotted predictions best l")
  rm(pred_plt)
  
  # plot_coef_map_full_fused(full_model, h, sst_cv, drop_out=TRUE, model_path)
  plot_coef_map_full_fused(full_model, h, sst_cv, drop_out=FALSE, model_path)
  print("plotted coef map")
}

plot_coef_map_full_fused <- function(full_model, h, sst_cv, drop_out, save_to) {
  all_coefs <- full_model$beta[,h]
  nonzero_coefs <- all_coefs != 0
  cnames <- colnames(sst_cv)
  nonzero_coef_names <- cnames[nonzero_coefs]
  num_coef_names <- coef_names_to_numeric(nonzero_coef_names)
  coef_mat <- cbind(num_coef_names, all_coefs[nonzero_coefs])
  if((drop_out == TRUE) & (sum(nonzero_coefs)!=0) ) {
    nz <- round(all_coefs[nonzero_coefs],6)
    b <- boxplot(nz)
    out_val <- unique(b$out)
    out_id <- nz %in% out_val
    coef_mat <- coef_mat[!out_id,]
  }
  plt <- plot_nonzero_coefficients(coef_mat)
  if(drop_out == FALSE) saveRDS(plt, paste0(save_to, "/coef-plots/", "coef-plot-full.rds"))
  if(drop_out == TRUE) saveRDS(plt, paste0(save_to, "/coef-plots/", "coef-plot-drop-out-full.rds"))
  
}


############ Plotting the CV-results ###########################################


plot_and_save_cv_results <- function(error_matrix, number_of_folds,
                                     cv_ids, lambdas, feature_data,
                                     target_data, save_to) {
  # plot maps
  plot_all_err(error_matrix, save_to)
  # plot err-mats
  plot_all_coef_maps(error_matrix, number_of_folds,
                     cv_ids, lambdas, feature_data,
                     target_data, save_to)
}

# old error plot function ******************************************************
plot_all_err <- function(error_matrix, save_to) {
  dir.create(paste0(save_to,"/err-mat-plots/"))
  error_matrix <- as.data.frame(error_matrix)
  for(i in 1:ncol(error_matrix)) {
    p <- ggplot(error_matrix, aes(x = 1:100, y = error_matrix[,i])) +
      geom_point()
    saveRDS(p, paste0(save_to,"/err-mat-plots/","err-plot-fold-",i,".rds"))
  }
}
# ******************************************************************************

# plot_all_coef_maps <- function(error_matrix, number_of_folds,
#                                cv_ids, lambdas, feature_data,
#                                target_data, save_to) {
#   dir.create(paste0(save_to,"/coef-plots/"))
#   for(i in 1:number_of_folds) {
#     p <- plot_nonzero_from_fold(error_matrix = error_matrix, fold = i,
#                                 cv_ids = cv_ids, lambdas = lambdas, 
#                                 feature_data = feature_data,
#                                 target_data = target_data)
#     saveRDS(p, paste0(save_to, "/coef-plots/", "coef-plot-fold-", i,".rds"))
#   }
# }
# 
# plot_nonzero_from_fold <- function(error_matrix, fold, cv_ids, lambdas,
#                                    feature_data, target_data) {
#   id_min <- which.min(error_matrix[,fold])
#   ids <- cv_ids$train[[fold]]
#   min_lambda <- lambdas[id_min]
#   # watch out for target dimensions and that feature_data
#   # is prepared f.e via prepare_sst
#   mod <- glmnet(feature_data[ids,], target_data[ids],
#                 lambda = min_lambda)
#   all_coef <- coef(mod)[-1,1]
#   nonzero_coef <- all_coef != 0
#   nonzero_coef_names <- names(all_coef[nonzero_coef])
#   num_coef_names <- coef_names_to_numeric(nonzero_coef_names)
#   coef_mat <- cbind(num_coef_names, all_coef[nonzero_coef])
#   plt <- plot_nonzero_coefficients(coef_mat)
#   return(plt)
# }

coef_names_to_numeric <- function(coefficient_names) {
  a <- unlist(strsplit(coefficient_names, " "))
  b <- as.numeric(a)
  c <- matrix(b, ncol = 2, byrow = TRUE)
  return(c)
}

plot_nonzero_coefficients <- function(nonzero_coef) {
  # Using GGPLOT, plot the Base World Map
  mp <- NULL
  mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
  mp <- ggplot() + mapWorld
  
  #vNow Layer the cities on top
  mp <- mp + geom_point(aes(x=nonzero_coef[,1], y=nonzero_coef[,2], 
                            colour = nonzero_coef[,3]), size=3) + 
    scale_colour_gradient2(name="Coefficient values")
  mp
}

# NEW erorr plot functions *****************************************************
# plot the error bars with ggplot 
plot_errbar_gg <- function(err_mat, loglambdas, save_to) {
  mean_mse <- apply(err_mat, 1, mean)
  sd_mse <- apply(err_mat, 1, sd)
  #median_mse <- apply(err_mat, 1, median)
  err_mat_e <- cbind(err_mat, loglambdas = loglambdas,
                     mean_mse=mean_mse, sd_mse=sd_mse)#, median=median_mse)
  err_mat_e <- as.data.frame(err_mat_e)
  me <- which.min(err_mat_e$mean_mse)
  l_min <- err_mat_e$loglambdas[me]
  p <- ggplot(err_mat_e, aes(x=loglambdas, y=mean_mse)) +
    geom_point() + geom_errorbar(aes(ymin=mean_mse-sd_mse, 
                                     ymax=mean_mse+sd_mse)) +
    ylab("MSE") + xlab(expression("log" ~ lambda)) +
    geom_vline(xintercept = l_min, linetype = "dashed", colour = "red")
  saveRDS(p, paste0(save_to,"/err-mat-plots/","err-bars-plot.rds"))
}

# plot the error for the loglambdas in each fold
plot_all_fold_errors <- function(err_mat, loglambdas, save_to) {
  err_mat_f <- cbind(err_mat, loglambdas = loglambdas)
  for(i in 1:(ncol(err_mat_f)-1)) {
    me <- which.min(err_mat_f[,i])
    l_min <- err_mat_f$loglambdas[me]
    p <- ggplot(err_mat_f, aes(x=loglambdas, y=err_mat_f[,i])) +
      ylab(paste("MSE Fold", i)) + 
      xlab(expression("log" ~ lambda)) +
      geom_point() +
      geom_vline(xintercept = l_min, linetype = "dashed", colour = "red")
    saveRDS(p, paste0(save_to,"/err-mat-plots/","err-plot-fold-",i,".rds"))
  }
}

# plot the errorbars and errors inside the fold in one step
plot_save_errors <- function(err_mat, lambdas, save_to) {
  dir.create(paste0(save_to,"/err-mat-plots/"))
  err_mat <- as.data.frame(err_mat)
  loglambdas <- log(lambdas)
  plot_errbar_gg(err_mat, loglambdas, save_to)
  plot_all_fold_errors(err_mat, loglambdas, save_to)
  print(paste("plotted all error plots and saved to", save_to))
}
# ******************************************************************************
# NEW Coef plot functions ******************************************************
plot_nonzero_coef_from_fold <- function(model, fold_nr, err_mat) {
  l_min <- which.min(err_mat[,fold_nr])
  all_coefs <- model$beta[,l_min]
  # MAYBE WE CAN DROP -1 bc we get only betas now!
  #all_coefs <- coefs[-1]
  nonzero_coefs <- all_coefs != 0
  nonzero_coef_names <- names(all_coefs[nonzero_coefs])
  num_coef_names <- coef_names_to_numeric(nonzero_coef_names)
  coef_mat <- cbind(num_coef_names, all_coefs[nonzero_coefs])
  plt <- plot_nonzero_coefficients(coef_mat)
  return(plt)
}

plot_coef_maps <- function(model_list, err_mat, save_to) {
  dir.create(paste0(save_to,"/coef-plots/"))
  for(i in 1:length(model_list)) {
    p <- plot_nonzero_coef_from_fold(model_list[[i]], i, err_mat)
    saveRDS(p, paste0(save_to, "/coef-plots/", "coef-plot-fold-", i,".rds"))
  }
}


# Plot predictions *************************************************************

plot_predictions_best_l <- function(err_mat, model_list, ids, features, target,
                                    lambdas, save_to, standardize=FALSE,
                                    include_ts_vars=FALSE, diff_features=FALSE,
                                    des_features=FALSE,
                                    standardize_features = FALSE,
                                    standardize_response = FALSE,
                                    diff_n = FALSE,
                                    stand_then_diff = FALSE) {
  dir.create(paste0(save_to,"/pred-plots/"))
  for(i in seq(length(model_list))) {
    ids_i_tr <- ids$train[[i]]
    ids_i <- ids$test[[i]]
    model_i <- model_list[[i]]
    l_min <- which.min(err_mat[,i])
    features_i <- features[ids_i,]
    target_i <- target[ids_i]
    
    
    if(stand_then_diff == TRUE) {
      features_i_tr <- features[ids_i_tr,]
      features_i_tr <- standardize_train(features_i_tr)
      features_i <- standardize_test(features_i_tr, features_i)
      max_ndiffs <- diff_n[2]
      features_i <- apply(features_i, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
      #target_i <- diff(target_i, max_ndiffs)
      target_i <- target_i[-c(seq(max_ndiffs))]
      ids_i <- ids_i[-c(seq(max_ndiffs))]
      preds <- c(predict(model_i, newx = data.matrix(features_i), s=lambdas[l_min],
                         standardize = standardize))
      plot_df <- data.frame(targets = target_i, predictions = preds, ids = ids_i)
    }
    if(include_ts_vars==TRUE) {
      features_i <- add_ts_vars(features_i)
      keep_vec <- complete.cases(features_i)
      features_i <- features_i[keep_vec,]
      target_i <- target_i[keep_vec]
      ids_i <- ids_i[keep_vec]
      preds <- c(predict(model_i, newx = data.matrix(features_i), s=lambdas[l_min],
                         standardize = standardize))
      #standardize=standardize))
      plot_df <- data.frame(targets = target_i, predictions = preds, ids = ids_i)
    }
    if(diff_features==TRUE) {
      # ndiffs <- apply(features_i, 2, unitroot_ndiffs)
      # max_ndiffs <- max(ndiffs)
      max_ndiffs <- 1
      
      nr <- nrow(features_i)
      time_ind <- seq(nr)
      fac_month <- time_ind %% 12
      fac_month[fac_month == 0] <- 12
      fac_month <- as.factor(fac_month)
      
      features_i <- apply(features_i, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
      
      time_ind <- time_ind[-c(seq(max_ndiffs))]
      fac_month <- fac_month[-c(seq(max_ndiffs))]
      features_i <- cbind(features_i, time_ind, fac_month)
      features_i <- data.matrix(features_i)
      
      # target_i <- target_i[-c(seq(max_ndiffs))]
      target_i <- diff(target_i, max_ndiffs)
      
      preds <- c(predict(model_i, newx = data.matrix(features_i), s=lambdas[l_min],
                         standardize = standardize))
      ids_i <- ids_i[-c(seq(max_ndiffs))]
      #standardize=standardize))
      plot_df <- data.frame(targets = target_i, predictions = preds, ids = ids_i)
      
      # add here possibility to add 
      # seasonal and time information
    }
    
    if(diff_n[1] == TRUE) {
      # ndiffs <- apply(features_i, 2, unitroot_ndiffs)
      # max_ndiffs <- max(ndiffs)
      max_ndiffs <- diff_n[2]
      features_i <- apply(features_i, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
      #target_i <- diff(target_i, max_ndiffs)
      target_i <- target_i[-c(seq(max_ndiffs))]
      ids_i <- ids_i[-c(seq(max_ndiffs))]
      
      if(!standardize_features){
        preds <- c(predict(model_i, newx = data.matrix(features_i), s=lambdas[l_min],
                           standardize = standardize))
        ids_i <- ids_i[-c(seq(max_ndiffs))]
        #standardize=standardize))
        plot_df <- data.frame(targets = target_i, predictions = preds, ids = ids_i)
      }
    }
    
    
    
    if(des_features==TRUE) {
      features_i <- apply(features_i, 2, function(x) c(stl(ts(x, frequency=12), s.window = "periodic",
                                                           robust = TRUE)$time.series[,"remainder"]))
      preds <- c(predict(model_i, newx = data.matrix(features_i), s=lambdas[l_min],
                         standardize = standardize))
      plot_df <- data.frame(targets = target_i, predictions = preds, ids = ids_i)
    }
    if(standardize_response == TRUE) {
      mean_target_i <- mean(target_i)
      sdn_target_i <- sdN(target_i)
      y_train <- scale(target_i, center=mean_target_i,
                       scale=sdn_target_i)
      # y_test <- scale(y_test, center=mean_y_train,
      #                 scale=sdn_y_train)
    }
    if(standardize_features == TRUE) {
      features_i_tr <- features[ids_i_tr,]
      
      if(diff_n[1] == TRUE) {
        features_i_tr <- apply(features_i_tr, 2, function(x) diff(x, lag=1, difference=max_ndiffs))
      }
      features_i_tr <- standardize_train(features_i_tr)
      features_i <- standardize_test(features_i_tr, features_i)
      
      # mean_features_i_tr <- apply(features_i_tr,2, mean)
      # sdn_features_i_tr <- apply(features_i_tr,2,sdN)
      # features_i <- scale(features_i, center=mean_features_i_tr,
      #                     scale=sdn_features_i_tr)
      # nonzero_sd_cols <- complete.cases(t(features_i))
      # features_i <- features_i[,nonzero_sd_cols]
      # # drop variables with 0 variance
      print("standardized features")
      preds <- c(predict(model_i, newx = data.matrix(features_i), s=lambdas[l_min],
                         standardize = standardize))
      if(standardize_response == TRUE) {
        preds <- preds*sdn_target_i + mean_target_i
      }
      plot_df <- data.frame(targets = target_i, predictions = preds, ids = ids_i)
    }
    
    
    if(sum(include_ts_vars,diff_features,diff_n[1], des_features, standardize_features,
           stand_then_diff)==0) {
      preds <- c(predict(model_i, newx = features[ids_i,], s=lambdas[l_min],
                         standardize=standardize))
      if(standardize_response == TRUE) {
        preds <- preds*sdn_y_train + mean_y_train
      }
      plot_df <- data.frame(targets = target[ids_i], predictions = preds, ids = ids_i)
      
    }
    plt <- plot_predictions(plot_df)
    saveRDS(plt, paste0(save_to, "/pred-plots/", "pred-plot-fold-", i,".rds"))
  }
}


plot_predictions <- function(plot_df) {
  p <- ggplot() + geom_line(data=plot_df, mapping= aes(x=ids, y=predictions, col = "red")) +
    geom_line(data=plot_df, mapping=aes(x=ids, y=targets)) +
    ylab("Precipitation") + xlab("Month") + theme(legend.position = "none")
  return(p)
}


#### FUSED PLOTS ####################################################

# plot predictions
plot_predictions_best_l_fused <- function(err_mat, model_list,
                                          ids, features, target,
                                          save_to, center_response = FALSE,
                                          standardize_features = FALSE,
                                          standardize_response = FALSE) {
  target <- scale(target, center = TRUE, scale = FALSE)
  dir.create(paste0(save_to, "/pred-plots/"))
  for(i in seq(length(model_list))) {
    ids_tr_i <- ids$train[[i]]
    ids_i <- ids$test[[i]]
    model_i <- model_list[[i]]
    l_min_id <- which.min(err_mat[,i])
    l_min <- model_i$lambda[l_min_id]
    features_i <- features[ids_i,]
    if(standardize_features == TRUE) {
      x_train_i <- features[ids_tr_i,]
      # mean_x_train_i <- apply(x_train_i,2, mean)
      # sdn_x_train_i <- apply(x_train_i,2,sdN)
      # x_train_i <- scale(x_train-i, center=mean_x_train_i,
      #                  scale=sdn_x_train_i)
      # x_test_i <- scale(features_i, center=mean_x_train_i,
      #                   scale=sdn_x_train_i)
      # features_i <- x_test_i
      x_train_i <- standardize_train(x_train_i)
      features_i <- standardize_test(x_train_i, features_i)
      print("standardized features")
      
    }
    if(standardize_response == TRUE) {
      y_train_i <- target[ids_tr_i]
      mean_y_train_i <- mean(y_train_i)
      sdn_y_train_i <- sdN(y_train_i)
      #y_test_i <- target[ids_i]
      # y_test_i <- scale(y_test_i, center=mean_y_train_i, 
      #                  scale=sdn_y_train_i)
      # # y_test <- scale(y_test, center=mean_y_train, 
      # #                 scale=sdn_y_train)
      #target_i <- y_test_i
    }
    if(center_response == TRUE) {
      y_train_i <- target[ids_tr_i]
      mean_y_train_i <- mean(y_train_i)
    }
    target_i <- target[ids_i]
    preds <- c(predict.genlasso(model_i, Xnew=features_i,
                                lambda = l_min)$fit)
    if(standardize_response == TRUE) {
      preds <- preds*sdn_y_train_i + mean_y_train_i
    }
    if(center_response == TRUE) {
      preds <- preds + mean_y_train_i
    }
    plot_df <- data.frame(targets=target_i, predictions=preds,
                          ids=ids_i)
    plt <- plot_predictions(plot_df)
    saveRDS(plt, paste0(save_to, "/pred-plots/", "pred-plot-fold-", i,".rds"))
  }
}
# plotting errors ####
plot_error_fused <- function(err_df_f, fold_i, l_min) {
  p <- ggplot(err_df_f, aes(x=log_lambda_i,
                            y=error_i)) +
    ylab(paste("MSE Fold", fold_i)) + 
    xlab(expression("log" ~ lambda)) +
    geom_point() +
    geom_vline(xintercept = l_min, linetype = "dashed", colour = "red")
  return(p)
}

plot_all_fold_error_fused <- function(model_list, err_mat, save_to) {
  dir.create(paste0(save_to, "err-mat-plots/"))
  for(i in seq(length(model_list))) {
    model_i <- model_list[[i]]
    lambda_i <- model_i$lambda
    log_lambda_i <- log(lambda_i)
    min_err_id <- which.min(err_mat[,i])
    l_min <- log_lambda_i[min_err_id]
    err_df_f <- data.frame(error_i = err_mat[,i],
                           log_lambda_i = log_lambda_i)
    p <- plot_error_fused(err_df_f, fold_i = i, l_min = l_min)
    saveRDS(p, paste0(save_to, "/err-mat-plots/", "err-plot-fold", i, ".rds"))
  }
}

plot_errline_gg_fused <- function(model_list, err_mat, save_to) {
  dir.create(paste0(save_to, "err-mat-plots/"))
  nm <- length(model_list)
  l_mat <- matrix(NA, nrow=nrow(err_mat),ncol=nm)
  for(i in seq(nm)) {
    l_mat[,i] <- log(model_list[[i]]$lambda)
  }
  df_list <- list()
  for(i in seq(nm)) {
    df_list[[i]] <- data.frame(err = err_mat[,i], loglambda = l_mat[,i],
                               fold = i)
  }
  da <- df_list[[1]]
  for(i in seq(length(df_list)-1)) {
    da <- full_join(da, df_list[[i+1]])
  }
  p <- ggplot(data=da, aes(x=loglambda, y=err)) +
    geom_point(aes(color=factor(fold))) +
    ylab("MSE") + 
    xlab(expression("log" ~ lambda))
  saveRDS(p, paste0(save_to, "err-mat-plots/err-line-plot.rds"))
  
  #return(p)
}

plot_nonzero_coef_from_fold_fused <- function(model, fold_nr, err_mat, features,
                                              target, standardize_features,ids,
                                              drop_out) {
  l_min <- which.min(err_mat[,fold_nr])
  all_coefs <- model$beta[,l_min]
  # ask fabi
  cnames <- colnames(features)
  if(standardize_features==TRUE){
    # https://stats.stackexchange.com/questions/74622/converting-standardized-betas-back-to-original-variables
    # sdny <- sdN(target)
    # sdn_vec <- apply(features_i, 2, custom_stand)
    # all_coefs <- (all_coefs*sdny)/sdn_vec
    # all_coefs <- all_coefs/sdn_vec
    
    # features_i <- standardize_train(features_i)
    
    
    # features_i <- features[ids$train[[fold_nr]],]
    # features_i <- standardize_train(features_i)
    # vec <- attr(features_i, "scale")
    
    # target_i <- target[ids$train[[fold_nr]]]
    # sdn_y_i <- sdN(target_i)
    # sdn_x_i <- apply(features_i, 2, sdN)
    # all_coefs <- (all_coefs/sdn_x_i) * sdn_y_i
    
    features_i <- features[ids$train[[fold_nr]], ]
    features_i <- standardize_train(features_i)
    cnames <- colnames(features_i)
    print("showing coefficients on standardized scale")
    
  }
  # MAYBE WE CAN DROP -1 bc we get only betas now!
  #all_coefs <- coefs[-1]
  nonzero_coefs <- all_coefs != 0
  # cnames <- colnames(features)
  nonzero_coef_names <- cnames[nonzero_coefs]
  num_coef_names <- coef_names_to_numeric(nonzero_coef_names)
  coef_mat <- cbind(num_coef_names, all_coefs[nonzero_coefs])
  if((drop_out == TRUE) & (sum(nonzero_coefs)!=0) ) {
    nz <- round(all_coefs[nonzero_coefs],6)
    b <- boxplot(nz)
    out_val <- unique(b$out)
    out_id <- nz %in% out_val
    coef_mat <- coef_mat[!out_id,]
  }
  plt <- plot_nonzero_coefficients(coef_mat)
  return(plt)
}


plot_coef_maps_fused <- function(model_list, err_mat, save_to,
                                 features,
                                 target,
                                 standardize_features,
                                 ids,
                                 drop_out) {
  dir.create(paste0(save_to,"/coef-plots/"))
  for(i in 1:length(model_list)) {
    p <- plot_nonzero_coef_from_fold_fused(model_list[[i]], i, err_mat,
                                           features,
                                           target,
                                           standardize_features,
                                           ids,
                                           drop_out)
    if(drop_out == FALSE) {
      saveRDS(p, paste0(save_to, "/coef-plots/", "coef-plot-fold-", i,".rds"))
    }
    if(drop_out == TRUE) {
      saveRDS(p, paste0(save_to, "/coef-plots/", "coef-plot-drop-out-fold-", i,".rds"))
      
    }
  }
}

plot_cv_fused <- function(conf) {
  
  # model_path <- paste0("results/CV-fused/", model_name, "/")
  # path_config <- paste0("code/R/fused-lasso/", model_name, "/config-", model_name, ".yml")
  # conf <- config::get(file = path_config)
  model_path <- paste0("results/CV-fused/", conf$save_folder, "/")
  
  sst_path <- conf$features_cv_path
  precip_path <- conf$target_cv_path
  standardize_features <- conf$standardize_features
  standardize_response <- conf$standardize_response
  
  sst_cv <- readRDS(sst_path)
  precip_cv <- readRDS(precip_path)
  
  ids <- readRDS(paste0(model_path, "index-list.rds"))
  err_mat <- readRDS(paste0(model_path, "err-mat.rds"))
  mins <- apply(err_mat, 2, which.min)
  
  model_list <- load_models(paste0(model_path, "fold-models"))
  
  plot_all_fold_error_fused(model_list, err_mat, save_to = model_path)
  plot_errline_gg_fused(model_list, err_mat, save_to = model_path)
  print("finished error plots")
  plot_predictions_best_l_fused(err_mat = err_mat, model_list = model_list,
                                ids = ids, features = sst_cv,
                                target = precip_cv, save_to = model_path,
                                standardize_features = standardize_features,
                                standardize_response = standardize_response)
  print("finished prediction plots")
  plot_coef_maps_fused(model_list, err_mat, save_to = model_path,
                       features = sst_cv,
                       target = precip_cv,
                       standardize_features = standardize_features,
                       ids = ids,
                       drop_out  = FALSE)
  # plot_coef_maps_fused(model_list, err_mat, save_to = model_path,
  #                      features = sst_cv,
  #                      target = precip_cv,
  #                      standardize_features = standardize_features,
  #                      ids = ids,
  #                      drop_out  = TRUE)
  print("finished coefficient plots")
}

# MISC ********************************************************************#####
load_models <- function(path_to_models) {
  l <- list()
  n_models <- length(list.files(path_to_models))
  for(i in seq(n_models)) {
    l[[i]] <- readRDS(paste0(path_to_models,"/model-fold-",
                             i, ".rds"))
  }
  return(l)
}

comp_mse <- function(predictions, target) {
  return(mean((predictions-target)^2))
}

get_mse_from_pred_plot <- function(pred_plot) {
  a <- ggplot_build(pred_plot)$data[[1]]$y
  b <- ggplot_build(pred_plot)$data[[2]]$y
  mse <- comp_mse(a,b)
  return(mse)
}

replot_err_mat <- function(err_mat_list) {
  l <- length(err_mat_list)
  r_vec <- matrix(nrow=l,ncol=2)
  for(i in seq(l)) {
    plot_i <- err_mat_list[[i]]
    r_vec[i,] <- range(ggplot_build(plot_i)$data[[1]]$y)
  }
  new_range <- range(r_vec)
  new_list <- list()
  for(i in seq(l)) {
    new_list[[i]] <- err_mat_list[[i]] + ylim(new_range)
  }
  return(new_list)
}

estimate_runtime <- function(hours,min,sec,steps) {
  rt <- ((hours*3600)+(min*60+sec)) / steps * 20000 / 60 / 60
  return(rt)
}

############ Run/Evaluate cluster CV ###########################################

fit_full_model_for_cluster <- function(sst, full_save_to, ncluster) {
  cluster_means_list <- readRDS(paste0(full_save_to, "cluster-means-list.rds"))
  for(i in seq(ncluster)) {
    cluster_path <- paste0(full_save_to, "/cluster-", i, "/")
    err_mat <- readRDS(paste0(cluster_path, "err-mat.rds"))
    lambdas <- readRDS(paste0(cluster_path, "lambda-vec.rds"))
    l_min_id <- which.min(apply(err_mat, 1, mean))
    l_min <- lambdas[l_min_id]
    precip <- cluster_means_list[[i]]
    ids <- readRDS(paste0(cluster_path,"index-list.rds"))
    start_eval <- max(ids$test[[length(ids$test)]])+1
    end_eval <- nrow(sst)
    sst_train <- sst[1:(start_eval-1),]
    precip_train <- precip[1:(start_eval)-1]
    sst_eval <- sst[start_eval:end_eval,]
    precip_eval <- precip[start_eval:end_eval]
    full_model <- glmnet(sst_train, precip_train, lambda=l_min,
                         standardize=FALSE)
    saveRDS(full_model, paste0(cluster_path, "full-model.rds"))
    # plot coefficients
    all_coefs <- full_model$beta
    nonzero_coefs <- which(all_coefs != 0)
    nonzero_coefs_names <- rownames(all_coefs)[nonzero_coefs]
    num_coef_names <- coef_names_to_numeric(nonzero_coefs_names)
    coef_mat <- cbind(num_coef_names, all_coefs[nonzero_coefs])
    coef_plt <- plot_nonzero_coefficients(coef_mat)
    saveRDS(coef_plt, paste0(cluster_path,"coef-plots/coef-plot-full.rds"))
    # predict on evaluation data
    preds <- c(predict(full_model, newx = sst_eval))
    # plot predictions against true data
    df <- data.frame(predictions = preds, targets = precip_eval, ids=c(start_eval:end_eval))
    plt <- plot_predictions(df)
    plt
    saveRDS(plt, paste0(cluster_path,"pred-plots/pred-plot-full.rds"))
    print(comp_mse(preds, precip_eval))
    print(paste("Done fitting and evaluating the full model with l_min for cluster", i))
  }
}

run_cv_over_clusters <- function(features, cluster_means_list, nfold=5, size_train=60, 
                                 size_test=14, 
                                 save_folder_cluster_result) {
  save_folder_long <- paste0("results/CV-lasso/", save_folder_cluster_result)
  dir.create(paste0(save_folder_long))
  l <- length(cluster_means_list)
  for(i in seq(l)) {
    save_folder_cluster_i <- paste0(save_folder_cluster_result,"/cluster-", i)
    dir.create(save_folder_cluster_i)
    precip_i <- cluster_means_list[[i]]
    err_mat_i <- cv_for_ts(sst = features, precip = precip_i, nfold = nfold,
                           size_train = size_train, size_test=size_test,
                           save_folder = save_folder_cluster_i)
  }
}

create_cluster_precip_list <- function(precip, clustering_result) {
  precip <- getValues(precip)
  cluster_precip_list <- list()
  cluster_vec <- clustering_result$cluster
  ncluster <- length(unique(cluster_vec))
  for(i in seq(ncluster)) {
    ids <- which(cluster_vec == i)
    cluster_precip_list[[i]] <- precip[ids,]
  }
  return(cluster_precip_list)
}

compute_cluster_means <- function(cluster_precip_list) {
  lapply(cluster_precip_list, function(x) apply(x, 2, mean))
}

evaluate_cluster_cv <- function(path_to_model_folder, cluster_means_list,
                                sst, ncluster) {
  for(i in seq(ncluster)) {
    load_path <- paste0(path_to_model_folder, "/cluster-", i, "/")
    dir.create(load_path)
    err_mat <- readRDS(paste0(load_path, "err-mat.rds"))
    lambdas <- readRDS(paste0(load_path, "lambda-vec.rds"))
    ids <- readRDS(paste0(load_path, "index-list.rds"))
    model_list <- load_models(paste0(load_path,"fold-models"))
    plot_save_errors(err_mat, lambdas, save_to = load_path)
    plot_coef_maps(model_list, err_mat = err_mat, save_to=load_path)
    precip <- cluster_means_list[[i]]
    plot_predictions_best_l(err_mat, model_list, ids, features=sst, target=precip,
                            lambdas, save_to = load_path)
  }
}

############ Fused Lasso helpers ###############################################
create_coords <- function(vec) {
  x_vec <- rep(c(1:vec[1]), vec[2])
  y_vec <- c()
  for(i in 1:vec[2]) {
    y_vec <- append(y_vec,(rep(i, vec[1])))
  }
  df <- as.data.frame(cbind(x_vec, y_vec))
  return(df)
}

igraph_from_raster <- function(raster_object, create_weighted_pen = FALSE) {
  dims <- dim(raster_object)[1:2]
  vals <- values(raster_object[[1]])
  land <- which(is.na(vals))
  rm(raster_object)
  
  dims <- rev(dims)
  g <- make_lattice(dims)
  ec <- create_coords(dims)
  V(g)$x <- ec$x_vec
  V(g)$y <- rev(ec$y_vec)

  if (create_weighted_pen) pen_mat <- create_pen_mat(g, dims, ec)
  
  g <- delete_vertices(g, land)
  pen_mat <- pen_mat[,-land]
  attr(g, "weighted_pen_mat") <- pen_mat
  return(g)
}

#gets the weights from the lattice graph distances
#we can reuse these weights for a full graph
#and delete the NA (aka land) vertices in the sst raster
#to prevent clusters of SST or grand lake areas in the graph
#but so far not sure if weights can be used in fused lasso
get_weights <- function(lattice_graph) {
  dist_latt <- distances(lattice_graph)
  dist_latt[upper.tri(dist_latt)] <- 0
  w <- (dist_latt[dist_latt!=0])^(-1)
  return(w)
}

get_weighted_pen_mat <- function(pen_mat, weights) {
  pen_mat <- pen_mat*sqrt(weights)
  return(pen_mat)
}

# TODO
# compute weights from lattice
# create full graph from dims
# give full graph same coordinates
# give full graph weights
# create sparse D from full graph
# get weighted sparse D (get_weighted_pen_mat)
# check if DTD is laplacian of full

create_pen_mat <- function(g, dims, ec) {
  ws <- get_weights(g)
  f <- make_full_graph(dims[1]*dims[2])
  V(f)$x <- ec$x_vec
  V(f)$y <- rev(ec$y_vec)
  E(f)$weight <- ws
  pen_mat <- getDgSparse(f)
  pen_mat <- get_weighted_pen_mat(pen_mat, ws)
  return(pen_mat)
}
# take raster obj
# create lattice
# get weights
# create full
# add weights
# delete land
# get dg sparse from graph
# multiply rows with weight
# give D as penalty matrix to fusedlasso
# check with test
library(raster)

############### run all code ###################################################
run_all_fused <- function(model_name, parts = c("all")) {
  path_config <- paste0("code/R/fused-lasso/", model_name,"/", "config-", model_name, ".yml")
  conf <- config::get(file = path_config)
  if(is.null(conf$center_response)) {
    conf$center_response <- FALSE
  }
  if(is.null(conf$noclust)) {
    conf$noclust <- FALSE
  }
  if(conf$center_response & conf$standardize_response) {
    stop("cant center AND standardize response, choose only on in the config")
  }
  # setwd("Repos/MA-climate/")
  # load packages
  library(sp)
  library(raster)
  library(igraph)
  library(genlasso)
  
  library(genlasso)
  library(ggplot2)
  library(dplyr)
  
  # load data
  features <- readRDS(conf$features_cv_path)
  targets <- readRDS(conf$target_cv_path)
  
  # load graph
  if(conf$small) {
    g <- readRDS("data/processed/small_graph_sst.rds")
  } 
  if(!conf$small) {
    g <- readRDS("data/processed/graph_sst.rds")
  }
  if(!is.null(conf$noclust)) {
    g <- readRDS("data/processed/noclust_graph_sst.rds")
    
  }
  
  if(parts == "all") parts <- c("cv", "plot-cv", "get-best-l", "fit-full",
                                "check-iterate", "plot-full")
  
  if("cv" %in% parts){
    cv_for_ts(sst = features, 
              precip = targets, 
              nfold = conf$nfold, 
              size_train = conf$size_train,
              size_test = conf$size_test,
              save_folder = conf$save_folder,
              model = conf$model,
              parallelize = conf$parallelize,
              graph = g,
              maxsteps = conf$maxsteps,
              include_ts_vars = conf$include_ts_vars,
              diff_features = conf$diff_features,
              des_features = conf$des_features,
              center_response = conf$center_response,
              standardize_features = conf$standardize_features, 
              standardize_response = conf$standardize_response,
              gamma = conf$gamma
              )
    print("finished cv")
  }
  if("plot-cv" %in% parts) {
    plot_cv_fused(conf = conf)
    print("plotted cv")
  }
  if("get-best-l" %in% parts) {
    get_best_lambda_fused(conf = conf)
    print("got best l")
  }
  if("fit-full" %in% parts) {
    fit_full_fused(conf = conf)
    print("fitted and save full model")
  }
  if("check-iterate" %in% parts) {
    check_and_iterate_fused(conf = conf)
    print("checked and if necessary iterated full model")
  }
  if("plot-full" %in% parts) {
    plot_fused_full(conf = conf)
    print("plotted full model")
  }
}

# plot testers inside lasso CV##################################################
# trajectory of errors on test set for different lambdas
plot_fold_errors_lasso <- function(err_col, lambda_vec, j) {
  ll <- log(lambda_vec)
  p_df <- data.frame(cbind(err = err_col, loglambdas = ll))
  id_min_l <- attr(err_col, "id_min_l")
  l_min <- ll[id_min_l]
  p <- ggplot(p_df, aes(x=loglambdas, y=err)) +
    ylab(paste("MSE Fold", j)) + 
    xlab(expression("log" ~ lambda)) +
    geom_point() +
    geom_vline(xintercept = l_min, linetype = "dashed", colour = "red")
  return(p)
}


# predictions inside fold for best lambda
plot_fold_preds_lasso <- function(err_col, trained_model, x_train, y_train, id_train) {
  min_l <- attr(err_col, "min_l")
  best_fit <- c(predict(trained_model, newx = x_train, s = min_l))
  plot_df <- data.frame(predictions = best_fit, targets = y_train, ids = id_train)
  p <- plot_predictions(plot_df)
  return(p)
}

# predictions on test set for best lambda
plot_test_preds_lasso <- function(err_col, predicted, y_test, id_test) {
  id_min_l <- attr(err_col, "id_min_l")
  best_pred <- c(predicted[,id_min_l])
  plot_df <- data.frame(predictions = best_pred, targets = y_test, ids = id_test)
  p <- plot_predictions(plot_df)
  return(p)
} 

# plot nonzero coefficients on map
plot_nonzero_coef_lasso <- function(model, err_col) {
  id_min_l <- attr(err_col, "id_min_l")
  all_coefs <- model$beta[,id_min_l]
  nonzero_coefs <- all_coefs != 0
  nonzero_coef_names <- names(all_coefs[nonzero_coefs])
  num_coef_names <- coef_names_to_numeric(nonzero_coef_names)
  coef_mat <- cbind(num_coef_names, all_coefs[nonzero_coefs])
  plt <- plot_nonzero_coefficients(coef_mat)
  return(plt)
}

# TESTING
get_l_1se_high_low <- function(err_bar_plot) {
  a <- ggplot_build(err_bar_plot)
  #d1 <- a$data[[1]]
  d2 <- a$data[[2]]
  d3 <- a$data[[3]]
  l_min_id <- which(d2$x==d3$xintercept)
  l_min <- d2$x[l_min_id]
  
  # largest lambda so that
  b <- (d2$y <= d2$ymax[l_min_id])
  
  b_left <- d2$x < l_min
  l_1se_low <- d2$x[max(which(b & b_left))]
  if(l_1se_low == length(d2$x)) {
    message("lambda 1se low is minimum lambda")
  }
  
  b_right <- d2$x > l_min
  l_1se_high <- d2$x[min(which(b & b_right))]
  if(l_1se_high==1) {
    message("lambda 1se is maximum lambda")
  }
  
  l_list <- list(l_1se_high = l_1se_high, l_1se_low=l_1se_low)
  
  return(l_list)
}
# TESTING
# err_bar_plot_new <- err_bar_plot +    
#   geom_vline(xintercept = l_list$l_1se_high, linetype = "dashed", colour = "blue") +
#   geom_vline(xintercept = l_list$l_1se_low, linetype = "dashed", colour = "purple")

# plot testers inside fused CV##################################################