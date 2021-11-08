# helper functions
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
# out: a ggplot of point on map
plot_cell <- function(cell) {
  # Using GGPLOT, plot the Base World Map
  mp <- NULL
  mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
  mp <- ggplot() + mapWorld
  
  #vNow Layer the cities on top
  mp <- mp + geom_point(aes(x=cell[1], y=cell[2]), ,color="blue", size=3)
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
    scale_fill_viridis_c(option="A") +
    theme_bw() +
    coord_quickmap() 
  return(plt)
}

###############For Decomposition################################################

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

#######################For Correlatio Analysis##################################

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

#######################For Time Series Cv##################################

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
    data <- data[-c(1:obs_to_drop),]
    return(data)
  }
  return(data)
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
  inner_prods <- precip %*% scale(sst, center = TRUE, scale = TRUE)
  #TODO get the max abs value
  max_inner <- max(abs(inner_prods))
  #TODO comput lambdamax
  max_lambda <- max_inner/nrow(sst)
  #TODO compute lambdamin
  min_lambda <- 0.001*max_lambda
  # TODO create vector with lambda values
  # create vector of lambdas
  # evenly spaced points on log scale between mx and mn
  lambda_vec <- exp(seq(log(min_lambda),log(max_lambda), length.out = 100))
  return(lambda_vec)
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
cv_for_ts <- function(sst, precip, nfold, size_train, size_test, save_folder) {
  set.seed(1234)
  #TODO aswer question:compute precip mean here or before?
  sst <- prepare_sst(sst)
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
  lambda_vec <- get_lambda_values(sst, precip)
  err_mat <- matrix(NA, ncol = nfold, nrow = length(lambda_vec))
  for(i in 1:length(lambda_vec)) {
    for(j in 1:length(index_list$train)) {
      id_train <- unlist(index_list$train[j], use.names = FALSE)
      id_test <- unlist(index_list$test[j], use.names = FALSE)
      x_train <- sst[id_train,]
      y_train <- precip[id_train]
      x_test <- sst[id_test,]
      y_test <- precip[id_test]
      # die cross validaion is done here classically
      # does not work!!!
      trained_model <- glmnet(x_train, y_train)
      #TODO change value her for s
      predicted <- predict(trained_model, newx = x_test, s = lambda_vec[i])
      err <- mean((predicted-y_test)^2)
      err_mat[i,j] <- err
      #save(trained_model, file=paste0("results/CV-lasso/model-","lambda-",i,"fold-",j,".RData"))
    }
  }
  index_list_path <- paste0("results/CV-lasso/", save_folder, "/index-list.rds")
  lambda_vec_path <- paste0("results/CV-lasso/", save_folder, "/lambda-vec.rds")
  err_mat_path <- paste0("results/CV-lasso/", save_folder, "/err-mat.rds")
  saveRDS(index_list, file = index_list_path)
  saveRDS(lambda_vec, file = lambda_vec_path) 
  saveRDS(err_mat, file = err_mat_path)
  return(err_mat)
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


