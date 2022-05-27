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
  mp <- mp + geom_point(aes(x=cell[,1], y=cell[,2]), ,color="blue", size=3)
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

# plot a summary done by raster calc function, f.e plot(precip, calc(precip, mean))
# in: here, precip <- aggregate::precip() and the summary computed by calc()
# out: plot showing the summary on a map
plot_summary <- function(data, summary) {
  df <- base::as.data.frame(cbind(coordinates(data), summary@data@values))
  colnames(df) <- c("Longitude","Latitude", "val")
  plt <- ggplot(data = df, aes(x = Longitude, y = Latitude, fill = val)) +
    geom_raster(interpolate = FALSE) +
    scale_fill_gradient2(low = "red", high = "blue", midpoint = mean(df$val))
  plt
}

# plot the trends of the data after computing it from the data
# in: data for example: aggregate::precip() and the trends computed by get_trend
# out: plot showing the trend values on a map
plot_trends <- function(data, trends) {
  df <- base::as.data.frame(cbind(coordinates(data), trends))
  colnames(df) <- c("Longitude","Latitude", "val")
  plt <- ggplot(data = df, aes(x = Longitude, y = Latitude, fill = val)) +
    geom_raster(interpolate = FALSE) +
    scale_fill_gradient2(low = "red", high = "blue", midpoint = mean(df$val))
  plt
}

# get the trends of raster time series by computing a linear model w/ intercept
# to be applied in a apply function call, f.e. trends <- apply(m, 1, get_trend)
# in: vector/ row of a matrix that consists row is lot/lan col is month
# out: the trend over the respective time series

get_trend <- function(vec) {
  b <- seq(length(vec))
  trend <- lm(vec ~ b)$coefficients[2]
  return(trend)
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
  # TODO answer question:compute precip mean here or before?
  sst <- prepare_sst(sst)
  assertthat::are_equal(nrow(sst), length(precip))
  n_row <- nrow(sst)
  obs_to_drop <- get_obs_to_drop(n_row, nfold)
  sst <- drop_obs(sst, obs_to_drop)
  precip <- drop_obs(precip, obs_to_drop)
  # now we made sure that nrow(data) %% nfold == 0
  assertthat::are_equal(size_train+size_test, nrow(sst)/nfold)
  index_list <- caret::createTimeSlices(1:nrow(sst), initialWindow=size_train, horizon=size_test,
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
  #for(i in 1:length(lambda_vec)) {
  for(j in 1:length(index_list$train)) {
    id_train <- unlist(index_list$train[j], use.names = FALSE)
    id_test <- unlist(index_list$test[j], use.names = FALSE)
    x_train <- sst[id_train,]
    y_train <- precip[id_train]
    x_test <- sst[id_test,]
    y_test <- precip[id_test]
    # die cross validaion is done here classically
    # does not work!!!
    # trained_model <- glmnet(x_train, y_train)
    trained_model <- glmnet(x_train, y_train, lambda=rev(lambda_vec))
    #TODO change value her for s
    predicted <- predict(trained_model, newx = data.matrix(x_test), s = rev(lambda_vec))
    err_col <- apply(predicted, 2, function(x) mean((x-y_test)^2))
    err_mat[,j] <- err_col
    #save(trained_model, file=paste0("results/CV-lasso/model-","lambda-",i,"fold-",j,".RData"))
    #print(paste("lambda", i, "fold", j))
    print(paste("finished fold", j))
  }
  #print(paste("finished lambda", i))
  #}
  print("finished fitting")
  dir.create(paste("results/CV-lasso/",save_folder))
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

############ Plotting the CV-results ###########
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

plot_all_err <- function(error_matrix, save_to) {
  dir.create(paste0(save_to,"/err-mat-plots/"))
  for(i in 1:ncol(error_matrix)) {
    p <- plot(error_matrix[,i])
    saveRDS(p, paste0(save_to,"/err-mat-plots/","err-plot-fold-",i,".rds"))
  }
}

plot_all_coef_maps <- function(error_matrix, number_of_folds,
                               cv_ids, lambdas, feature_data,
                               target_data, save_to) {
  dir.create(paste0(save_to,"/coef-plots/"))
  for(i in 1:number_of_folds) {
    p <- plot_nonzero_from_fold(error_matrix = error_matrix, fold = i,
                                cv_ids = cv_ids, lambdas = lambdas, 
                                feature_data = feature_data,
                                target_data = target_data)
    saveRDS(p, paste0(save_to, "/coef-plots/", "coef-plot-fold-", i,".rds"))
  }
}

plot_nonzero_from_fold <- function(error_matrix, fold, cv_ids, lambdas,
                                   feature_data, target_data) {
  id_min <- which.min(error_matrix[,fold])
  ids <- cv_ids$train[[fold]]
  min_lambda <- lambdas[id_min]
  # watch out for target dimensions and that feature_data
  # is prepared f.e via prepare_sst
  mod <- glmnet(feature_data[ids,], target_data[ids],
                lambda = min_lambda)
  all_coef <- coef(mod)[-1,1]
  nonzero_coef <- all_coef != 0
  nonzero_coef_names <- names(all_coef[nonzero_coef])
  num_coef_names <- coef_names_to_numeric(nonzero_coef_names)
  coef_mat <- cbind(num_coef_names, all_coef[nonzero_coef])
  plt <- plot_nonzero_coefficients(coef_mat)
  return(plt)
}

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
                            colour = nonzero_coef[,3]), size=3) + scale_colour_gradient2()
  mp
}


