# fit-full model for each cluster

# load data

# start loop like in evaluate clustered
# then fit and plot full model

save_to <- "cluster-cv-lasso-og"
full_save_to <- paste0("results/CV-lasso/", save_to, "/")

#inside loop with function
#load err_mat, load lambdas
#get lminid and lmin

#load means of precip

#load sst 
sst <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
sst <- as.matrix(sst)
sst <- add_colnames("data/interim/sst/ersst_setreftime.nc",sst)
sst <- prepare_sst(sst)

#split in evaluation and train

#fit the full model

#predict on evaluation data

#plot predictions agains true data

#plot predictions against true data


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
    # predict on evaluation data
    preds <- c(predict(full_model, newx = sst_eval))
    
    # plot predictions against true data
    df <- data.frame(predictions = preds, targets = precip_eval)
    plt <- ggplot() + geom_line(data=df, mapping= aes(x=seq(length(predictions)), 
                                                      y=predictions, col = "red")) +
      geom_line(data=df, mapping=aes(x=seq(lengths(predictions)), y=targets))
    plt
    saveRDS(plt, paste0(cluster_path,"pred-plots/pred-plot-full.rds"))
    print(comp_mse(preds, precip_eval))
    print(paste("Done fitting and evaluating the full model with l_min for cluster", i))
  }
}

fit_full_model_for_cluster(sst, full_save_to, ncluster=5)

