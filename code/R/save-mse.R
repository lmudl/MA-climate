save_mse <- function(path_to_model_folder) {
  full_preds <- readRDS(paste0(path_to_model_folder, "/pred-plots/pred-plot-full.rds"))
  mse_full <- get_mse_from_pred_plot(full_preds)
  saveRDS(mse_full, "mse_full_model_eval.rds")
  print(mse_full)
}

# path_to_model_folder <- "./results/CV-lasso/test-deseas-lasso/"
path_to_model_folder <- "./results/CV-fused/noclust-large-fused-5k-gamma-005//"

#setwd("..")
save_mse(path_to_model_folder)
# lasso 1314.929
# lasso stand 1214.489
# lasso diff1 1361.82
# lasso deseas 1809.455

# fused lasso
# fused lasso 1131.709
# noclust 1070.042
# noclust gamma 01, 1840.589
# noclust gamma 05, 1836.632
