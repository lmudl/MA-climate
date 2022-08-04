# replot from full model should be relatively easy
# thats nice to know because we can actually only load the 
# models and "resave" the model plots
# this will save us a lot of trouble when knitting the whole document

# but we need the info which was the best-lambda here this is h
replot_coef_plots <- function(model, h) {
  all_coefs <- model$beta[,h]
  nonzero_coefs <- all_coefs != 0
  cnames <- colnames(model$X)
  nonzero_coef_names <- coef_names_to_numeric(nonzer_coef_names)
  coef_mat <- cbind(num_coef_names, all_coefs[nonzer_coefs])
  if(drop_out == TRUE & ((sum(nonzero_coefs)!=0))) {
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