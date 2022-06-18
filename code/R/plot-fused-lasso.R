# Plot fused lasso

co2 <- coef.genlasso(full_mod)
length(co2$beta)
dim(co2$beta)
a <- co2$beta[,2000]
plot(density(a))
names(a)
length(cnames)
num_coef_names <- coef_names_to_numeric(colnames(sst))
dim(num_coef_names)
coef_mat <- cbind(num_coef_names, a)
plot1 <- plot_nonzero_coefficients(coef_mat)
plot1

#replot without large values
a[a< -1] <- 0
coef_mat <- cbind(num_coef_names, a)
plot2 <- plot_nonzero_coefficients(coef_mat)
plot2


