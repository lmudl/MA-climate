# evaluate fused-cv-full-100
getwd() # is MA-climate on the shell 
# but we could add a check here
#setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")
library(sp)
library(raster)
library(genlasso)
library(ggplot2)
# load model
full_mod <- readRDS("results/CV-lasso/fused-cv-test-100-steps/full-model.rds")
table(full_mod$beta[,100])
# load evaluation data
val_obs <- 371:432
# load and prepare sst for evaluation
sst <- brick("data/interim/sst/ersst_setreftime.nc", var = "sst")
ext <- extent(-180,0,-50,40)
sst <- crop(sst,ext)
coord <- coordinates(sst)
cnames <- paste(coord[,1], coord[,2])
sst <- as.matrix(sst)
sst <- t(sst)
colnames(sst) <- cnames
sst <- prepare_sst(sst)
sst <- sst[val_obs,]

# load and prepare sst for evaluation
precip <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
precip <- values(precip)
precip <- apply(precip, 2, mean)
precip <- precip[val_obs]

preds <- predict.genlasso(full_mod, Xnew=sst)
dim(preds$fit)

# get errors from precip
# get rmse from each col
errs <- apply(preds$fit, 2, function(x) mean((x-precip)^2))
minmin <- which.min(errs)
min(errs)
plot(errs)
# plot predictions and validation data together

df <- data.frame(preds = preds$fit[,100], target = precip)
#dim(preds_full)
ggplot() + geom_line(data = df, mapping = aes(x=1:62, y=preds, colour = "blue")) +
  geom_line(data = df, mapping= aes(x=1:62, y=target))

#plot coefficients

# Plot fused lasso

co2 <- coef.genlasso(full_mod)
length(co2$beta)
dim(co2$beta)
a <- co2$beta[,100]
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



