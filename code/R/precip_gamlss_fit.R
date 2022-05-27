install.packages("gamlss")
#library(gamlss.dist)
library(gamlss)
x <- precip_dens$data$value
fit <- fitDist(x, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)
summary(fit)
saveRDS(fit, "results/gamlss_precip_fit.rds")
plot(fit)

