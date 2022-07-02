# play with normal glmnet and log transformation
library(glmnet)
tm <- cv.glmnet(small_sst_cv, log(precip_cv), standardize = TRUE)
tm2 <- cv.glmnet(small_sst_cv, log(precip_cv), standardize = TRUE,
                 alpha = 0.5)
s_precip_cv <- scale(precip_cv)
tm3 <- cv.glmnet(small_sst_cv, precip_cv, standardize = TRUE,
                 alpha=0.1, standardize.response=TRUE)
tm4 <- cv.glmnet(small_sst_cv, log(precip_cv), family=gaussian(link=log))
plot(tm4)
pr <- predict(tm3, newx=small_sst_eval)
#plot(ts(pr))
#precip_eval <- readRDS("data/processed/precip_eval.rds")
dev.off()
plot(ts(precip_eval))
plot(ts(pr))
#s_precip_eval <- scale(precip_eval)
comp_mse(precip_eval, pr)
exp(min(tm$cvm))
min(tm2$cvm)
plot(tm)
?cv.glmnet
plot(tm2)
unscale(s_precip_eval) == precip_eval
cbind(unscale(s_precip_eval), precip_eval)
MASE(s_precip_eval-pr)
mean(precip_eval)
sd(precip_eval)
pr*sd(precip_eval)
?glmnet
?scale
