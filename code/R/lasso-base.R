# code for lasso baseline
# actually it should be possible to create a dataframe sa follows
# y = amazon basin averageed
# t starts at 0
# month t is month of the year, or indicator for spring summer etc
# number of year
# interaction month and grid point coordinate?
# deseasonalize the time series beforehand?
# lat lon
# y = t + point + seasonal + point::seasonal 
setwd("MA-climate")
library(dplyr)
library(MASS)
library(glmnet)

# load both data sets
# WARNINNG data does not exist anymore
load("data/processed/hadcrut-processed.Rda")
load("data/processed/cru-processed.Rda")

# for drought col is named wrongly
drought %>% rename(drought = temp) -> drought
any(is.na(drought$drought)) # FALSE
drought %>% group_by(time) %>% 
  dplyr::summarise(drought_mean = mean(drought, na.rm = TRUE)) -> drought

t_drought <- drought %>% filter(time > 1300)
t_temp <- temp %>% filter(time > 1300)
t_temp <- na.omit(t_temp)
base::merge(t_drought, t_temp, by = "time") -> data
any(is.na(data)) #FALSE

data$coord <- factor(data$coord)
X <- model.matrix(~., dplyr::select(data, -drought_mean))
Y <- data %>% dplyr::select(drought_mean) %>% as.matrix()
fit_lasso <- glmnet(X, Y, family = "gaussian", alpha = 1)
print(fit_lasso)
plot(fit_lasso)
head(fit_lasso)
length(coef(fit_lasso))
dim(X)

nz <- coef(fit_lasso)[,1] != 0
rownames(coef(fit_lasso))[nz]

# do one regression model for each coordinate?
etst <- model.matrix(~ time + temp + as.factor(coord), temp)
head(temp)


temp$coord <- factor(temp$coord)
X <- model.matrix(~., dplyr::select(temp, -temp))
head(t_drought)
head(t_temp)
head(data)
