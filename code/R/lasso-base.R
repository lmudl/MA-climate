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
load("data/processed/hadcrut-processed.Rda")
load("data/processed/cru-processed.Rda")
drought %>% rename(drought = temp) -> drought
any(is.na(drought$drought))
t1 <- head(drought)
drought %>% group_by(time) %>% dplyr::summarise(drought_mean = mean(drought, na.rm = TRUE))
t2 <- head(temp)
drought %>% group_by(time) %>% 
  dplyr::summarise(drought_mean = mean(drought, na.rm = TRUE)) -> drought

base::merge(drought, temp, by = "time") -> data
library(MASS)
library(glmnet)
data %>% mutate(coord = str(coord)) 
head(data)
X <- data %>% dplyr::select(time, temp, coord)
Y <- data %>% dplyr::select(drought_mean)
fit_lasso <- glmnet(X, Y, family = "gaussian", alpha = 1)

# do one regression model for each coordinate?
etst <- model.matrix(~ time + temp + as.factor(coord), temp)
head(temp)

#mtcars example
data("mtcars")
#add cat toy variable
mtcars$cat <- factor(rep(letters[1:8], 4))
# factor(head(temp$coord))
X <- model.matrix(~. , dplyr::select(mtcars, -mpg))
fit <- glmnet(X, y = mtcars$mpg)
print(fit)
plot(fit)
coef(fit)
cvfit <- cv.glmnet(X, mtcars$mpg)
plot(cvfit)
cvfit$lambda.min

temp$coord <- factor(temp$coord)
X <- model.matrix(~., dplyr::select(temp, -temp))
