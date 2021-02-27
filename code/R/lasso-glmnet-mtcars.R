library("glmnet")
#mtcars example
data("mtcars")
#add cat toy variable
mtcars$cat <- factor(rep(letters[1:8], 4))
# factor(head(temp$coord))
X_cars <- model.matrix(~. , dplyr::select(mtcars, -mpg))
fit <- glmnet(X, y = mtcars$mpg)
print(fit)
plot(fit)
nonzero <- coef(fit)[,1]!=0
rownames(coef(fit))[nonzero]
?coef()
cvfit <- cv.glmnet(X_cars, mtcars$mpg)
plot(cvfit)
cvfit$lambda.min

fit <- glmnet(X_cars, y = mtcars$mpg, lambda = cvfit$lambda.1se)
coef(fit)

fit = glmnet(as.matrix(mtcars[-1]), mtcars[,1], 
             lambda=cv.glmnet(as.matrix(mtcars[-1]), mtcars[,1])$lambda.1se)
coef(fit)

plot(predict.glmnet(fit, X_cars, s = "lambda.min"))
cbind(mtcars$mpg,(predict.glmnet(fit, X_cars, s = "lambda.min")))

