# helper functions for data preprocessing
# in fold
# what makes more sense first deseasonalising
# then scale?
# then difference?

if(standardize_features = TRUE) {
  mean_x_train <- apply(x_train,2, mean)
  sdn_x_train <- apply(x_train,2,sdN)
  x_train <- scale(x_train, center=mean_x_train,
                   scale=sdn_x_train)
  x_test <- scale(x_test, center=mean_x_train,
                  scale=sdn_x_train)
}

if(standardize_response = TRUE) {
  mean_y_train <- mean(y_train)
  sdn_y_train <- sdN(y_train)
  y_train <- scale(y_train, center=mean_y_train, 
                   scale=sdn_y_train)
  # y_test <- scale(y_test, center=mean_y_train, 
  #                 scale=sdn_y_train)
}

# also make sure to back transform targets
# and coefficients

# for scaling back coefficients
# when can use R1magic
# scale.back.lm
# for y 
# if y.s=y-ymean/sd(y)
# y.s*sd(y) +ymean = y

preds <- ...
if(standardize_response=TRUE) {
  preds <- preds*sdn_y_train + mean_y_train
}

# y_test-mean_y_train/sdn_y_train=y_test.s
# y_test.s-preds

# y_test-mean_y_train/sdn_y_train-preds=diff
# y_test-preds*sdn_y_train+mean_y_train





