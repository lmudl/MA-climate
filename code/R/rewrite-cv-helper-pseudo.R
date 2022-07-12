# experimental new helper function cv

# index_list in external function

# if model lasso

# get lambda value
# create empty error matrix

# for i in nfold
  # split cv in training and test
  # preprocessing
    # add time_vars
    # add differenciation
    # deseasonalise features
    # standardize features
    # standardize response
  # fit model
  # do predictions
    # eventually scale back
  # now we could do the plots
    # plot error line for the predictions
      # need: only error vector
    # plot error line for the fitted values
      # need: fitted values, true values
    # plot coefficients
      # need: model coefficients and coordinate names.
# plot errbar/ errlines 
  # need all errors so do after all folds have been computed


# if thats too much work we could just also save the predictions
# in future models 

# most importanly make preprocessing clean and easy now
# so that is easily reusable for the cv and the plotting
# make the function for lasso and for fused the almost the same

# what is difference between fused and lasso?
# input output etc
# then we can make them the same mas o menos hopefully
