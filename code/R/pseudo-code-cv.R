# how do I plot again?
# at the moment
# plot_nonzero from fold
# I have to refit the models
# But it would make more sense to directly 
# plot the folds when I do the CV
# so we fit the model 
# have trained_model
# predicted etc
# IMPORTANT!: for time lags etc plotting becomes more difficult
# so maybe:
  # decide not to plot but save the model
  # then get nonzero coefficients after loading the model?
  # only save nonzero coefficients?
  # add a plot flag with TRUE or FALSE
  # add a save models with TRUE or FALSE
# in each fold we have to:
# get nonzer coefficients, check if it was fitted with or without intercept
# get the names of the coefficients
# create coef_mat
# plot_nonzero_coefficients

# we could also directly create the fold prediction plots
# 

# so add save model, plot coeff, plot predictions

# maybe saving the model is already fine because afterwards
# we can run a script load the model
# plot coefficients
# load test data, predict on test data for that fold


# at end of fold we could also fit full model and save the full model
# probably the easiest way
# because afterwards I can still tweak plotting functions etc 
# without having to run the whole model again.

# so I would then always have 
# run-cv-model etc
# analyse-cv-model etc.

# I could afterwards plot learning inside fold
# and learning outside the plot

# Check how fast is prediction in fused lasso
# eventually we have to save predictions in fused lasso


# could rewrite

# create id list here already but will get more complicated because we also check
# and maybe timelags etc.
ids

# NOTE: we use get_obs_to_drop and drop_obs so that the first observations
# are discarded and not the last
# but we use it so that we dont have to drop any of them rn since 370 etc
cv_for_ts <- function(sst, precip, nfold, size_train, size_test, save_folder,
                      model, graph, maxsteps) {
  # the arguments are the same + the ones for old cv_for_ts
  # default is lasso so its backward compatable,
  # still can use old scripts hopefully
  # because model will be lasso and there will be no check
  # of graph and maxsteps
  
  # variable checks
  
  # prepare sst can be done outside of cv_for_ts I think
  
  # obs to drop
  
  # assert equal sizes
  
  # id list
  
  # now check if lasso or fused
  # if lasso then cv_lasso
  #   create directory for saving the whole CV
  #   create directory for saving the fold models
  # start loop over folds
  #   ids train and test
  #   split train and test
  #   fit model
  #   make predictions on test
  #   compute mse
  #   save err_col
  #   
  
  # if fused then cv_fused
  
}
# DONE
# save lambda in reverse order in function already
# dont have to reverse afterwards anymore

# DONE
# err mat
# check how to add rownames in fused lasso case
# NOTE: I can get the lambda values or knots 
# f1$beta, could add that, but each lambda is different in
# each fold (as of now) so maybe use that afterwards when plotting
# fusedlasso


# DONE
# create error_mat inside cv_lasso call

# DONE, but only for lasso
# save the lamba vector in the error matrix as rownames?

# DONE, saved after fitting the folds
# save lambda vec inside cv_lasso

# DONE 
# moveout dir.create before starting CV

# DONE DIFFERENCES in lasso and fused
# lasso compute lambda vector 
# lasso computes err matrix on lambda vector length
# fused gets lambda internally, gets error matrix on maxsteps

# DONE important have to realise that in plots maybe lambdas are not reversed?

# TODO PLOTS
# add lambda in plot on x-axis
# NOTE: plot_all_err will not work for fused
# try: x = rownames(err_matrix)

# TODO scripts in the end
# fit cv
# analyse cv
# and there also fit full/ validate cv?

# TODO create test-cv runs for making sure that the functions
# are working
$
# TODO
# change name cv_for_ts_up to cv_for_ts in fused lasso scripts

# TODO PLOTS
# for plot_nonzero_from_fold we can just load the model
# get the nonzero coefficients
# from the names we get the coordinates
# together we can plot them easily wit plot_nonzero_coefficinets






# TESTING
l <- list(letters[1:5])
m <- matrix(1:10, nrow = 5, dimnames =  list(letters[1:5]))
?matrix

















