# Predicting Droughts in the Amazon Basin based on Global Sea Surface Temperatures

This is the repository for my master thesis written at LMU Munich.
It was supervised by Dr. Fabian Scheipl (LMU) and Dr. Niklas Boers (PIK)
It was submitted on August 9th, 2022.

Here lives the code and information to reproduce the findings of my thesis.

## Master thesis overview

The master thesis explored the SST and precipitation data in
an explorative, cluster and regression analysis.
The regression models used were lasso and fused lasso.

## /cygwin/cygwin fast.txt

Notes for the data preprocessing done via Climate Data Operators (CDO).
Relevant here is only the preprocessing of "Chirps" and "ERSST"

## /code/R

contains all of the code that was used to create the explorative,
cluster and regression results.

*helper-functions.R*, *helper-functions-parallel-cv*, *glyph-helpers*,
*glyph-helpers2.R*:
Contains the helpers that are used in the other scripts.

*prepare-data-cv*:
Takes as input the data after applying the cygwin preprocessing.
Preprocesses data and creates datasets used for cross-validation (or forward-validation in
our case) and evaluation.

*create-plots-sst-eda* and *create-plots-precip-eda*:
Here we create the explorative analysis plots of the SST and precipitation
data.

*glyph-plotting*:
Create the precipitation glyph plots

*deseasonalise*:
De-seasonalize the SST data before creating the correlation plots
on the de-sesaonalized data.

*cluster-precip*:
General script to inspect the influence of different settings
on outcome of gap statistic.

*cluster-kmeans*, *cluster-pam*:
Clustering the precipitation data with k-means and pam (k-medios),
respectively.

*cluster-pca-kmeans*, *cluster-pca-pam*:
Same as above but applying a PCA before.

*replot-gap*:
Modify gap statistic plots created in the cluster analysis.

*corr-analysis-original-data*, *corr-analysis-deseasonalised-data*:
Create the correlation plots for original and de-seasonalized data

*preprocess-data*:
CDO used with the R wrapper use either this or cygwin

### code/R/lasso and code/R/fused-lasso
In these folders are the codefiles for the lasso and fused-lasso
models that were analyzed in the master thesis (plus some additional models)

## thesis
Contains the markdown files to render the Master thesis

