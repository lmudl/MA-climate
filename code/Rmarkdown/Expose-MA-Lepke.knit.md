---
title: "Master Thesis Overview"
author: "Dario Lepke"
date: "29.04.21"
output:
  bookdown::html_document2: default
  pdf_document: Null
bibliography: expose-lib.bib
biblio-style: apalike
link-citations: yes
---
## Introduction {-}

With future climate change droughts in the Amazon forest may become more
frequent and/or severe. Droughts can turn Amazon regions from rain forest into
savanna, leading to high amounts of carbon released into the atmosphere.
Therefore, predicting future droughts and understanding the underlying mechanisms 
is of great interest. @ciemer2020early, established an 
early warning indicator for droughts in the central Amazon basin (CAB), based on tropical Atlantic sea surface 
temperatures (SSTs). In my thesis I would like to build on this work and improve the 
predictive power by using different statistical methods.
Meaning, we seek to build a model that is able to predict droughts (resp. rainfall) based on preceding
sea temperatures, desirably with as much lead time as possible. Also we want to 
identify those sea regions that are most important for doing so, making interpretability a point of interest, too. A first model could be a cross-validated (generalised) LASSO approach
trying to identify the most important oceanic regions and the respective time-scales.

The thesis will be done in cooperation with Dr. Niklas Boers from the Postdam Institute for Climate Impact Research (@PIC).

## Summary @ciemer2020early {-}

As already mentioned the paper by @ciemer2020early, created an early warning
indicator for Amazon droughts. They did so using a complex network approach.
They used two datasets, one for the Sea Surface Temperatures (@smith2008improvements) and one for
the precipitation (@funk2015climate) with monthly data for the time period of 1981 until 2016. The data can be downloaded for example in netcdf format and manipulated conventiently with Climate Data Operators (CDO, @schulzweida2019cdo). CDO in turn can be used with wrappers for R and Python. The data is organized on a longitude/latitude grid.  
They identify 4 oceanic regions that correlate the most with rain in the amazon basin, using a coupled network approach.
Figure \@ref(fig:cross) shows the cross degree towards rainfall in the central Amazon basin (CAB, blue box), for positive and negative correlations. Darker shades indicate a larger cross degree, hence a larger number of links and correlations with rainfall at more grid points in the CBA.  
The correlations are measured using a spearman rank-order correlation coefficient\
\[
 \rho = 1 - \frac{6\sum\Delta_{R_i}^2}{n(n^2-1)}  .
\]
Where $\Delta_{R_i}$ denotes the difference between the ranks of observations of both variables at the same time $i$ and $n$ is the number of observations.
An Adjacency Matrix describes the resulting network, where the threshold $p_{th}$ was chosen so that only 10% of the strongest correlations are represented as links in the network\
\[
A_{ij} = \begin{cases}0 \textrm{ } p_{ij} < p_{th}\\ 1 \textrm{ } p_{ij}\geq p_{th}\\\end{cases}  .
\]
The cross degree then gives the strength of correlation between a specific grid point $i$ of network $V_l$ (oceanic grid point) and another (sub)network $V_m$ (all grid points $j$ in central amazon basin)\
\[
k_{i}^{lm} = \sum\limits_{j \in V_m} A_{ij}, i \in V_l\  .
\]

<div class="figure" style="text-align: center">
<img src="../../figures/cross-degree.jpg" alt="&quot;Cross degree between sea surface temperature and continental rainfall anomalies. For each sea surface temperature grid cell of the Atlantic and Pacific Ocean, the cross degree towards rainfall in the Central Amazon Basin (blue box) is shown, for a positive correlations and b negative correlations. Darker shading indicates a larger cross degree, implying a larger number of links, and thus significant correlations with rainfall at more grid points in the Central Amazon Basin. Red areas outline coherent oceanic regions with a the 20% highest cross degrees for positive correlations, found in the Southern Pacific Ocean (SPO) and Southern Tropical Atlantic Ocean (STAO), and b the 20% highest cross degrees for negative correlations, found in the Central Pacific Ocean (CPO) and Northern Tropical Atlantic Ocean (NTAO)&quot; [@ciemer2020early]" width="50%" />
<p class="caption">(\#fig:cross)"Cross degree between sea surface temperature and continental rainfall anomalies. For each sea surface temperature grid cell of the Atlantic and Pacific Ocean, the cross degree towards rainfall in the Central Amazon Basin (blue box) is shown, for a positive correlations and b negative correlations. Darker shading indicates a larger cross degree, implying a larger number of links, and thus significant correlations with rainfall at more grid points in the Central Amazon Basin. Red areas outline coherent oceanic regions with a the 20% highest cross degrees for positive correlations, found in the Southern Pacific Ocean (SPO) and Southern Tropical Atlantic Ocean (STAO), and b the 20% highest cross degrees for negative correlations, found in the Central Pacific Ocean (CPO) and Northern Tropical Atlantic Ocean (NTAO)" [@ciemer2020early]</p>
</div>

They further explore the relationship by constructing (weighted) networks for sliding windows of 24 months between the Central Amazon Basin and each of the ocean regions. For each month except for the first two years, an individual network is computed based on the data of the previous 24 months.
Then they take the average of the cross correlations for each of the networks which gives a new time series of average cross correlation (ACC) values.
Each ACC summarizes the connectivity of one region with the CAB for the last 24 months.  
They find that NTAO and STAO give the strongest signal, hence they apply the same sliding window coupled network approach between the ocean regions NTAO and STAO.
Before they computed networks between ocean and continental regions, now it is computed between these two atlantic regions, NTAO and STAO.  
The resulting time series and its comparison to the drought index time series is shown in figure \@ref(fig:early) below.
They find that using a ACC threshold for a drought (SPI below -1.5), lets them
forecast 6 of the 7 droughts in the observation period, missing the 2005 drought, while also giving one false alarm in 2002. 

<div class="figure" style="text-align: center">
<img src="../../figures/early-warning-signals.jpg" alt="&quot;Early-warning signal for droughts in the central Amazon basin. We compare the time evolution of the average cross correlation of the Northern Tropical Atlantic Ocean (NTAO) and Southern Tropical Atlantic Ocean (STAO), given by the blue curve, with the standardized precipitation index (SPI, orange) of the central Amazon basin. Orange dips indicate a negative SPI with a threshold for severely dry periods (SPI -1, dotted red line). We expect a drought event within the following one and a half years whenever the average cross correlation between NTAO and STAO SST anomalies falls below an empirically found threshold of -0.06. Green circles indicate a matching forecast based on the Atlantic SST correlation structure, with one false alarm in 2002 indicated by a grey circle, where the threshold is crossed but no drought took place in the direct aftermath (see Discussion). The temporal evolution of the average cross correlation shown here is smoothed using a Chebyshev type-I low-pass filter with a cutoff at 24 months&quot; [@ciemer2020early]." width="75%" />
<p class="caption">(\#fig:early)"Early-warning signal for droughts in the central Amazon basin. We compare the time evolution of the average cross correlation of the Northern Tropical Atlantic Ocean (NTAO) and Southern Tropical Atlantic Ocean (STAO), given by the blue curve, with the standardized precipitation index (SPI, orange) of the central Amazon basin. Orange dips indicate a negative SPI with a threshold for severely dry periods (SPI -1, dotted red line). We expect a drought event within the following one and a half years whenever the average cross correlation between NTAO and STAO SST anomalies falls below an empirically found threshold of -0.06. Green circles indicate a matching forecast based on the Atlantic SST correlation structure, with one false alarm in 2002 indicated by a grey circle, where the threshold is crossed but no drought took place in the direct aftermath (see Discussion). The temporal evolution of the average cross correlation shown here is smoothed using a Chebyshev type-I low-pass filter with a cutoff at 24 months" [@ciemer2020early].</p>
</div>

## Outlook {-}
The work by @ciemer2020early shows potential forecasting capabilities and limitations.
While they are able to predict 5 out of 6 drought events, they also give one false negative and one false positive result. Their work uses a complex network approach that is applied stepwise (first two unweighted networks, then two weighted networks 
and in the end a dichotomous threshold decision rule). For the thesis we would like
to create a more general predictive model that can learn the relationship between the
SSTs and rainfall in the CAB. As already mentioned a first step can be a LASSO model. First findings show that, the classic LASSO only chooses single points in the ocean as predictors, though. But our motivation is to discover predictive regions and not only single seperated points.
Therefore in a next step we want to make use of a generalised form of the LASSO that also takes into account that chosen predictors should be close to each other. This model is the so called Fused LASSO.  
For the models we also need a form of evaluation. Classic Cross Validation assumes independence of the observations. In our setting this is clearly violated due to the time dependency of the data.
We will explore different possibilities to use an adjusted form of Cross Validation that takes this characteristic into account.  
Depending on how well the relationship between SST and rain can be established, we can take this a step further and use it as a so called "Emergent Constraint" (EC).
Since different climate models give different answers about future climate 
there is a need to narrow this "spread", which can be done by ECs.
To do so, we need a plausible relationship between a Variable X and Y (here: SST and drought).  
According to how well the relationship is represented in a climate model we 
assign "credibility" to a climate model's future projections 
(here: projections of future droughts in the Amazon rain forest). 
In summary this can be used to reduce uncertainty in the ensemble of 
climate models' future projections, f.e by using ML techniques as done by
@schlund2020constraining.

# References