getwd()
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")

library(raster)
library(ggplot2)

# Load original data
sst_og <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
precip_og <- brick("data/interim/drought/chirps_setreftime.nc", varname = "precip")

# Transform to matrix
m_sst_og <- brick_to_matrix_wna(sst_og, nlayers = nlayers(sst_og))
m_precip_og <- brick_to_matrix_wna(precip_og, nlayers = nlayers(precip_og))
rm(precip_og)

# Compute correlations between sst data and precipitation means and plot it
# Timelag 0
# Compute Correlations
corr0_og <- compute_corr(m_sst_og, m_precip_og, timelag = 0)
plot_dens0_og <- plot_density(corr0_og,0)
ggsave("plots/og-data/dens-0.jpg", plot_dens0_og)

plot_corr0_og <- plot_corr(corr_vec = corr0_og,timelag = 0,
                           old_sst = sst_og,quantiles = FALSE)
ggsave("plots/og-data/corr-0.jpg", plot_corr0_og)

q_plot_corr0_og <- plot_corr(corr_vec = corr0_og,timelag = 0,
                             old_sst = sst_og,quantiles = TRUE)
ggsave("plots/og-data/q-corr-0.jpg", q_plot_corr0_og)

# Timelag 3
corr3_og <- compute_corr(m_sst_og, m_precip_og, timelag = 3)
plot_dens3_og <- plot_density(corr3_og,3)
ggsave("plots/og-data/dens-3.jpg", plot_dens3_og)

plot_corr3_og <- plot_corr(corr_vec = corr3_og,timelag = 3,
                           old_sst = sst_og,quantiles = FALSE)
ggsave("plots/og-data/corr-3.jpg", plot_corr3_og)

q_plot_corr3_og <- plot_corr(corr_vec = corr3_og,timelag = 3,
                             old_sst = sst_og,quantiles = TRUE)
ggsave("plots/og-data/q-corr-3.jpg", q_plot_corr3_og)


# Timelag 6
corr6_og <- compute_corr(m_sst_og, m_precip_og, timelag = 6)
plot_dens6_og <- plot_density(corr6_og,6)
ggsave("plots/og-data/dens-6.jpg", plot_dens6_og)

plot_corr6_og <- plot_corr(corr_vec = corr6_og,timelag = 6,
                           old_sst = sst_og,quantiles = FALSE)
ggsave("plots/og-data/corr-6.jpg", plot_corr6_og)

q_plot_corr6_og <- plot_corr(corr_vec = corr6_og,timelag = 6,
                             old_sst = sst_og,quantiles = TRUE)
ggsave("plots/og-data/q-corr-6.jpg", q_plot_corr6_og)

# Timelag 12
corr12_og <- compute_corr(m_sst_og, m_precip_og, timelag = 12)
plot_dens12_og <- plot_density(corr12_og,12)
ggsave("plots/og-data/dens-12.jpg", plot_dens12_og)

plot_corr12_og <- plot_corr(corr_vec = corr12_og,timelag = 12,
                           old_sst = sst_og,quantiles = FALSE)
ggsave("plots/og-data/corr-12.jpg", plot_corr12_og)

q_plot_corr12_og <- plot_corr(corr_vec = corr12_og,timelag = 12,
                             old_sst = sst_og,quantiles = TRUE)
ggsave("plots/og-data/q-corr-12.jpg", q_plot_corr12_og)
