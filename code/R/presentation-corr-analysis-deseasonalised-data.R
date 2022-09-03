# corr analysis of decomposed data
setwd("Repos/MA-climate/")
source("code/R/helper-functions.R")

library(raster)
library(ggplot2)

# Load original sst data for reference when creating raster
sst_og <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")

# Load deseasonalised data
m_sst_des <- readRDS("data/processed/deseasonalised_sst.rds")
m_precip_des <- readRDS("data/processed/deseasonalised_precip.rds")

# Compute correlations between sst data and precipitation means and plot it
# Timelag 0
# Compute Correlations
corr0_des <- compute_corr(m_sst_des, m_precip_des, timelag = 0)
plot_dens0_des <- plot_density(corr0_des,0)
ggsave("pres-plots/des-data/dens-0.jpg", plot_dens0_des)

plot_corr0_des <- plot_corr(corr_vec = corr0_des,timelag = 0,
                            old_sst = sst_og,quantiles = FALSE)
ggsave("pres-plots/des-data/corr-0.jpg", plot_corr0_des)

q_plot_corr0_des <- plot_corr(corr_vec = corr0_des,timelag = 0,
                              old_sst = sst_og,quantiles = TRUE)
ggsave("pres-plots/des-data/q-corr-0.jpg", q_plot_corr0_des)

# Timelag 1
# Compute Correlations
corr1_des <- compute_corr(m_sst_des, m_precip_des, timelag = 1)
plot_dens1_des <- plot_density(corr1_des,1)
ggsave("pres-plots/des-data/dens-1.jpg", plot_dens1_des)

plot_corr1_des <- plot_corr(corr_vec = corr1_des,timelag = 1,
                            old_sst = sst_og,quantiles = FALSE)
ggsave("pres-plots/des-data/corr-1.jpg", plot_corr1_des)

q_plot_corr1_des <- plot_corr(corr_vec = corr1_des,timelag = 1,
                              old_sst = sst_og,quantiles = TRUE)
ggsave("pres-plots/des-data/q-corr-1.jpg", q_plot_corr1_des)


# Timelag 3
corr3_des <- compute_corr(m_sst_des, m_precip_des, timelag = 3)
plot_dens3_des <- plot_density(corr3_des,3)
ggsave("pres-plots/des-data/dens-3.jpg", plot_dens3_des)

plot_corr3_des <- plot_corr(corr_vec = corr3_des,timelag = 3,
                            old_sst = sst_og,quantiles = FALSE)
ggsave("pres-plots/des-data/corr-3.jpg", plot_corr3_des)

q_plot_corr3_des <- plot_corr(corr_vec = corr3_des,timelag = 3,
                              old_sst = sst_og,quantiles = TRUE)
ggsave("pres-plots/des-data/q-corr-3.jpg", q_plot_corr3_des)


# Timelag 6
corr6_des <- compute_corr(m_sst_des, m_precip_des, timelag = 6)
plot_dens6_des <- plot_density(corr6_des,6)
ggsave("pres-plots/des-data/dens-6.jpg", plot_dens6_des)

plot_corr6_des <- plot_corr(corr_vec = corr6_des,timelag = 6,
                            old_sst = sst_og,quantiles = FALSE)
ggsave("pres-plots/des-data/corr-6.jpg", plot_corr6_des)

q_plot_corr6_des <- plot_corr(corr_vec = corr6_des,timelag = 6,
                              old_sst = sst_og,quantiles = TRUE)
ggsave("pres-plots/des-data/q-corr-6.jpg", q_plot_corr6_des)

# Timelag 12
corr12_des <- compute_corr(m_sst_des, m_precip_des, timelag = 12)
plot_dens12_des <- plot_density(corr12_des,12)
ggsave("pres-plots/des-data/dens-12.jpg", plot_dens12_des)

plot_corr12_des <- plot_corr(corr_vec = corr12_des,timelag = 12,
                             old_sst = sst_og,quantiles = FALSE)
ggsave("pres-plots/des-data/corr-12.jpg", plot_corr12_des)

q_plot_corr12_des <- plot_corr(corr_vec = corr12_des,timelag = 12,
                               old_sst = sst_og,quantiles = TRUE)
ggsave("pres-plots/des-data/q-corr-12.jpg", q_plot_corr12_des)



