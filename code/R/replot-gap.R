# write function to replot 
# the gap statistics
library(ggplot2)
library(patchwork)

gap_example <- readRDS("results/clustering/gap_pc3_centered_kmeans_plot.rds")

get_tibsh_k_from_gapplot <- function(gapplot_data) {
  gk <- gapplot_data[,"gap"]
  l <- length(gk)
  gk <- gk[-l]
  gk1 <- gapplot_data[,"gap"][-1]
  sk1 <- gapplot_data[,"SE.sim"][-1]
  d <- gk1 - sk1
  tibsh_k <- which(gk >= d)[1]
  if (is.na(which(gk >= d)[1])) {
    return(l)
  }
  return(tibsh_k)
}
get_tibsh_k_from_gapplot(gap_example$data)
gap_stat_data <- gap_example$data
#stepbbystep
gap <- gap_stat_data[, "gap"]
se <- gap_stat_data[, "SE.sim"]
decr <- diff(gap) <= 0
k <-  get_tibsh_k_from_gapplot(gap_stat_data)
df <- as.data.frame(gap_stat_data, stringsAsFactors = TRUE)
df$clusters <- as.factor(1:nrow(df))
df$ymin <- gap - se
df$ymax <- gap + se
p <- ggpubr::ggline(df, x = "clusters", y = "gap", 
                    group = 1, color = "steelblue") + ggplot2::geom_errorbar(aes_string(ymin = "ymin", 
                                                                                      ymax = "ymax"), width = 0.2, color = "steelblue") + 
  geom_vline(xintercept = k, linetype = 2, color = "steelblue") + 
  labs(y = "Gap statistic (k)", x = "Number of clusters k", 
       title = "Optimal number of clusters")
p

replot_gap <- function (gap_stat_plot, linecolor = "steelblue") {
# {
#   if (!inherits(gap_stat, "clusGap")) 
#     stop("Only an object of class clusGap is allowed. (cluster package)")
#   if (is.list(maxSE)) {
#     if (is.null(maxSE$method)) 
#       maxSE$method = "firstmax"
#     if (is.null(maxSE$SE.factor)) 
#       maxSE$SE.factor = 1
#   }
#   else stop("The argument maxSE must be a list containing the parameters method and SE.factor")
  gap_stat_data <- gap_stat_plot$data
  gap <- gap_stat_data[, "gap"]
  se <- gap_stat_data[, "SE.sim"]
  decr <- diff(gap) <= 0
  k <-  get_tibsh_k_from_gapplot(gap_stat_data)
  df <- as.data.frame(gap_stat_data, stringsAsFactors = TRUE)
  df$clusters <- as.factor(1:nrow(df))
  df$ymin <- gap - se
  df$ymax <- gap + se
  p <- ggpubr::ggline(df, x = "clusters", y = "gap", 
                      group = 1, color = linecolor) + ggplot2::geom_errorbar(aes_string(ymin = "ymin", 
                                                                                        ymax = "ymax"), width = 0.2, color = linecolor) + 
    geom_vline(xintercept = k, linetype = 2, color = linecolor) + 
    labs(y = "Gap statistic (k)", x = "Number of clusters k", 
         title = gap_stat_plot$labels$title)
  p
}
replot_gap(gap_example)

# replot and save gap without pca ####
gap_kmeans_centered_plot <- readRDS("results/clustering/gap_kmeans_centered_plot.rds")
replotted_gap_kmeans_centered_plot <- replot_gap(gap_kmeans_centered_plot)
saveRDS(replotted_gap_kmeans_centered_plot, "results/clustering/replotted_gap_kmeans_centered_plot.rds")

gap_pam_centered_plot <- readRDS("results/clustering/gap_pam_centered_plot.rds")
replotted_gap_pam_centered_plot <- replot_gap(gap_pam_centered_plot)
saveRDS(replotted_gap_pam_centered_plot, "results/clustering/replotted_gap_pam_centered_plot.rds")

# replot and save gap with pca #####
gap_pc3_centered_kmeans_plot <- readRDS("results/clustering/gap_pc3_centered_kmeans_plot.rds")
replotted_gap_pc3_centered_kmeans_plot <- replot_gap(gap_pc3_centered_kmeans_plot)
saveRDS(replotted_gap_pc3_centered_kmeans_plot, 
         "results/clustering/replotted_gap_pc3_centered_kmeans_plot.rds")

gap_pc3_centered_pam_plot <- readRDS("results/clustering/gap_pc3_centered_pam_plot.rds")
replotted_gap_pc3_centered_pam_plot <- replot_gap(gap_pc3_centered_pam_plot)
saveRDS(replotted_gap_pc3_centered_pam_plot, 
        "results/clustering/replotted_gap_pc3_centered_pam_plot.rds")

gap_pc4_centered_kmeans_plot <- readRDS("results/clustering/gap_pc4_centered_kmeans_plot.rds")
replotted_gap_pc4_centered_kmeans_plot <- replot_gap(gap_pc4_centered_kmeans_plot)
saveRDS(replotted_gap_pc4_centered_kmeans_plot, 
        "results/clustering/replotted_gap_pc4_centered_kmeans_plot.rds")

gap_pc4_centered_pam_plot <- readRDS("results/clustering/gap_pc4_centered_pam_plot.rds")
replotted_gap_pc4_centered_pam_plot <- replot_gap(gap_pc4_centered_pam_plot)
saveRDS(replotted_gap_pc4_centered_pam_plot, 
        "results/clustering/replotted_gap_pc4_centered_pam_plot.rds")

(replotted_gap_pc3_centered_kmeans_plot +
  replotted_gap_pc3_centered_pam_plot) /
  (replotted_gap_pc4_centered_kmeans_plot +
  replotted_gap_pc4_centered_pam_plot) -> replotted_summary_plot_pca_centered
saveRDS(replotted_summary_plot_pca_centered, "results/clustering/replotted_summary_plot_pca_centered.rds")
