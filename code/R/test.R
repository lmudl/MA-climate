library(patchwork)

(gap_pc3_centered_kmeans_plot | gap_pc3_centered_pam_plot) / 
  (gap_pc4_centered_kmeans_plot | gap_pc4_centered_pam_plot) -> summary_plot_pca_centered

saveRDS(summary_plot_pca_centered, "results/clustering/summary_plot_pca_centered.rds")

(gap_pc2_uncentered_kmeans_plot | gap_pc2_uncentered_pam_plot) /
  (gap_pc3_uncentered_kmeans_plot | gap_pc3_uncentered_pam_plot) -> summary_plot_pca_uncentered

saveRDS(summary_plot_pca_uncentered, "results/clustering/summary_plot_pca_uncentered.rds")

