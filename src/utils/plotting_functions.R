# ==============================================================================
# Utility Functions for Statistical Plotting
# ==============================================================================

#' Create publication-ready theme for ggplot2
theme_thesis <- function() {
  theme_classic() +
    theme(
      axis.title.y = element_text(margin = margin(t=0, r=15, b=0, l=0)),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      legend.text.align = 1,
      legend.position = "bottom",
      text = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.text = element_text(size = 10)
    )
}

#' Create correlation heatmap
plot_correlation_heatmap <- function(correlation_matrix, title = "Correlation Analysis") {
  
  # Convert to long format
  cor_data <- expand.grid(X = rownames(correlation_matrix), 
                         Y = colnames(correlation_matrix))
  cor_data$Correlation <- as.vector(correlation_matrix)
  
  # Create heatmap
  ggplot(cor_data, aes(X, Y, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                        midpoint = 0, limit = c(-1, 1), space = "Lab",
                        name = "Pearson\nCorrelation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(title = title, x = "", y = "") +
    coord_fixed()
}

cat("Plotting utility functions loaded successfully!\n")
