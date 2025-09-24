# ==============================================================================
# Master's Thesis: Statistical Efficacy Study of Prevention Campaigns in Switzerland
# Author: Jules Odje
# Institution: Université de Neuchâtel & LIVES Institute (UNIL)
# Supervisor: Prof. André Berchtold
# 
# MAIN ANALYSIS SCRIPT - Orchestrates the complete statistical analysis
# ==============================================================================

# Clear workspace and set options
rm(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)  # Disable scientific notation

# Set working directory (adjust as needed)
# setwd("/path/to/your/thesis/directory")

cat("=================================================================\n")
cat("Swiss Prevention Campaigns Statistical Efficacy Study\n") 
cat("Master's Thesis Analysis Pipeline\n")
cat("Author: Jules Odje | Université de Neuchâtel\n")
cat("=================================================================\n\n")

# Create directory structure if it doesn't exist
dir.create("results", showWarnings = FALSE)
dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("results/models", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# Record start time
start_time <- Sys.time()

# ==============================================================================
# STEP 1: DATA LOADING AND PREPROCESSING
# ==============================================================================
cat("STEP 1: Loading and preprocessing data...\n")
source("src/01_data_loading_preprocessing.R")

# Load all prevention campaign datasets
prevention_data <- load_prevention_data()
cat("✓ Prevention campaign data loaded successfully\n")

# ==============================================================================  
# STEP 2: EXPLORATORY DATA ANALYSIS
# ==============================================================================
cat("\nSTEP 2: Performing exploratory data analysis...\n")
source("src/02_exploratory_analysis.R")

# Generate tobacco consumption plots
if(!is.null(prevention_data$OFSP) && !is.null(prevention_data$Monitorage)) {
  tobacco_plot <- create_tobacco_evolution_plot(prevention_data)
  ggsave("results/figures/tobacco_consumption_by_gender.png", 
         tobacco_plot, width = 14, height = 10, dpi = 300)
  cat("✓ Tobacco consumption analysis completed\n")
}

# ==============================================================================
# STEP 3: TIME SERIES ANALYSIS AND FORECASTING  
# ==============================================================================
cat("\nSTEP 3: Time series analysis and mortality trends...\n")
source("src/03_time_series_analysis.R")

# Analyze liver cirrhosis mortality trends
cirrhosis_analysis <- analyze_cirrhosis_trends()
cat("✓ Cirrhosis mortality time series analysis completed\n")

# ==============================================================================
# STEP 4: CORRELATION AND CAUSALITY ANALYSIS
# ==============================================================================
cat("\nSTEP 4: Correlation and Granger causality analysis...\n") 
source("src/04_correlation_causality_analysis.R")

# Comprehensive correlation and causality analysis
correlation_results <- complete_correlation_causality_analysis()
cat("✓ Correlation analysis completed\n")

# ==============================================================================
# ANALYSIS COMPLETION AND SUMMARY
# ==============================================================================

# Calculate total execution time
end_time <- Sys.time()
execution_time <- round(difftime(end_time, start_time, units = "mins"), 2)

cat("\n=================================================================\n")
cat("ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("=================================================================\n")
cat(sprintf("Total execution time: %s minutes\n", execution_time))
cat("Supervisor: Prof. André Berchtold (LIVES Institute, UNIL)\n")
cat("\nGenerated outputs:\n")
cat("• Figures: results/figures/\n") 
cat("• Statistical models: results/models/\n")
cat("• Summary tables: results/tables/\n")
cat("=================================================================\n")
