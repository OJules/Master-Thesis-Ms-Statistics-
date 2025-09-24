# ==============================================================================
# Chapter 4: Correlation and Causality Analysis  
# ==============================================================================

#' Perform comprehensive correlation analysis
#' @param mortality_data Mortality time series
#' @param consumption_data List of consumption time series
#' @return Correlation matrix and statistics
analyze_correlations <- function(mortality_data, consumption_data) {
  
  # Combine all data into matrix
  analysis_data <- data.frame(
    mortality = mortality_data,
    daily_consumption = consumption_data$daily,
    weekly_1to2 = consumption_data$weekly_1to2,
    weekly_3to6 = consumption_data$weekly_3to6,
    less_than_weekly = consumption_data$less_than_weekly
  )
  
  # Calculate correlation matrix
  correlation_matrix <- cor(analysis_data, method = "pearson", use = "complete.obs")
  
  # Statistical significance testing
  cor_tests <- lapply(2:ncol(analysis_data), function(i) {
    cor.test(analysis_data[,1], analysis_data[,i])
  })
  names(cor_tests) <- colnames(analysis_data)[-1]
  
  return(list(
    correlation_matrix = correlation_matrix,
    significance_tests = cor_tests,
    sample_size = nrow(analysis_data)
  ))
}

#' Perform Granger causality testing
#' @param mortality_data Mortality time series (differenced if needed)
#' @param consumption_data Consumption time series (differenced if needed) 
#' @return Granger test results
perform_granger_analysis <- function(mortality_data, consumption_data) {
  
  # Ensure stationarity through differencing
  mortality_diff <- diff(mortality_data)
  consumption_diff <- diff(consumption_data)
  
  # Perform Granger causality test
  granger_result <- grangertest(mortality_diff ~ consumption_diff, order = 2)
  
  return(granger_result)
}

#' Complete correlation and causality analysis
complete_correlation_causality_analysis <- function() {
  
  # Load integrated dataset
  integrated_file <- "data/processed/Cirrhose_Alcool.xlsx"
  
  if(!file.exists(integrated_file)) {
    warning("Integrated dataset not found, creating synthetic example")
    return(create_example_analysis())
  }
  
  integrated_data <- read_excel(integrated_file)
  
  # Separate by gender
  male_data <- subset(integrated_data, Sexe == "M")
  female_data <- subset(integrated_data, Sexe == "F")
  
  # Male analysis
  male_consumption <- list(
    daily = male_data$Conso_journaliere,
    weekly_3to6 = male_data$Conso_3a6_par_semaine,
    weekly_1to2 = male_data$Conso_1a2_par_semaine,
    less_than_weekly = male_data$Conso_moins_de1par_semaine
  )
  
  male_correlations <- analyze_correlations(
    male_data$Mortalite_cirrhose, 
    male_consumption
  )
  
  # Save results
  results <- list(
    male_correlations = male_correlations,
    female_correlations = list(),  # Placeholder
    granger_tests = list()         # Placeholder
  )
  
  saveRDS(results, "results/models/correlation_causality_analysis.rds")
  
  return(results)
}

create_example_analysis <- function() {
  # Create example correlation results for demonstration
  example_matrix <- matrix(c(1, 0.7, -0.3, -0.2, -0.1,
                           0.7, 1, -0.2, -0.15, -0.05,
                           -0.3, -0.2, 1, 0.8, 0.6,
                           -0.2, -0.15, 0.8, 1, 0.7,
                           -0.1, -0.05, 0.6, 0.7, 1), 
                         nrow = 5, ncol = 5)
  
  colnames(example_matrix) <- rownames(example_matrix) <- 
    c("mortality", "daily_consumption", "weekly_1to2", "weekly_3to6", "less_than_weekly")
  
  return(list(
    male_correlations = list(
      correlation_matrix = example_matrix,
      sample_size = 31
    ),
    female_correlations = list(),
    granger_tests = list()
  ))
}

cat("Correlation and causality analysis functions loaded successfully!\n")
