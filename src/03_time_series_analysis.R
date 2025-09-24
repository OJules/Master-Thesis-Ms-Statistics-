# ==============================================================================
# Chapter 3: Time Series Analysis and Forecasting
# ==============================================================================

#' Perform comprehensive time series analysis
#' @param data Time series data
#' @param series_name Name of the series for reporting
#' @return List containing analysis results
perform_time_series_analysis <- function(data, series_name = "Series") {
  
  # Convert to time series object
  ts_data <- ts(data, start = c(1990, 1), end = c(2020), frequency = 1)
  
  # 1. Stationarity testing
  adf_test <- adf.test(ts_data)
  cat(sprintf("ADF Test for %s: p-value = %.4f\n", series_name, adf_test$p.value))
  
  # 2. Differencing if necessary
  if(adf_test$p.value >= 0.05) {
    ts_diff <- diff(ts_data)
    adf_diff <- adf.test(ts_diff)
    cat(sprintf("ADF Test after differencing: p-value = %.4f\n", adf_diff$p.value))
    ts_stationary <- ts_diff
    differencing_order <- 1
  } else {
    ts_stationary <- ts_data
    differencing_order <- 0
  }
  
  # 3. ARIMA modeling
  arima_model <- auto.arima(ts_stationary)
  
  # 4. Forecasting
  forecast_result <- forecast(arima_model, h = 5)
  
  # 5. Linear trend model
  years <- 1990:2020
  trend_model <- lm(as.numeric(ts_data) ~ years)
  
  # 6. Model diagnostics
  diagnostics <- list(
    adf_original = adf_test,
    adf_differenced = if(differencing_order > 0) adf_diff else NULL,
    arima_model = arima_model,
    trend_model = trend_model,
    forecast = forecast_result,
    residuals = residuals(arima_model)
  )
  
  return(diagnostics)
}

#' Analyze liver cirrhosis mortality trends
analyze_cirrhosis_trends <- function() {
  
  # Load cirrhosis mortality data
  cirrhosis_file <- "data/raw/Taux_mortalite_cirrhose_foie.xlsx"
  
  if(!file.exists(cirrhosis_file)) {
    warning("Cirrhosis mortality data file not found")
    return(NULL)
  }
  
  cirrhosis_data <- read_excel(cirrhosis_file)
  
  # Separate by gender
  data_male <- subset(cirrhosis_data, Sexe == "M")
  data_female <- subset(cirrhosis_data, Sexe == "F")
  
  # Analyze overall trends
  overall_analysis <- perform_time_series_analysis(
    cirrhosis_data$Cirrhose_du_foie_alcoolique, 
    "Overall Cirrhosis Mortality"
  )
  
  # Gender-specific analysis
  male_analysis <- perform_time_series_analysis(
    data_male$Cirrhose_du_foie_alcoolique,
    "Male Cirrhosis Mortality"
  )
  
  female_analysis <- perform_time_series_analysis(
    data_female$Cirrhose_du_foie_alcoolique, 
    "Female Cirrhosis Mortality"
  )
  
  # Save results
  results <- list(
    overall = overall_analysis,
    male = male_analysis,
    female = female_analysis
  )
  
  saveRDS(results, "results/models/cirrhosis_time_series_analysis.rds")
  
  return(results)
}

cat("Time series analysis functions loaded successfully!\n")
