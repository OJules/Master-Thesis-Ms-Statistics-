# ==============================================================================
# Master's Thesis: Statistical Efficacy Study of Prevention Campaigns
# Author: Jules Odje
# Chapter 1: Data Loading and Preprocessing
# ==============================================================================

# Load required libraries
library(ggplot2)
library(readxl)
library(forecast)
library(tseries)
library(zoo)
library(boot)
library(lmtest)
library(dplyr)
library(openxlsx)

#' Load all prevention campaign datasets
#' @return list of data frames
load_prevention_data <- function() {
  data_files <- list(
    OMS = "data/raw/OMS_data.xlsx",
    OFSP = "data/raw/OFSP_data.xlsx", 
    Panel = "data/raw/Panel_data.xlsx",
    neutre = "data/raw/neutre.xlsx",
    Monitorage = "data/raw/Monitorage_data.xlsx"
  )
  
  # Load all datasets
  datasets <- lapply(data_files, function(file) {
    if(file.exists(file)) {
      read_excel(file)
    } else {
      warning(paste("File not found:", file))
      NULL
    }
  })
  
  return(datasets)
}

#' Interpolate and extrapolate consumption data
#' @param data Raw consumption data
#' @param gender_col Column name for gender-specific data
#' @return Complete time series data (1990-2020)
interpolate_consumption_data <- function(data, gender_col) {
  years <- seq(1990, 2020, by = 1)
  
  # Interpolation
  consumption_interp <- approx(data$Annee, data[[gender_col]], xout = years)
  
  # Create data frame
  consumption_complete <- data.frame(
    year = years,
    consumption = consumption_interp$y
  )
  
  # Extrapolation for missing years
  if(any(is.na(consumption_complete$consumption))) {
    # Fit linear model for extrapolation
    valid_data <- consumption_complete[!is.na(consumption_complete$consumption), ]
    extrapolation_model <- lm(consumption ~ year, data = valid_data)
    
    # Fill missing values
    missing_indices <- is.na(consumption_complete$consumption)
    consumption_complete$consumption[missing_indices] <- 
      predict(extrapolation_model, 
              newdata = consumption_complete[missing_indices, ])
  }
  
  return(consumption_complete)
}

cat("Data loading functions initialized successfully!\n")
