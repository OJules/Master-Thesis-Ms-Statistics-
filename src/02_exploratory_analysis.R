# ==============================================================================
# Chapter 2: Exploratory Data Analysis and Visualization
# ==============================================================================

source("src/utils/plotting_functions.R")

#' Create tobacco consumption evolution plot by gender
create_tobacco_evolution_plot <- function(datasets) {
  
  # Prepare color and line schemes
  LINES <- c("Homme" = "solid", "Femme" = "dotted")
  Coul <- c("Monitorage" = "orange", "Panel" = "green", 
            "OFSP" = "blue", "OMS" = "gray")
  
  # Extract variables
  OMS_data <- datasets$OMS
  OFSP_data <- datasets$OFSP
  Panel_data <- datasets$Panel
  neutre <- datasets$neutre
  Monitorage_data <- datasets$Monitorage
  
  if(is.null(OMS_data) || is.null(OFSP_data)) {
    warning("Required datasets not available")
    return(NULL)
  }
  
  # Create comprehensive plot
  test <- ggplot() +
    geom_point(data = Monitorage_data, 
               aes(x = year, y = Msmokers_Monitorage, colour = "Monitorage")) +
    geom_line(data = Monitorage_data,
              aes(x = year, y = Msmokers_Monitorage, colour = "Monitorage", 
                  linetype = "Homme")) +
    geom_point(data = Monitorage_data,
               aes(x = year, y = Wsmokers_Monitorage, colour = "Monitorage")) +
    geom_line(data = Monitorage_data,
              aes(x = year, y = Wsmokers_Monitorage, linetype = "Femme", 
                  colour = "Monitorage")) +
    geom_point(data = OMS_data, 
               aes(x = year, y = Msmokers_OMS, colour = "OMS")) +
    geom_line(data = OMS_data,
              aes(x = year, y = Msmokers_OMS, colour = "OMS", 
                  linetype = "Homme")) +
    geom_point(data = OMS_data, 
               aes(x = year, y = Wsmokers_OMS, colour = "OMS")) +
    geom_line(data = OMS_data,
              aes(x = year, y = Wsmokers_OMS, linetype = "Femme", 
                  colour = "OMS")) +
    scale_linetype_manual(values = LINES, name = "Sexe:") +
    scale_colour_manual(values = Coul, name = "Source:") +
    scale_y_continuous(breaks = seq(0, 40, 10)) +
    scale_x_continuous(breaks = seq(1990, 2020, 5)) +
    labs(x = "Année", y = "Pourcentage", 
         title = "Évolution de la Consommation de Tabac par Genre") +
    theme_classic() +
    theme(
      axis.title.y = element_text(margin = margin(t=0, r=15, b=0, l=0)),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      legend.text.align = 1, 
      legend.position = "bottom"
    )
  
  return(test)
}

cat("Exploratory analysis functions loaded successfully!\n")
