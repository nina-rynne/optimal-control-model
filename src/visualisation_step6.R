#' Step 6 Visualization Functions
#' Simple plots for the single-run Step 6 result
#' Fully self-contained script

# Load required libraries
library(ggplot2)
library(patchwork)
library(here)  # If you want to save plots

# Check if required packages are installed
required_packages <- c("ggplot2", "patchwork")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if(length(missing_packages) > 0) {
  stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
}

#' Create Step 6 Dashboard
create_step6_dashboard <- function(result_step6) {
  
  # Debug: Check what's in the result object
  cat("Available result fields:", names(result_step6), "\n")
  
  # Handle both Step 6 and Step 6.1 (shooting) result structures
  if ("temperature_anomaly" %in% names(result_step6)) {
    # Standard Step 6 result
    final_temp <- result_step6$temperature_anomaly[result_step6$n_years]
    scenario_name <- ifelse("scenario" %in% names(result_step6), 
                            result_step6$scenario, "Unknown")
  } else if ("final_temperature" %in% names(result_step6)) {
    # Step 6.1 shooting result  
    final_temp <- result_step6$final_temperature
    scenario_name <- "SSP3-Baseline"  # Default for shooting method
  } else {
    # Try to calculate final temperature from available data
    if ("temperature_anomaly" %in% names(result_step6)) {
      final_temp <- tail(result_step6$temperature_anomaly, 1)
    } else {
      final_temp <- NA
      cat("Warning: Could not find temperature data\n")
    }
    scenario_name <- "Unknown"
  }
  
  # Prepare data for plotting - handle missing fields gracefully
  plot_data <- data.frame(
    Year = result_step6$years,
    Baseline_Emissions = result_step6$baseline_annual_emissions,
    Cumulative_Emissions = result_step6$cumulative_emissions,
    Temperature = result_step6$temperature_anomaly,
    Mitigation = result_step6$qty_mitig,
    CDR = result_step6$qty_remov,
    Adjoint = result_step6$adjoint_var
  )
  
  # Common theme
  my_theme <- theme_minimal() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # 1. Emissions plot
  p1 <- ggplot(plot_data, aes(x = Year, y = Cumulative_Emissions)) +
    geom_line(color = "firebrick", linewidth = 1) +
    geom_hline(yintercept = 650, linetype = "dashed", alpha = 0.7) +
    labs(title = "Cumulative Emissions", y = "GtCO2") +
    my_theme
  
  # 2. Temperature plot
  p2 <- ggplot(plot_data, aes(x = Year, y = Temperature)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_hline(yintercept = 1.5, linetype = "dashed", alpha = 0.7) +
    labs(title = "Temperature Anomaly", y = "°C") +
    my_theme
  
  # 3. Controls plot
  p3 <- ggplot(plot_data, aes(x = Year)) +
    geom_line(aes(y = Baseline_Emissions, color = "Baseline"), linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = Mitigation, color = "Mitigation"), linewidth = 1) +
    geom_line(aes(y = CDR, color = "CDR"), linewidth = 1) +
    scale_color_manual(values = c("Baseline" = "black", "Mitigation" = "forestgreen", "CDR" = "purple")) +
    labs(title = "Control Strategies", y = "GtCO2/year", color = "Control") +
    my_theme +
    theme(legend.position = "bottom")
  
  # 4. Adjoint plot
  p4 <- ggplot(plot_data, aes(x = Year, y = Adjoint)) +
    geom_line(color = "darkorange", linewidth = 1) +
    labs(title = "Adjoint Variable (Shadow Price)", y = "λ(t)") +
    my_theme
  
  # Combine plots
  combined <- (p1 + p2) / (p3 + p4)
  
  # Create subtitle with error handling
  if (!is.na(final_temp) && "final_emissions" %in% names(result_step6)) {
    subtitle_text <- paste0("Final emissions: ", round(result_step6$final_emissions, 1), 
                            " GtCO2, Final temp: ", round(final_temp, 2), "°C")
  } else {
    subtitle_text <- "Step 6 Results"
  }
  
  # Add overall title with key results
  final_plot <- combined + 
    plot_annotation(
      title = paste0("Step 6 Results: Real IPCC Data (", scenario_name, ")"),
      subtitle = subtitle_text,
      theme = theme(plot.title = element_text(size = 14, face = "bold"))
    )
  
  return(final_plot)
}

# Usage example:
# Assuming you have result_step6 from running Step 6:
step6_dashboard <- create_step6_dashboard(result_step6_shooting)
print(step6_dashboard)

# Optional: Save the plot
ggsave("step6_dashboard.png", step6_dashboard, width = 12, height = 8)