#' Step 6 Visualisation Functions
#' Simple plots for the single-run Step 6 result
#' Fully self-contained script with styling matching main visualisation.R

# Load required libraries
library(ggplot2)
library(patchwork)
library(here)  # If you want to save plots
library(tidyr)  # For pivot_longer

# Check if required packages are installed
required_packages <- c("ggplot2", "patchwork", "tidyr")
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
    CDR = result_step6$qty_remov
  )
  
  # Prepare cost data for stacked plot
  cost_data <- data.frame(
    Year = result_step6$years,
    Mitigation = result_step6$mitig_costs_annual,
    Removal = result_step6$remov_costs_annual,
    Residual = result_step6$temp_costs_annual
  ) %>%
    pivot_longer(cols = c(Mitigation, Removal, Residual),
                 names_to = "cost_type",
                 values_to = "cost_value")
  
  # Define theme for consistent styling
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # 1. Emissions plot (matching spaghetti plot style)
  p1 <- ggplot(plot_data, aes(x = Year, y = Cumulative_Emissions)) +
    geom_line(color = colorspace::darken("firebrick", 0.3), linewidth = 1) +
    geom_hline(yintercept = 650, linetype = "dashed", alpha = 0.7, color = "gray40") +
    labs(title = "Cumulative Emissions", y = "Cumulative Emissions (GtCO2)") +
    my_theme +
    theme(legend.position = "none")
  
  # 2. Temperature plot (matching spaghetti plot style)
  p2 <- ggplot(plot_data, aes(x = Year, y = Temperature)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.3) +
    geom_line(color = colorspace::darken("steelblue", 0.3), linewidth = 1) +
    geom_hline(yintercept = 1.5, linetype = "dashed", alpha = 0.7, color = "gray40") +
    scale_y_continuous(limits = c(NA, 2)) +
    labs(title = "Temperature Anomaly", y = "Temperature Anomaly (°C)") +
    my_theme +
    theme(legend.position = "none")
  
  # 3. Controls plot (matching the style)
  p3 <- ggplot(plot_data, aes(x = Year)) +
    geom_line(aes(y = Baseline_Emissions, color = "Baseline"), linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = Mitigation, color = "Mitigation"), linewidth = 1) +
    geom_line(aes(y = CDR, color = "CDR"), linewidth = 1) +
    scale_color_manual(values = c(
      "Baseline" = "black", 
      "Mitigation" = colorspace::darken("forestgreen", 0.3), 
      "CDR" = colorspace::darken("purple4", 0.3)
    )) +
    labs(title = "Control Strategies", y = "GtCO2/year", color = "Control") +
    my_theme +
    theme(legend.position = "bottom")
  
  # 4. Stacked annual costs plot (with colorblind-friendly, stronger colours)
  p4 <- ggplot(cost_data, aes(x = Year, y = cost_value, fill = cost_type)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_manual(values = c(
      "Mitigation" = "#E69F00",    # Orange
      "Removal" = "#009E73",       # Green  
      "Residual" = "#CC79A7"       # Pink
    )) +
    labs(
      title = "Annual Costs",
      y = "Annual Costs ($ Trillion)",
      fill = " "
    ) +
    my_theme +
    theme(legend.position = "bottom")
  
  # 5. Create blank plots for the bottom row to match original layout
  p5 <- ggplot() + 
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA))
  
  p6 <- ggplot() + 
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA))
  
  # Combine plots using patchwork (2x3 layout to match original)
  combined <- (p1 + p2) /
    (p3 + p4) /
    (p5 + p6)
  
  # Create subtitle with error handling
  if (!is.na(final_temp) && "final_emissions" %in% names(result_step6)) {
    subtitle_text <- paste0("Final emissions: ", round(result_step6$final_emissions, 1), 
                            " GtCO2, Final temp: ", round(final_temp, 2), "°C")
  } else {
    subtitle_text <- "Step 6 Results"
  }
  
  # Add overall title with key results (matching the annotation style)
  final_plot <- combined + 
    plot_annotation(
      title = paste0("Step 6 Results: Real IPCC Data (", scenario_name, ")"),
      subtitle = subtitle_text,
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.margin = margin(10, 0, 10, 0)
      )
    )
  
  return(final_plot)
}

# Usage example:
# Assuming you have result_step6 from running Step 6:
# step6_dashboard <- create_step6_dashboard(result_step6_shooting)
# print(step6_dashboard)

# Optional: Save the plot
# ggsave("step6_dashboard.png", step6_dashboard, width = 12, height = 8)