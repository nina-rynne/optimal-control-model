#' @title Multi-Scenario Plotting Functions
#' @description
#' Functions to create plots comparing results across multiple SSP scenarios.
#' All scenarios are plotted on the same axes with different colors.
#'
#' @author Nina Rynne
#' @date June 2025

# Load required libraries
library(ggplot2)
library(dplyr)
library(patchwork)
library(RColorBrewer)

# Define colorblind-friendly palette for scenarios
scenario_colors <- c(
  "#E69F00",  # Orange
  "#56B4E9",  # Sky blue
  "#009E73",  # Green
  "#F0E442",  # Yellow
  "#CC79A7"   # Pink
)

#' @title Plot Temperature Trajectories for Multiple Scenarios
#' @description Creates plot showing temperature trajectories for multiple scenarios
#' @param scenario_results Named list of results from run_scenario_comparison
#' @param title Plot title (optional)
#' @return ggplot object
plot_temperature_multi_scenario <- function(scenario_results, 
                                            title = "Temperature Trajectories") {
  
  # Extract data from all scenarios
  plot_data <- map_dfr(names(scenario_results), function(scenario_name) {
    result <- scenario_results[[scenario_name]]
    if (!is.null(result$solution) && result$success) {
      data.frame(
        scenario = scenario_name,
        years = result$solution$years,
        temperature = result$solution$temperature_anomaly
      )
    }
  })
  
  if (nrow(plot_data) == 0) {
    stop("No valid temperature data found across scenarios")
  }
  
  # Remove "SSP" prefix and "-Baseline" suffix for cleaner legend
  plot_data$scenario_clean <- gsub("^SSP|SSP", "", plot_data$scenario)
  plot_data$scenario_clean <- gsub("-Baseline$", "", plot_data$scenario_clean)
  
  # Define consistent theme
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = "white", color = "gray80", size = 0.3),
      legend.margin = margin(2, 4, 2, 4)
    )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = years, y = temperature, color = scenario_clean)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.3) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 1.5, linetype = "dashed", alpha = 0.7, color = "gray40") +
    scale_color_manual(
      name = "Scenario",
      values = scenario_colors[1:length(unique(plot_data$scenario_clean))]
    ) +
    scale_y_continuous(limits = c(NA, 2)) +
    labs(
      title = title,
      x = "Year",
      y = "Temperature Anomaly (°C)"
    ) +
    my_theme
  
  return(p)
}

#' @title Plot Cumulative Emissions for Multiple Scenarios
#' @description Creates plot showing cumulative emissions for multiple scenarios
#' @param scenario_results Named list of results from run_scenario_comparison
#' @param title Plot title (optional)
#' @return ggplot object
plot_emissions_multi_scenario <- function(scenario_results, 
                                          title = "Cumulative Emissions") {
  
  # Extract data from all scenarios
  plot_data <- map_dfr(names(scenario_results), function(scenario_name) {
    result <- scenario_results[[scenario_name]]
    if (!is.null(result$solution) && result$success) {
      data.frame(
        scenario = scenario_name,
        years = result$solution$years,
        cumulative_emissions = result$solution$cumulative_emissions
      )
    }
  })
  
  if (nrow(plot_data) == 0) {
    stop("No valid emissions data found across scenarios")
  }
  
  # Remove "SSP" prefix and "-Baseline" suffix for cleaner legend
  plot_data$scenario_clean <- gsub("^SSP|SSP", "", plot_data$scenario)
  plot_data$scenario_clean <- gsub("-Baseline$", "", plot_data$scenario_clean)
  
  # Define consistent theme
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = "white", color = "gray80", size = 0.3),
      legend.margin = margin(2, 4, 2, 4)
    )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = years, y = cumulative_emissions, color = scenario_clean)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(
      name = "Scenario",
      values = scenario_colors[1:length(unique(plot_data$scenario_clean))]
    ) +
    labs(
      title = title,
      x = "Year",
      y = "Cumulative Emissions (GtCO2)"
    ) +
    my_theme
  
  return(p)
}

#' @title Plot Mitigation Strategies for Multiple Scenarios
#' @description Creates plot showing mitigation strategies for multiple scenarios
#' @param scenario_results Named list of results from run_scenario_comparison
#' @param title Plot title (optional)
#' @return ggplot object
plot_mitigation_multi_scenario <- function(scenario_results, 
                                           title = "Mitigation Strategies") {
  
  # Extract data from all scenarios
  plot_data <- map_dfr(names(scenario_results), function(scenario_name) {
    result <- scenario_results[[scenario_name]]
    if (!is.null(result$solution) && result$success) {
      data.frame(
        scenario = scenario_name,
        years = result$solution$years,
        mitigation = result$solution$qty_mitig
      )
    }
  })
  
  if (nrow(plot_data) == 0) {
    stop("No valid mitigation data found across scenarios")
  }
  
  # Remove "SSP" prefix and "-Baseline" suffix for cleaner legend
  plot_data$scenario_clean <- gsub("^SSP|SSP", "", plot_data$scenario)
  plot_data$scenario_clean <- gsub("-Baseline$", "", plot_data$scenario_clean)
  
  # Define consistent theme
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = "white", color = "gray80", size = 0.3),
      legend.margin = margin(2, 4, 2, 4)
    )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = years, y = mitigation, color = scenario_clean)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(
      name = "Scenario",
      values = scenario_colors[1:length(unique(plot_data$scenario_clean))]
    ) +
    labs(
      title = title,
      x = "Year",
      y = "Mitigation (GtCO2/yr)"
    ) +
    my_theme
  
  return(p)
}

#' @title Plot CDR Strategies for Multiple Scenarios
#' @description Creates plot showing CDR strategies for multiple scenarios
#' @param scenario_results Named list of results from run_scenario_comparison
#' @param title Plot title (optional)
#' @return ggplot object
plot_cdr_multi_scenario <- function(scenario_results, 
                                    title = "CDR Strategies") {
  
  # Extract data from all scenarios
  plot_data <- map_dfr(names(scenario_results), function(scenario_name) {
    result <- scenario_results[[scenario_name]]
    if (!is.null(result$solution) && result$success) {
      data.frame(
        scenario = scenario_name,
        years = result$solution$years,
        cdr = result$solution$qty_remov
      )
    }
  })
  
  if (nrow(plot_data) == 0) {
    stop("No valid CDR data found across scenarios")
  }
  
  # Remove "SSP" prefix and "-Baseline" suffix for cleaner legend
  plot_data$scenario_clean <- gsub("^SSP|SSP", "", plot_data$scenario)
  plot_data$scenario_clean <- gsub("-Baseline$", "", plot_data$scenario_clean)
  
  # Define consistent theme
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = "white", color = "gray80", size = 0.3),
      legend.margin = margin(2, 4, 2, 4)
    )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = years, y = cdr, color = scenario_clean)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(
      name = "Scenario",
      values = scenario_colors[1:length(unique(plot_data$scenario_clean))]
    ) +
    labs(
      title = title,
      x = "Year",
      y = "CDR (GtCO2/yr)"
    ) +
    my_theme
  
  return(p)
}

#' @title Plot Total Costs for Multiple Scenarios
#' @description Creates plot showing total costs for multiple scenarios
#' @param scenario_results Named list of results from run_scenario_comparison
#' @param title Plot title (optional)
#' @return ggplot object
plot_total_cost_multi_scenario <- function(scenario_results, 
                                           title = "Total Costs") {
  
  # Extract data from all scenarios
  plot_data <- map_dfr(names(scenario_results), function(scenario_name) {
    result <- scenario_results[[scenario_name]]
    if (!is.null(result$solution) && result$success) {
      # Calculate total cost as sum of all cost components
      total_cost_annual <- result$solution$mitig_costs_annual + 
        result$solution$remov_costs_annual + 
        result$solution$temp_costs_annual
      
      data.frame(
        scenario = scenario_name,
        years = result$solution$years,
        total_cost = total_cost_annual
      )
    }
  })
  
  if (nrow(plot_data) == 0) {
    stop("No valid cost data found across scenarios")
  }
  
  # Remove "SSP" prefix and "-Baseline" suffix for cleaner legend
  plot_data$scenario_clean <- gsub("^SSP|SSP", "", plot_data$scenario)
  plot_data$scenario_clean <- gsub("-Baseline$", "", plot_data$scenario_clean)
  
  # Define consistent theme
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = "white", color = "gray80", size = 0.3),
      legend.margin = margin(2, 4, 2, 4)
    )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = years, y = total_cost, color = scenario_clean)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(
      name = "Scenario",
      values = scenario_colors[1:length(unique(plot_data$scenario_clean))]
    ) +
    labs(
      title = title,
      x = "Year",
      y = "Total Annual Cost ($ Trillion)"
    ) +
    my_theme
  
  return(p)
}

#' @title Create Multi-Scenario Dashboard
#' @description Creates a 2×3 dashboard comparing multiple scenarios
#' @param scenario_results Named list of results from run_scenario_comparison
#' @param save_plot Whether to save the plot (default: FALSE)
#' @param filename Custom filename for saving
#' @return Combined ggplot object
create_multi_scenario_dashboard <- function(scenario_results, 
                                            save_plot = FALSE,
                                            filename = NULL) {
  
  # Create individual plots
  p1 <- plot_temperature_multi_scenario(scenario_results)
  p2 <- plot_emissions_multi_scenario(scenario_results)
  p3 <- plot_mitigation_multi_scenario(scenario_results)
  p4 <- plot_cdr_multi_scenario(scenario_results)
  p5 <- plot_total_cost_multi_scenario(scenario_results)
  
  # Create blank plot for consistency with existing layout
  p6 <- ggplot() + 
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA))
  
  # Combine plots using patchwork (2×3 layout)
  combined <- (p1 + p2) /
    (p3 + p4) /
    (p5 + p6)
  
  # Count successful scenarios for subtitle
  n_scenarios <- length(scenario_results)
  n_successful <- sum(sapply(scenario_results, function(x) x$success))
  
  # Add overall title and subtitle
  final_plot <- combined + 
    plot_annotation(
      title = "Multi-Scenario Comparison: Optimal Control Results",
      subtitle = paste0("Comparing ", n_successful, " successful scenarios out of ", n_scenarios, " total"),
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.margin = margin(10, 0, 10, 0)
      )
    )
  
  # Save if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0("multi_scenario_dashboard_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    }
    
    filepath <- here::here("figs", filename)
    ggsave(filepath, final_plot, width = 190, height = 260, units = "mm", device = cairo_pdf)
    cat("Multi-scenario dashboard saved to:", filepath, "\n")
  }
  
  return(final_plot)
}

# Usage examples:
# Assuming you have scenario_results from run_scenario_comparison():
#
# # Create individual plots
# temp_plot <- plot_temperature_multi_scenario(scenario_results$scenario_results)
# emissions_plot <- plot_emissions_multi_scenario(scenario_results$scenario_results)
# mitigation_plot <- plot_mitigation_multi_scenario(scenario_results$scenario_results)
# cdr_plot <- plot_cdr_multi_scenario(scenario_results$scenario_results)
# cost_plot <- plot_total_cost_multi_scenario(scenario_results$scenario_results)
#
# # Create dashboard
# dashboard <- create_multi_scenario_dashboard(scenario_results$scenario_results)
# print(dashboard)
#
# # Save dashboard
# create_multi_scenario_dashboard(scenario_results$scenario_results, 
#                                save_plot = TRUE, 
#                                filename = "my_scenario_comparison.pdf")