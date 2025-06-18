#' @title Heatmap Functions for Delayed Deployment Analysis
#' @description
#' Functions to create heatmaps showing the effect of delayed mitigation and CDR deployment
#' on various outcome metrics. All heatmaps use mitigation delay (x-axis) vs CDR delay (y-axis).
#'
#' @author Nina Rynne
#' @date June 2025

# Load required libraries
library(ggplot2)
library(dplyr)
library(viridis)

#' @title Create Peak Temperature Heatmap
#' @description Creates heatmap showing peak temperature vs deployment delays
#' @param results_df Data frame from run_delayed_deployment_analysis with columns:
#'   mitigation_delay, cdr_delay, peak_temperature, feasible
#' @param title Plot title (optional)
#' @param add_contours Whether to add contour lines (default: TRUE)
#' @return ggplot object
create_peak_temperature_heatmap <- function(results_df, 
                                            title = "Peak Temperature vs Deployment Delays",
                                            add_contours = TRUE) {
  
  # Filter for valid results
  plot_data <- results_df %>%
    filter(!is.na(peak_temperature))
  
  if (nrow(plot_data) == 0) {
    stop("No valid peak temperature data found")
  }
  
  # Define consistent theme (matching your existing code)
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
  
  # Create base heatmap
  p <- ggplot(plot_data, aes(x = mitigation_delay, y = cdr_delay, fill = peak_temperature)) +
    geom_tile() +
    scale_fill_viridis_c(
      name = "Peak\nTemp\n(°C)",
      option = "plasma",
      direction = -1
    ) +
    labs(
      title = title,
      x = "Mitigation Deployment Delay (years)",
      y = "CDR Deployment Delay (years)"
    ) +
    my_theme +
    coord_equal()
  
  # Add contour lines if requested
  if (add_contours) {
    p <- p + 
      geom_contour(aes(z = peak_temperature), 
                   color = "white", 
                   alpha = 0.6, 
                   linewidth = 0.5)
  }
  
  # Add X marks for infeasible combinations (if any exist)
  infeasible_data <- plot_data %>% filter(!feasible)
  if (nrow(infeasible_data) > 0) {
    p <- p + 
      geom_point(data = infeasible_data, 
                 aes(x = mitigation_delay, y = cdr_delay), 
                 shape = 4, size = 2, color = "red", alpha = 0.8, 
                 stroke = 1.5, inherit.aes = FALSE)
  }
  
  return(p)
}

#' @title Create Years Above 1.5°C Heatmap
#' @description Creates heatmap showing years above 1.5°C vs deployment delays
#' @param results_df Data frame from run_delayed_deployment_analysis with columns:
#'   mitigation_delay, cdr_delay, years_above_1p5, feasible
#' @param title Plot title (optional)
#' @param add_contours Whether to add contour lines (default: TRUE)
#' @return ggplot object
create_years_above_1p5_heatmap <- function(results_df, 
                                           title = "Years Above 1.5°C vs Deployment Delays",
                                           add_contours = TRUE) {
  
  # Filter for valid results
  plot_data <- results_df %>%
    filter(!is.na(years_above_1p5))
  
  if (nrow(plot_data) == 0) {
    stop("No valid years above 1.5°C data found")
  }
  
  # Define consistent theme
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
  
  # Create base heatmap
  p <- ggplot(plot_data, aes(x = mitigation_delay, y = cdr_delay, fill = years_above_1p5)) +
    geom_tile() +
    scale_fill_viridis_c(
      name = "Years\nAbove\n1.5°C",
      option = "plasma",
      direction = -1
    ) +
    labs(
      title = title,
      x = "Mitigation Deployment Delay (years)",
      y = "CDR Deployment Delay (years)"
    ) +
    my_theme +
    coord_equal()
  
  # Add contour lines if requested
  if (add_contours) {
    p <- p + 
      geom_contour(aes(z = years_above_1p5), 
                   color = "white", 
                   alpha = 0.6, 
                   linewidth = 0.5)
  }
  
  # Add X marks for infeasible combinations (if any exist)
  infeasible_data <- plot_data %>% filter(!feasible)
  if (nrow(infeasible_data) > 0) {
    p <- p + 
      geom_point(data = infeasible_data, 
                 aes(x = mitigation_delay, y = cdr_delay), 
                 shape = 4, size = 2, color = "red", alpha = 0.8, 
                 stroke = 1.5, inherit.aes = FALSE)
  }
  
  return(p)
}

#' @title Create Total Cost Heatmap
#' @description Creates heatmap showing total cost vs deployment delays
#' @param results_df Data frame from run_delayed_deployment_analysis with columns:
#'   mitigation_delay, cdr_delay, total_cost, feasible
#' @param title Plot title (optional)
#' @param add_contours Whether to add contour lines (default: TRUE)
#' @return ggplot object
create_total_cost_heatmap <- function(results_df, 
                                      title = "Total Cost vs Deployment Delays",
                                      add_contours = TRUE) {
  
  # Filter for valid results
  plot_data <- results_df %>%
    filter(!is.na(total_cost))
  
  if (nrow(plot_data) == 0) {
    stop("No valid total cost data found")
  }
  
  # Define consistent theme
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
  
  # Create base heatmap
  p <- ggplot(plot_data, aes(x = mitigation_delay, y = cdr_delay, fill = total_cost)) +
    geom_tile() +
    scale_fill_viridis_c(
      name = "Total\nCost\n($ Trillion)",
      option = "plasma",
      direction = -1
    ) +
    labs(
      title = title,
      x = "Mitigation Deployment Delay (years)",
      y = "CDR Deployment Delay (years)"
    ) +
    my_theme +
    coord_equal()
  
  # Add contour lines if requested
  if (add_contours) {
    p <- p + 
      geom_contour(aes(z = total_cost), 
                   color = "white", 
                   alpha = 0.6, 
                   linewidth = 0.5)
  }
  
  # Add X marks for infeasible combinations (if any exist)
  infeasible_data <- plot_data %>% filter(!feasible)
  if (nrow(infeasible_data) > 0) {
    p <- p + 
      geom_point(data = infeasible_data, 
                 aes(x = mitigation_delay, y = cdr_delay), 
                 shape = 4, size = 2, color = "red", alpha = 0.8, 
                 stroke = 1.5, inherit.aes = FALSE)
  }
  
  return(p)
}

#' @title Create CDR Units Heatmap
#' @description Creates heatmap showing total CDR units vs deployment delays
#' @param results_df Data frame from run_delayed_deployment_analysis with columns:
#'   mitigation_delay, cdr_delay, total_cdr_units, feasible
#' @param title Plot title (optional)
#' @param add_contours Whether to add contour lines (default: TRUE)
#' @return ggplot object
create_cdr_units_heatmap <- function(results_df, 
                                     title = "Total CDR Units vs Deployment Delays",
                                     add_contours = TRUE) {
  
  # Filter for valid results
  plot_data <- results_df %>%
    filter(!is.na(total_cdr_units))
  
  if (nrow(plot_data) == 0) {
    stop("No valid CDR units data found")
  }
  
  # Define consistent theme
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
  
  # Create base heatmap
  p <- ggplot(plot_data, aes(x = mitigation_delay, y = cdr_delay, fill = total_cdr_units)) +
    geom_tile() +
    scale_fill_viridis_c(
      name = "Total\nCDR\n(GtCO2)",
      option = "plasma",
      direction = -1
    ) +
    labs(
      title = title,
      x = "Mitigation Deployment Delay (years)",
      y = "CDR Deployment Delay (years)"
    ) +
    my_theme +
    coord_equal()
  
  # Add contour lines if requested
  if (add_contours) {
    p <- p + 
      geom_contour(aes(z = total_cdr_units), 
                   color = "white", 
                   alpha = 0.6, 
                   linewidth = 0.5)
  }
  
  # Add X marks for infeasible combinations (if any exist)
  infeasible_data <- plot_data %>% filter(!feasible)
  if (nrow(infeasible_data) > 0) {
    p <- p + 
      geom_point(data = infeasible_data, 
                 aes(x = mitigation_delay, y = cdr_delay), 
                 shape = 4, size = 2, color = "red", alpha = 0.8, 
                 stroke = 1.5, inherit.aes = FALSE)
  }
  
  return(p)
}

# Usage examples:
# Assuming you have delay_results from run_delayed_deployment_analysis():
#
# # Create individual heatmaps
# peak_temp_heatmap <- create_peak_temperature_heatmap(delay_results$results_grid)
# years_1p5_heatmap <- create_years_above_1p5_heatmap(delay_results$results_grid)
# cost_heatmap <- create_total_cost_heatmap(delay_results$results_grid)
# cdr_heatmap <- create_cdr_units_heatmap(delay_results$results_grid)
#
# # Display plots
# print(peak_temp_heatmap)
# print(years_1p5_heatmap)
# print(cost_heatmap)
# print(cdr_heatmap)
#
# # Save plots if needed
# ggsave("peak_temperature_heatmap.pdf", peak_temp_heatmap, 
#        width = 190, height = 260, units = "mm")