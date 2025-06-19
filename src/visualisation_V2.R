#' @title New Visualization Functions for Optimal Control Results
#' @description
#' Clean visualization functions that work with current optimal control outputs
#' while maintaining the established visual style and color schemes.
#'
#' @author Nina Rynne
#' @date June 2025

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(colorspace)

#' @title Plot Temperature Trajectory
#' @description Creates a plot showing temperature anomaly over time with established styling
#' @param solution Output from optimal_control_shooting or optimal_control_solve
#' @param title Optional plot title
#' @return ggplot object
plot_temperature <- function(solution, title = "Temperature Anomaly") {
  
  # Create data frame for plotting
  temp_data <- data.frame(
    Year = solution$years,
    Temperature = solution$temperature_anomaly
  )
  
  # Define consistent theme (based on your existing code)
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  # Create plot with your established styling
  p <- ggplot(temp_data, aes(x = Year, y = Temperature)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.3) +
    geom_line(color = colorspace::darken("steelblue", 0.3), linewidth = 1) +
    geom_hline(yintercept = 1.5, linetype = "dashed", alpha = 0.7, color = "gray40") +
    scale_y_continuous(limits = c(NA, 2)) +
    labs(title = title, y = "Temperature Anomaly (°C)") +
    my_theme
  
  return(p)
}

#' @title Plot Cumulative Emissions
#' @description Creates a plot showing cumulative emissions over time
#' @param solution Output from optimal_control_shooting or optimal_control_solve
#' @param title Optional plot title
#' @return ggplot object
plot_cumulative_emissions <- function(solution, title = "Cumulative Emissions") {
  
  # Create data frame for plotting
  emissions_data <- data.frame(
    Year = solution$years,
    Cumulative_Emissions = solution$cumulative_emissions
  )
  
  # Define consistent theme
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  # Create plot
  p <- ggplot(emissions_data, aes(x = Year, y = Cumulative_Emissions)) +
    geom_line(color = colorspace::darken("firebrick", 0.3), linewidth = 1) +
    geom_hline(yintercept = solution$parameters$co2_target_2100, 
               linetype = "dashed", alpha = 0.7, color = "gray40") +
    labs(title = title, y = "Cumulative Emissions (GtCO2)") +
    my_theme
  
  return(p)
}

#' @title Plot Control Strategies
#' @description Creates a plot showing baseline emissions, mitigation, and CDR
#' @param solution Output from optimal_control_shooting or optimal_control_solve
#' @param title Optional plot title
#' @return ggplot object
plot_controls <- function(solution, title = "Control Strategies") {
  
  # Create data frame for plotting
  control_data <- data.frame(
    Year = solution$years,
    Baseline = solution$baseline_annual_emissions,
    Mitigation = solution$qty_mitig,
    CDR = solution$qty_remov
  )
  
  # Define consistent theme
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  # Create plot with your established color scheme
  p <- ggplot(control_data, aes(x = Year)) +
    geom_line(aes(y = Baseline, color = "Baseline"), linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = Mitigation, color = "Mitigation"), linewidth = 1) +
    geom_line(aes(y = CDR, color = "CDR"), linewidth = 1) +
    scale_color_manual(values = c(
      "Baseline" = "black", 
      "Mitigation" = colorspace::darken("forestgreen", 0.3), 
      "CDR" = colorspace::darken("purple4", 0.3)
    )) +
    labs(title = title, y = "GtCO2/year", color = "Control") +
    my_theme
  
  return(p)
}

#' @title Plot Annual Costs
#' @description Creates a stacked area plot showing annual cost breakdown
#' @param solution Output from optimal_control_shooting or optimal_control_solve
#' @param title Optional plot title
#' @return ggplot object
plot_annual_costs <- function(solution, title = "Annual Costs") {
  
  # Create data frame for plotting
  cost_data <- data.frame(
    Year = solution$years,
    Mitigation = solution$mitig_costs_annual,
    Removal = solution$remov_costs_annual,
    Residual = solution$temp_costs_annual
  ) %>%
    pivot_longer(cols = c(Mitigation, Removal, Residual),
                 names_to = "cost_type",
                 values_to = "cost_value")
  
  # Define consistent theme
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  # Create stacked area plot with colorblind-friendly colors (from your code)
  p <- ggplot(cost_data, aes(x = Year, y = cost_value, fill = cost_type)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_manual(values = c(
      "Mitigation" = "#E69F00",    # Orange
      "Removal" = "#009E73",       # Green  
      "Residual" = "#CC79A7"       # Pink
    )) +
    labs(title = title, y = "Annual Costs ($ Trillion)", fill = " ") +
    my_theme
  
  return(p)
}

#' @title Plot Adjoint Variable
#' @description Creates a plot showing the adjoint variable (shadow price) over time
#' @param solution Output from optimal_control_shooting or optimal_control_solve
#' @param title Optional plot title
#' @return ggplot object
plot_adjoint <- function(solution, title = "Adjoint Variable") {
  
  # Create data frame for plotting
  adjoint_data <- data.frame(
    Year = solution$years,
    Adjoint = solution$adjoint_var
  )
  
  # Define consistent theme
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  # Create plot
  p <- ggplot(adjoint_data, aes(x = Year, y = Adjoint)) +
    geom_line(color = colorspace::darken("darkorange", 0.3), linewidth = 1) +
    labs(title = title, y = "Adjoint Variable Value") +
    my_theme
  
  return(p)
}

#' @title Create Dashboard for Single Solution
#' @description Creates a 6-panel dashboard matching your established layout
#' @param solution Output from optimal_control_shooting or optimal_control_solve
#' @param scenario_name Name of scenario for subtitle
#' @param save_plot Whether to save the plot (default: FALSE)
#' @param filename Custom filename for saving
#' @return Combined ggplot object
create_single_solution_dashboard <- function(solution, 
                                             scenario_name = "Unknown",
                                             save_plot = FALSE, 
                                             filename = NULL) {
  
  # Create individual plots
  p1 <- plot_cumulative_emissions(solution)
  p2 <- plot_temperature(solution)
  p3 <- plot_controls(solution)
  p4 <- plot_annual_costs(solution)
  p5 <- plot_adjoint(solution)
  
  # Create blank plot for consistency with your layout
  p6 <- ggplot() + 
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA))
  
  # Combine plots using patchwork (matching your 2×3 layout)
  combined <- (p1 + p2) /
    (p3 + p4) /
    (p5 + p6)
  
  # Create subtitle with key results (matching your style)
  if (solution$converged) {
    subtitle_text <- paste0("Final emissions: ", round(solution$final_emissions, 1), 
                            " GtCO2, Final temp: ", round(solution$final_temperature, 2), "°C")
  } else {
    subtitle_text <- "Solution did not converge"
  }
  
  # Add overall title and subtitle (matching your annotation style)
  final_plot <- combined + 
    plot_annotation(
      title = paste0("Optimal Control Results: ", scenario_name),
      subtitle = subtitle_text,
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.margin = margin(10, 0, 10, 0)
      )
    )
  
  # Save if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0("optimal_control_dashboard_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    }
    
    filepath <- here::here("figs", filename)
    ggsave(filepath, final_plot, width = 190, height = 260, units = "mm", device = cairo_pdf)
    cat("Dashboard saved to:", filepath, "\n")
  }
  
  return(final_plot)
}

#' @title Create Multi-Run Spaghetti Dashboard  
#' @description Creates dashboard with spaghetti plots for multiple runs
#' @param solution_list List of solutions (e.g., from sensitivity analysis)
#' @param scenario_name Name of scenario
#' @param save_plot Whether to save the plot
#' @param filename Custom filename for saving
#' @return Combined ggplot object
create_spaghetti_dashboard <- function(solution_list, 
                                       scenario_name = "Multiple Runs",
                                       save_plot = FALSE,
                                       filename = NULL) {
  
  # Convert solution list to the format expected by your existing spaghetti functions
  formatted_solutions <- list()
  for (i in 1:length(solution_list)) {
    formatted_solutions[[paste0("run_", i)]] <- solution_list[[i]]
  }
  
  # Use your existing spaghetti plot functions with consistent theme
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  # Create spaghetti plots (simplified versions of your existing functions)
  p1 <- create_emissions_spaghetti_plot(formatted_solutions) + my_theme + labs(title = "Cumulative Emissions")
  p2 <- create_temperature_spaghetti_plot(formatted_solutions) + my_theme + labs(title = "Temperature Anomaly")
  p3 <- create_mitigation_spaghetti_plot(formatted_solutions) + my_theme + labs(title = "Mitigation")
  p4 <- create_cdr_spaghetti_plot(formatted_solutions) + my_theme + labs(title = "CDR")
  p5 <- create_adjoint_spaghetti_plot(formatted_solutions) + my_theme + labs(title = "Adjoint Variable")
  
  # Blank plot for consistency
  p6 <- ggplot() + theme_void() + theme(panel.background = element_rect(fill = "white", color = NA))
  
  # Combine plots
  combined <- (p1 + p2) / (p3 + p4) / (p5 + p6)
  
  # Add title
  final_plot <- combined + 
    plot_annotation(
      title = paste0("Sensitivity Analysis: ", scenario_name),
      subtitle = paste("Based on", length(solution_list), "parameter combinations"),
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.margin = margin(10, 0, 10, 0)
      )
    )
  
  # Save if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0("spaghetti_dashboard_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    }
    
    filepath <- here::here("figs", filename)
    ggsave(filepath, final_plot, width = 190, height = 260, units = "mm", device = cairo_pdf)
    cat("Spaghetti dashboard saved to:", filepath, "\n")
  }
  
  return(final_plot)
}

# Helper functions for spaghetti plots (simplified versions of your existing ones)
create_emissions_spaghetti_plot <- function(solution_list) {
  # Extract data
  plot_data <- map_dfr(names(solution_list), function(run_name) {
    sol <- solution_list[[run_name]]
    data.frame(
      run_id = run_name,
      years = sol$years,
      cumulative_emissions = sol$cumulative_emissions
    )
  })
  
  # Calculate summary
  summary_data <- plot_data %>%
    group_by(years) %>%
    summarise(median_val = median(cumulative_emissions), .groups = "drop")
  
  # Create plot
  ggplot() +
    geom_line(data = plot_data, aes(x = years, y = cumulative_emissions, group = run_id),
              color = "firebrick", alpha = 0.1, linewidth = 0.2) +
    geom_line(data = summary_data, aes(x = years, y = median_val),
              color = colorspace::darken("firebrick", 0.3), linewidth = 0.7) +
    labs(y = "Cumulative Emissions (GtCO2)")
}

create_temperature_spaghetti_plot <- function(solution_list) {
  # Extract data
  plot_data <- map_dfr(names(solution_list), function(run_name) {
    sol <- solution_list[[run_name]]
    data.frame(
      run_id = run_name,
      years = sol$years,
      temperature_anomaly = sol$temperature_anomaly
    )
  })
  
  # Calculate summary
  summary_data <- plot_data %>%
    group_by(years) %>%
    summarise(median_val = median(temperature_anomaly), .groups = "drop")
  
  # Create plot
  ggplot() +
    geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
    geom_line(data = plot_data, aes(x = years, y = temperature_anomaly, group = run_id),
              color = "steelblue", alpha = 0.1, linewidth = 0.2) +
    geom_line(data = summary_data, aes(x = years, y = median_val),
              color = colorspace::darken("steelblue", 0.3), linewidth = 0.7) +
    labs(y = "Temperature Anomaly (°C)")
}

create_mitigation_spaghetti_plot <- function(solution_list) {
  # Extract data
  plot_data <- map_dfr(names(solution_list), function(run_name) {
    sol <- solution_list[[run_name]]
    data.frame(
      run_id = run_name,
      years = sol$years,
      qty_mitig = sol$qty_mitig
    )
  })
  
  # Calculate summary
  summary_data <- plot_data %>%
    group_by(years) %>%
    summarise(median_val = median(qty_mitig), .groups = "drop")
  
  # Create plot
  ggplot() +
    geom_line(data = plot_data, aes(x = years, y = qty_mitig, group = run_id),
              color = "forestgreen", alpha = 0.1, linewidth = 0.2) +
    geom_line(data = summary_data, aes(x = years, y = median_val),
              color = colorspace::darken("forestgreen", 0.3), linewidth = 0.7) +
    labs(y = "Mitigation (GtCO2/yr)")
}

create_cdr_spaghetti_plot <- function(solution_list) {
  # Extract data
  plot_data <- map_dfr(names(solution_list), function(run_name) {
    sol <- solution_list[[run_name]]
    data.frame(
      run_id = run_name,
      years = sol$years,
      qty_remov = sol$qty_remov
    )
  })
  
  # Calculate summary
  summary_data <- plot_data %>%
    group_by(years) %>%
    summarise(median_val = median(qty_remov), .groups = "drop")
  
  # Create plot
  ggplot() +
    geom_line(data = plot_data, aes(x = years, y = qty_remov, group = run_id),
              color = "purple4", alpha = 0.1, linewidth = 0.2) +
    geom_line(data = summary_data, aes(x = years, y = median_val),
              color = colorspace::darken("purple4", 0.3), linewidth = 0.7) +
    labs(y = "CDR (GtCO2/yr)")
}

create_adjoint_spaghetti_plot <- function(solution_list) {
  # Extract data
  plot_data <- map_dfr(names(solution_list), function(run_name) {
    sol <- solution_list[[run_name]]
    data.frame(
      run_id = run_name,
      years = sol$years,
      adjoint_var = sol$adjoint_var
    )
  })
  
  # Calculate summary
  summary_data <- plot_data %>%
    group_by(years) %>%
    summarise(median_val = median(adjoint_var), .groups = "drop")
  
  # Create plot
  ggplot() +
    geom_line(data = plot_data, aes(x = years, y = adjoint_var, group = run_id),
              color = "darkorange", alpha = 0.1, linewidth = 0.2) +
    geom_line(data = summary_data, aes(x = years, y = median_val),
              color = colorspace::darken("darkorange", 0.3), linewidth = 0.7) +
    labs(y = "Adjoint Variable")
}

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
                   breaks = c(51, 52, 53, 54, 55, 56),
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
  
  # Keep "SSP" prefix but remove "-Baseline" suffix for cleaner legend
  plot_data$scenario_clean <- gsub("-Baseline$", "", plot_data$scenario)
  
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
      legend.position = "none"
    )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = years, y = temperature, color = scenario_clean)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 1.5, linetype = "dashed", alpha = 0.7, color = "red") +
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
  
  # Keep "SSP" prefix but remove "-Baseline" suffix for cleaner legend
  plot_data$scenario_clean <- gsub("-Baseline$", "", plot_data$scenario)
  
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
      legend.position = "none"
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
  
  # Keep "SSP" prefix but remove "-Baseline" suffix for cleaner legend
  plot_data$scenario_clean <- gsub("-Baseline$", "", plot_data$scenario)
  
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
      legend.position = "none"
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
  
  # Keep "SSP" prefix but remove "-Baseline" suffix for cleaner legend
  plot_data$scenario_clean <- gsub("-Baseline$", "", plot_data$scenario)
  
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
      legend.position = "none"
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
  
  # Keep "SSP" prefix but remove "-Baseline" suffix for cleaner legend
  plot_data$scenario_clean <- gsub("-Baseline$", "", plot_data$scenario)
  
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
      legend.position = "none"
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
  
  # Create shared legend in bottom right
  shared_legend <- get_legend(p1 + theme(legend.position = "right"))
  
  # Combine plots with swapped temperature/emissions positions
  combined <- (p2 + p1) /
    (p3 + p4) /
    (p5 + shared_legend)
  
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

#' @title Create Delayed Deployment Heatmap Dashboard
#' @description Creates a 2×2 dashboard showing all four delay analysis heatmaps
#' @param results_df Data frame from run_delayed_deployment_analysis with columns:
#'   mitigation_delay, cdr_delay, peak_temperature, total_cost, years_above_1p5, total_cdr_units, feasible
#' @param scenario_name Name of scenario for subtitle (default: "Unknown")
#' @param save_plot Whether to save the plot (default: FALSE)
#' @param filename Custom filename for saving
#' @return Combined ggplot object
create_delay_heatmap_dashboard <- function(results_df, 
                                           scenario_name = "Unknown",
                                           save_plot = FALSE,
                                           filename = NULL) {
  
  # Check if required data exists
  required_cols <- c("mitigation_delay", "cdr_delay", "peak_temperature", "total_cost", 
                     "years_above_1p5", "total_cdr_units", "feasible")
  missing_cols <- setdiff(required_cols, names(results_df))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Create individual heatmaps with improved contour settings
  
  # 1. Peak Temperature Heatmap (top-left)
  p1 <- create_peak_temperature_heatmap(
    results_df = results_df,
    title = "Peak Temperature",
    add_contours = TRUE
  )
  
  # 2. Years Above 1.5°C Heatmap (top-right) - with fixed contours
  p2 <- create_years_above_1p5_heatmap(
    results_df = results_df,
    title = "Years Above 1.5°C",
    add_contours = TRUE
  )
  
  # Override contours for years above 1.5°C to fix doubled-up lines
  if (!is.null(p2)) {
    # Determine appropriate contour breaks based on data range
    years_range <- range(results_df$years_above_1p5, na.rm = TRUE)
    contour_breaks <- seq(ceiling(years_range[1]), floor(years_range[2]), by = 1)
    
    # Rebuild the plot with specific contour breaks
    plot_data <- results_df %>%
      filter(!is.na(years_above_1p5))
    
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
    
    p2 <- ggplot(plot_data, aes(x = mitigation_delay, y = cdr_delay, fill = years_above_1p5)) +
      geom_tile() +
      scale_fill_viridis_c(
        name = "Years\nAbove\n1.5°C",
        option = "plasma",
        direction = -1
      ) +
      geom_contour(aes(z = years_above_1p5), 
                   breaks = contour_breaks,
                   color = "white", 
                   alpha = 0.8, 
                   linewidth = 0.5) +
      labs(
        title = "Years Above 1.5°C",
        x = "Mitigation Deployment Delay (years)",
        y = "CDR Deployment Delay (years)"
      ) +
      my_theme +
      coord_equal()
    
    # Add infeasible markers
    infeasible_data <- plot_data %>% filter(!feasible)
    if (nrow(infeasible_data) > 0) {
      p2 <- p2 + 
        geom_point(data = infeasible_data, 
                   aes(x = mitigation_delay, y = cdr_delay), 
                   shape = 4, size = 2, color = "red", alpha = 0.8, 
                   stroke = 1.5, inherit.aes = FALSE)
    }
  }
  
  # 3. Total Cost Heatmap (bottom-left)
  p3 <- create_total_cost_heatmap(
    results_df = results_df,
    title = "Total Cost",
    add_contours = TRUE
  )
  
  # 4. CDR Units Heatmap (bottom-right)
  p4 <- create_cdr_units_heatmap(
    results_df = results_df,
    title = "Total CDR Units",
    add_contours = TRUE
  )
  
  # Check if any plots failed to create
  plots_created <- sum(!sapply(list(p1, p2, p3, p4), is.null))
  
  if (plots_created == 0) {
    stop("No valid heatmaps could be created from the data")
  }
  
  # Create 2×2 layout using patchwork
  combined <- (p1 + p2) / (p3 + p4)
  
  # Count feasible combinations for subtitle
  n_combinations <- nrow(results_df)
  n_feasible <- sum(results_df$feasible, na.rm = TRUE)
  
  # Add overall title and subtitle (matching existing dashboard style)
  final_plot <- combined + 
    plot_annotation(
      title = paste0("Delayed Deployment Analysis: ", scenario_name),
      subtitle = paste0("Total combinations: ", n_combinations, " | Feasible: ", n_feasible, 
                        " (", sprintf("%.1f%%)", 100 * n_feasible / n_combinations)),
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.margin = margin(10, 0, 10, 0)
      )
    )
  
  # Save if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0("delay_heatmap_dashboard_", scenario_name, "_", 
                         format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    }
    
    filepath <- here::here("figs", filename)
    ggsave(filepath, final_plot, width = 190, height = 190, units = "mm", device = cairo_pdf)
    cat("Delay heatmap dashboard saved to:", filepath, "\n")
  }
  
  return(final_plot)
}

# Usage example:
# Assuming you have delay_results from run_delayed_deployment_analysis():
#
# # Create dashboard
# dashboard <- create_delay_heatmap_dashboard(
#   results_df = delay_results$results,
#   scenario_name = "SSP3-Baseline",
#   save_plot = FALSE
# )
# print(dashboard)
#
# # Save dashboard
# create_delay_heatmap_dashboard(
#   results_df = delay_results$results,
#   scenario_name = "SSP3-Baseline", 
#   save_plot = TRUE,
#   filename = "my_delay_analysis.pdf"
# )