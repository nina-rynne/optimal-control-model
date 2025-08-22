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
  
  # Create spaghetti plots with alphabetic labels - now using 6 plots in 3x2 layout
  p1 <- create_emissions_spaghetti_plot(formatted_solutions) + 
    my_theme + 
    labs(title = "a) Cumulative Emissions")
  
  p2 <- create_temperature_spaghetti_plot(formatted_solutions) + 
    my_theme + 
    labs(title = "b) Temperature Anomaly")
  
  p3 <- create_mitigation_spaghetti_plot(formatted_solutions) + 
    my_theme + 
    labs(title = "c) Mitigation")
  
  p4 <- create_cdr_spaghetti_plot(formatted_solutions) + 
    my_theme + 
    labs(title = "d) CDR")
  
  p5 <- create_abatement_cost_spaghetti_plot(formatted_solutions) + 
    my_theme + 
    labs(title = "e) Abatement Costs")
  
  p6 <- create_damage_cost_spaghetti_plot(formatted_solutions) + 
    my_theme + 
    labs(title = "f) Damage Costs")
  
  # Combine plots in 3x2 layout
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
              color = "firebrick", alpha = 0.05, linewidth = 0.2) +
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
              color = "steelblue", alpha = 0.05, linewidth = 0.2) +
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
              color = "forestgreen", alpha = 0.05, linewidth = 0.2) +
    geom_line(data = summary_data, aes(x = years, y = median_val),
              color = colorspace::darken("forestgreen", 0.3), linewidth = 0.7) +
    scale_y_continuous(limits = c(0, 65)) +
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
              color = "purple4", alpha = 0.05, linewidth = 0.2) +
    geom_line(data = summary_data, aes(x = years, y = median_val),
              color = colorspace::darken("purple4", 0.3), linewidth = 0.7) +
    scale_y_continuous(limits = c(0, 65)) +
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
              color = "darkorange", alpha = 0.05, linewidth = 0.2) +
    geom_line(data = summary_data, aes(x = years, y = median_val),
              color = colorspace::darken("darkorange", 0.3), linewidth = 0.7) +
    labs(y = "Adjoint Variable")
}

#' @title Create Total Cost Spaghetti Plot
#' @description Creates spaghetti plot showing total annual cost across multiple runs
#' @param solution_list Named list of solutions
#' @return ggplot object
create_total_cost_spaghetti_plot <- function(solution_list) {
  # First pass: calculate global maximum
  all_costs <- map_dbl(solution_list, function(sol) {
    total_cost_annual <- sol$mitig_costs_annual + 
      sol$remov_costs_annual + 
      sol$temp_costs_annual
    max(total_cost_annual, na.rm = TRUE)
  })
  global_max <- max(all_costs, na.rm = TRUE)
  
  # Extract data with global normalization
  plot_data <- map_dfr(names(solution_list), function(run_name) {
    sol <- solution_list[[run_name]]
    # Calculate total cost as sum of all cost components
    total_cost_annual <- sol$mitig_costs_annual + 
      sol$remov_costs_annual + 
      sol$temp_costs_annual
    
    # Normalize relative to global maximum
    relative_cost <- total_cost_annual / global_max
    
    data.frame(
      run_id = run_name,
      years = sol$years,
      total_cost = relative_cost
    )
  })
  
  # Calculate summary
  summary_data <- plot_data %>%
    group_by(years) %>%
    summarise(median_val = median(total_cost), .groups = "drop")
  
  # Create plot
  ggplot() +
    geom_line(data = plot_data, aes(x = years, y = total_cost, group = run_id),
              color = "darkorange", alpha = 0.05, linewidth = 0.2) +
    geom_line(data = summary_data, aes(x = years, y = median_val),
              color = colorspace::darken("darkorange", 0.3), linewidth = 0.7) +
    labs(y = "Relative Total Cost")
}

#' @title Create Abatement Cost Spaghetti Plot
#' @description Creates spaghetti plot showing abatement costs (mitigation + removal) 
#' as proportion of baseline GWP across multiple runs
#' @param solution_list Named list of solutions
#' @return ggplot object
create_abatement_cost_spaghetti_plot <- function(solution_list) {
  # Extract data with normalization by baseline GWP
  plot_data <- map_dfr(names(solution_list), function(run_name) {
    sol <- solution_list[[run_name]]
    # Calculate abatement cost as sum of mitigation and removal costs
    abatement_cost_annual <- sol$mitig_costs_annual + sol$remov_costs_annual
    
    # Normalize by baseline GWP (as proportion)
    abatement_cost_proportion <- abatement_cost_annual / sol$baseline_annual_gwp
    
    data.frame(
      run_id = run_name,
      years = sol$years,
      abatement_cost_proportion = abatement_cost_proportion
    )
  })
  
  # Calculate summary
  summary_data <- plot_data %>%
    group_by(years) %>%
    summarise(median_val = median(abatement_cost_proportion, na.rm = TRUE), .groups = "drop")
  
  # Create plot with reduced opacity
  ggplot() +
    geom_line(data = plot_data, aes(x = years, y = abatement_cost_proportion, group = run_id),
              color = "darkorange", alpha = 0.05, linewidth = 0.2) +  # Reduced from 0.1 to 0.05
    geom_line(data = summary_data, aes(x = years, y = median_val),
              color = colorspace::darken("darkorange", 0.3), linewidth = 0.7) +
    scale_y_continuous(limits = c(0, 4)) +
    labs(y = "Abatement Costs\n(% of GWP)")
}

#' @title Create Damage Cost Spaghetti Plot
#' @description Creates spaghetti plot showing damage costs (temperature costs) 
#' as proportion of baseline GWP across multiple runs
#' @param solution_list Named list of solutions
#' @return ggplot object
create_damage_cost_spaghetti_plot <- function(solution_list) {
  # Extract data with normalization by baseline GWP
  plot_data <- map_dfr(names(solution_list), function(run_name) {
    sol <- solution_list[[run_name]]
    # Use temperature costs as damage costs
    damage_cost_annual <- sol$temp_costs_annual
    
    # Normalize by baseline GWP (as proportion)
    damage_cost_proportion <- damage_cost_annual / sol$baseline_annual_gwp
    
    data.frame(
      run_id = run_name,
      years = sol$years,
      damage_cost_proportion = damage_cost_proportion
    )
  })
  
  # Calculate summary
  summary_data <- plot_data %>%
    group_by(years) %>%
    summarise(median_val = median(damage_cost_proportion, na.rm = TRUE), .groups = "drop")
  
  # Create plot with reduced opacity
  ggplot() +
    geom_line(data = plot_data, aes(x = years, y = damage_cost_proportion, group = run_id),
              color = "maroon", alpha = 0.05, linewidth = 0.2) +  # Reduced from 0.1 to 0.05
    geom_line(data = summary_data, aes(x = years, y = median_val),
              color = colorspace::darken("maroon", 0.3), linewidth = 0.7) +
    scale_y_continuous(limits = c(0, 4)) +
    labs(y = "Damage Costs\n(% of GWP)")
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
      option = "viridis",
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
  
  # Create individual plots with alphabetic labels
  p1 <- plot_temperature_multi_scenario(scenario_results) + 
    labs(title = "B) Temperature Trajectories")
  
  p2 <- plot_emissions_multi_scenario(scenario_results) + 
    labs(title = "A) Cumulative Emissions")
  
  p3 <- plot_mitigation_multi_scenario(scenario_results) + 
    labs(title = "C) Mitigation Strategies")
  
  p4 <- plot_cdr_multi_scenario(scenario_results) + 
    labs(title = "D) CDR Strategies")
  
  p5 <- plot_total_cost_multi_scenario(scenario_results) + 
    labs(title = "E) Total Costs")
  
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


#' @title Create Control Comparison Dashboard
#' @description
#' Creates a 3x3 dashboard comparing temperature trajectories, control strategies, and annual costs
#' under different control scenarios: no controls, mitigation only, and full controls.
#' Top row shows temperature trajectories, middle row shows control strategies with legend,
#' bottom row shows annual cost breakdown. All panels use consistent scales for easy comparison.
#'
#' @param no_controls_result Solution from scenario with both mitigation and CDR disabled
#' @param mitigation_only_result Solution from scenario with only mitigation enabled
#' @param full_controls_result Solution from scenario with both controls enabled
#' @param scenario_name Name of the baseline scenario used (e.g., "SSP3-Baseline")
#' @param save_plot Whether to save the plot (default: FALSE)
#' @param filename Custom filename for saving
#' @return Combined ggplot object
#'
#' @examples
#' # Create comparison plot
#' comparison_plot <- create_control_comparison(
#'   no_controls_result = scenario_1_no_controls,
#'   mitigation_only_result = scenario_2_mitigation_only,
#'   full_controls_result = scenario_3_full_controls,
#'   scenario_name = "SSP3-Baseline"
#' )
create_control_comparison <- function(no_controls_result,
                                      mitigation_only_result,
                                      full_controls_result,
                                      scenario_name = "Unknown",
                                      save_plot = FALSE,
                                      filename = NULL) {
  
  # Input validation
  required_fields <- c("years", "temperature_anomaly", "converged", "baseline_annual_emissions", 
                       "qty_mitig", "qty_remov", "mitig_costs_annual", "remov_costs_annual", "temp_costs_annual")
  
  for (result in list(no_controls_result, mitigation_only_result, full_controls_result)) {
    missing_fields <- setdiff(required_fields, names(result))
    if (length(missing_fields) > 0) {
      stop("Missing required fields in result: ", paste(missing_fields, collapse = ", "))
    }
  }
  
  # Check if all scenarios converged
  if (!all(c(no_controls_result$converged, mitigation_only_result$converged, full_controls_result$converged))) {
    warning("Some scenarios did not converge. Results may not be reliable.")
  }
  
  # Define consistent theme for plots without legend
  my_theme_no_legend <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Define theme for plots with legend
  my_theme_with_legend <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Calculate shared y-axis limits for temperature plots
  all_temps <- c(
    no_controls_result$temperature_anomaly,
    mitigation_only_result$temperature_anomaly,
    full_controls_result$temperature_anomaly
  )
  temp_y_limits <- c(0, max(all_temps, na.rm = TRUE) * 1.05)
  
  # Calculate shared y-axis limits for control plots
  all_controls <- c(
    no_controls_result$baseline_annual_emissions,
    no_controls_result$qty_mitig,
    no_controls_result$qty_remov,
    mitigation_only_result$baseline_annual_emissions,
    mitigation_only_result$qty_mitig,
    mitigation_only_result$qty_remov,
    full_controls_result$baseline_annual_emissions,
    full_controls_result$qty_mitig,
    full_controls_result$qty_remov
  )
  control_y_limits <- c(0, max(all_controls, na.rm = TRUE) * 1.05)
  
  # Calculate shared y-axis limits for cost plots
  all_costs <- c(
    no_controls_result$mitig_costs_annual + no_controls_result$remov_costs_annual + no_controls_result$temp_costs_annual,
    mitigation_only_result$mitig_costs_annual + mitigation_only_result$remov_costs_annual + mitigation_only_result$temp_costs_annual,
    full_controls_result$mitig_costs_annual + full_controls_result$remov_costs_annual + full_controls_result$temp_costs_annual
  )
  cost_y_limits <- c(0, max(all_costs, na.rm = TRUE) * 1.05)
  
  # === TOP ROW: TEMPERATURE PLOTS ===
  
  # Panel 1: No Controls Temperature
  temp_1_data <- data.frame(
    Year = no_controls_result$years,
    Temperature = no_controls_result$temperature_anomaly
  )
  
  p1_temp <- ggplot(temp_1_data, aes(x = Year, y = Temperature)) +
    geom_hline(yintercept = 1.5, linetype = "dashed", color = "red", alpha = 0.7, linewidth = 0.8) +
    geom_line(color = colorspace::darken("steelblue", 0.3), linewidth = 1) +
    scale_y_continuous(limits = temp_y_limits) +
    labs(
      title = "A) No Controls",
      x = "",  # Remove x-axis label for top row
      y = "Temperature Anomaly (°C)"
    ) +
    my_theme_no_legend
  
  # Panel 2: Mitigation Only Temperature
  temp_2_data <- data.frame(
    Year = mitigation_only_result$years,
    Temperature = mitigation_only_result$temperature_anomaly
  )
  
  p2_temp <- ggplot(temp_2_data, aes(x = Year, y = Temperature)) +
    geom_hline(yintercept = 1.5, linetype = "dashed", color = "red", alpha = 0.7, linewidth = 0.8) +
    geom_line(color = colorspace::darken("steelblue", 0.3), linewidth = 1) +
    scale_y_continuous(limits = temp_y_limits) +
    labs(
      title = "B) Mitigation Only",
      x = "",  # Remove x-axis label for top row
      y = "Temperature Anomaly (°C)"
    ) +
    my_theme_no_legend
  
  # Panel 3: Full Controls Temperature
  temp_3_data <- data.frame(
    Year = full_controls_result$years,
    Temperature = full_controls_result$temperature_anomaly
  )
  
  p3_temp <- ggplot(temp_3_data, aes(x = Year, y = Temperature)) +
    geom_hline(yintercept = 1.5, linetype = "dashed", color = "red", alpha = 0.7, linewidth = 0.8) +
    geom_line(color = colorspace::darken("steelblue", 0.3), linewidth = 1) +
    scale_y_continuous(limits = temp_y_limits) +
    labs(
      title = "C) Full Controls",
      x = "",  # Remove x-axis label for top row
      y = "Temperature Anomaly (°C)"
    ) +
    my_theme_no_legend
  
  # === MIDDLE ROW: CONTROL STRATEGY PLOTS ===
  
  # Panel 4: No Controls Strategy
  control_1_data <- data.frame(
    Year = no_controls_result$years,
    Baseline = no_controls_result$baseline_annual_emissions,
    Mitigation = no_controls_result$qty_mitig,
    CDR = no_controls_result$qty_remov
  )
  
  p4_control <- ggplot(control_1_data, aes(x = Year)) +
    geom_line(aes(y = Baseline, color = "Baseline"), linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = Mitigation, color = "Mitigation"), linewidth = 1) +
    geom_line(aes(y = CDR, color = "CDR"), linewidth = 1) +
    scale_color_manual(values = c(
      "Baseline" = "black", 
      "Mitigation" = colorspace::darken("forestgreen", 0.3), 
      "CDR" = colorspace::darken("purple4", 0.3)
    )) +
    scale_y_continuous(limits = control_y_limits) +
    labs(
      title = "",  # No title for middle row
      x = "",      # Remove x-axis label for middle row
      y = "GtCO2/year",
      color = ""  # Remove legend title
    ) +
    my_theme_with_legend
  
  # Panel 5: Mitigation Only Strategy (legend hidden to avoid duplication)
  control_2_data <- data.frame(
    Year = mitigation_only_result$years,
    Baseline = mitigation_only_result$baseline_annual_emissions,
    Mitigation = mitigation_only_result$qty_mitig,
    CDR = mitigation_only_result$qty_remov
  )
  
  p5_control <- ggplot(control_2_data, aes(x = Year)) +
    geom_line(aes(y = Baseline, color = "Baseline"), linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = Mitigation, color = "Mitigation"), linewidth = 1) +
    geom_line(aes(y = CDR, color = "CDR"), linewidth = 1) +
    scale_color_manual(values = c(
      "Baseline" = "black", 
      "Mitigation" = colorspace::darken("forestgreen", 0.3), 
      "CDR" = colorspace::darken("purple4", 0.3)
    )) +
    scale_y_continuous(limits = control_y_limits) +
    labs(
      title = "",  # No title for middle row
      x = "",      # Remove x-axis label for middle row
      y = "GtCO2/year",
      color = ""
    ) +
    my_theme_no_legend  # Hide legend for middle plot
  
  # Panel 6: Full Controls Strategy (legend hidden to avoid duplication)
  control_3_data <- data.frame(
    Year = full_controls_result$years,
    Baseline = full_controls_result$baseline_annual_emissions,
    Mitigation = full_controls_result$qty_mitig,
    CDR = full_controls_result$qty_remov
  )
  
  p6_control <- ggplot(control_3_data, aes(x = Year)) +
    geom_line(aes(y = Baseline, color = "Baseline"), linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = Mitigation, color = "Mitigation"), linewidth = 1) +
    geom_line(aes(y = CDR, color = "CDR"), linewidth = 1) +
    scale_color_manual(values = c(
      "Baseline" = "black", 
      "Mitigation" = colorspace::darken("forestgreen", 0.3), 
      "CDR" = colorspace::darken("purple4", 0.3)
    )) +
    scale_y_continuous(limits = control_y_limits) +
    labs(
      title = "",  # No title for middle row
      x = "",      # Remove x-axis label for middle row
      y = "GtCO2/year",
      color = ""
    ) +
    my_theme_no_legend  # Hide legend for right plot
  
  # === BOTTOM ROW: ANNUAL COSTS PLOTS ===
  
  # Panel 7: No Controls Costs
  cost_1_data <- data.frame(
    Year = no_controls_result$years,
    Mitigation = no_controls_result$mitig_costs_annual,
    Removal = no_controls_result$remov_costs_annual,
    Residual = no_controls_result$temp_costs_annual
  ) %>%
    pivot_longer(cols = c(Mitigation, Removal, Residual),
                 names_to = "cost_type",
                 values_to = "cost_value")
  
  p7_cost <- ggplot(cost_1_data, aes(x = Year, y = cost_value, fill = cost_type)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_manual(values = c(
      "Mitigation" = "#E69F00",    # Orange
      "Removal" = "#009E73",       # Green  
      "Residual" = "#CC79A7"       # Pink
    )) +
    scale_y_continuous(limits = cost_y_limits) +
    labs(
      title = "",  # No title for bottom row
      x = "Year",
      y = "Annual Costs ($ Trillion)",
      fill = ""  # Remove legend title
    ) +
    my_theme_with_legend
  
  # Panel 8: Mitigation Only Costs (legend hidden to avoid duplication)
  cost_2_data <- data.frame(
    Year = mitigation_only_result$years,
    Mitigation = mitigation_only_result$mitig_costs_annual,
    Removal = mitigation_only_result$remov_costs_annual,
    Residual = mitigation_only_result$temp_costs_annual
  ) %>%
    pivot_longer(cols = c(Mitigation, Removal, Residual),
                 names_to = "cost_type",
                 values_to = "cost_value")
  
  p8_cost <- ggplot(cost_2_data, aes(x = Year, y = cost_value, fill = cost_type)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_manual(values = c(
      "Mitigation" = "#E69F00",    # Orange
      "Removal" = "#009E73",       # Green  
      "Residual" = "#CC79A7"       # Pink
    )) +
    scale_y_continuous(limits = cost_y_limits) +
    labs(
      title = "",  # No title for bottom row
      x = "Year",
      y = "Annual Costs ($ Trillion)",
      fill = ""
    ) +
    my_theme_no_legend  # Hide legend for middle plot
  
  # Panel 9: Full Controls Costs (legend hidden to avoid duplication)
  cost_3_data <- data.frame(
    Year = full_controls_result$years,
    Mitigation = full_controls_result$mitig_costs_annual,
    Removal = full_controls_result$remov_costs_annual,
    Residual = full_controls_result$temp_costs_annual
  ) %>%
    pivot_longer(cols = c(Mitigation, Removal, Residual),
                 names_to = "cost_type",
                 values_to = "cost_value")
  
  p9_cost <- ggplot(cost_3_data, aes(x = Year, y = cost_value, fill = cost_type)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_manual(values = c(
      "Mitigation" = "#E69F00",    # Orange
      "Removal" = "#009E73",       # Green  
      "Residual" = "#CC79A7"       # Pink
    )) +
    scale_y_continuous(limits = cost_y_limits) +
    labs(
      title = "",  # No title for bottom row
      x = "Year",
      y = "Annual Costs ($ Trillion)",
      fill = ""
    ) +
    my_theme_no_legend  # Hide legend for right plot
  
  # Combine plots in 3x3 layout using patchwork
  combined_plot <- (p1_temp + p2_temp + p3_temp) / 
    (p4_control + p5_control + p6_control) /
    (p7_cost + p8_cost + p9_cost)
  
  # Add overall title
  final_plot <- combined_plot + 
    plot_annotation(
      title = paste0("Temperature Trajectories, Control Strategies, and Annual Costs: ", scenario_name),
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.margin = margin(15, 10, 10, 10)
      )
    )
  
  # Save if requested (A4 portrait dimensions for better 3x3 layout)
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0("control_comparison_", scenario_name, "_", 
                         format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    }
    
    filepath <- here::here("figs", filename)
    
    # A4 portrait: 210mm x 297mm (better for 3x3 layout)
    ggsave(
      filename = filepath,
      plot = final_plot,
      width = 210,
      height = 297,
      units = "mm",
      device = cairo_pdf
    )
    
    cat("Control comparison plot saved to:", filepath, "\n")
  }
  
  return(final_plot)
}


#' @title Updated Phase Diagram Functions for Optimal Control Analysis
#' @description
#' Creates phase diagrams showing how different interventions affect the trajectory
#' in emissions vs cumulative emissions phase space. Updated to remove dashed lines
#' and axis scales while keeping axis labels.
#'
#' @author Nina Rynne
#' @date July 2025

# Load required libraries for phase diagrams
library(ggplot2)
library(dplyr)
library(patchwork)

#' @title Create Temperature Anomaly Plot for No Controls Scenario
#' @description Shows temperature anomaly over time (no interventions)
#' @param no_controls_result Solution from no controls scenario
#' @param title Plot title
#' @return ggplot object
create_no_controls_temperature_plot <- function(no_controls_result, 
                                                title = "A) No Interventions") {
  
  # Extract temperature data
  temp_data <- data.frame(
    years = no_controls_result$years,
    temperature = no_controls_result$temperature_anomaly
  )
  
  # Define consistent theme with removed axis scales
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_blank(),  # Remove axis text/values
      axis.ticks = element_blank(), # Remove axis ticks
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      aspect.ratio = 1  # Make plot roughly square
    )
  
  # Create temperature plot
  p <- ggplot(temp_data, aes(x = years, y = temperature)) +
    geom_line(color = "red", linewidth = 1.2, alpha = 0.8) +
    labs(
      title = title,
      x = "Year",
      y = "Temperature Anomaly (°C)"
    ) +
    my_theme
  
  return(p)
}

#' @title Create Temperature Anomaly Plot for Mitigation Only Scenario
#' @description Shows temperature anomaly over time after mitigation
#' @param mitigation_only_result Solution from mitigation only scenario
#' @param title Plot title
#' @return ggplot object
create_mitigation_temperature_plot <- function(mitigation_only_result, 
                                               title = "B) Mitigation Only") {
  
  # Extract temperature data
  temp_data <- data.frame(
    years = mitigation_only_result$years,
    temperature = mitigation_only_result$temperature_anomaly
  )
  
  # Define consistent theme with removed axis scales
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_blank(),  # Remove axis text/values
      axis.ticks = element_blank(), # Remove axis ticks
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      aspect.ratio = 1  # Make plot roughly square
    )
  
  # Create temperature plot
  p <- ggplot(temp_data, aes(x = years, y = temperature)) +
    geom_line(color = colorspace::darken("forestgreen", 0.3), linewidth = 1.2, alpha = 0.8) +
    labs(
      title = title,
      x = "Year",
      y = "Temperature Anomaly (°C)"
    ) +
    my_theme
  
  return(p)
}



#' @title Create Temperature Anomaly Plot for Full Model
#' @description Shows temperature anomaly over time after both controls
#' @param full_controls_result Solution from full controls scenario
#' @param title Plot title
#' @return ggplot object
create_full_model_temperature_plot <- function(full_controls_result, 
                                               title = "C) Full Model (Mitigation + CDR)") {
  
  # Extract temperature data
  temp_data <- data.frame(
    years = full_controls_result$years,
    temperature = full_controls_result$temperature_anomaly
  )
  
  # Define consistent theme with removed axis scales
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_blank(),  # Remove axis text/values
      axis.ticks = element_blank(), # Remove axis ticks
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      aspect.ratio = 1  # Make plot roughly square
    )
  
  # Create temperature plot
  p <- ggplot(temp_data, aes(x = years, y = temperature)) +
    geom_line(color = "steelblue", linewidth = 1.2, alpha = 0.8) +
    labs(
      title = title,
      x = "Year",
      y = "Temperature Anomaly (°C)"
    ) +
    my_theme
  
  return(p)
}

#' @title Create Temperature Dashboard
#' @description Creates 1×3 dashboard showing how different interventions 
#' affect temperature anomaly trajectories over time
#' @param no_controls_result Solution from no controls scenario
#' @param mitigation_only_result Solution from mitigation only scenario  
#' @param full_controls_result Solution from full controls scenario
#' @param scenario_name Name of baseline scenario
#' @param save_plot Whether to save the plot
#' @param filename Custom filename for saving
#' @return Combined ggplot object
create_temperature_dashboard <- function(no_controls_result,
                                         mitigation_only_result,
                                         full_controls_result,
                                         scenario_name = "Unknown",
                                         save_plot = FALSE,
                                         filename = NULL) {
  
  # Create individual temperature plots (only 3 now)
  p1 <- create_no_controls_temperature_plot(no_controls_result)
  p2 <- create_mitigation_temperature_plot(mitigation_only_result)
  p3 <- create_full_model_temperature_plot(full_controls_result)
  
  # Combine in 1×3 layout (single row)
  combined <- p1 + p2 + p3
  
  # Add overall title and subtitle
  final_plot <- combined + 
    plot_annotation(
      title = paste0("Temperature Trajectories: How Interventions Affect Climate Outcomes (", scenario_name, ")"),
      subtitle = "Temperature anomaly over time under different intervention scenarios",
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5),
        plot.margin = margin(15, 10, 10, 10)
      )
    )
  
  # Save if requested (adjusted dimensions for 1×3 layout)
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0("temperature_dashboard_", scenario_name, "_", 
                         format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    }
    
    filepath <- here::here("figs", filename)
    # Wide landscape format for 1×3 layout with roughly square plots
    ggsave(filepath, final_plot, width = 210, height = 100, units = "mm", device = cairo_pdf)
    cat("Temperature dashboard saved to:", filepath, "\n")
  }
  
  return(final_plot)
}

#' @title Create Multi-Scenario Delay Heatmap Dashboard
#' @description
#' Creates a 5×2 dashboard showing peak temperature and years above 1.5°C heatmaps
#' for all SSP scenarios from multi-scenario delayed deployment analysis.
#' 
#' @param multi_scenario_results Results from run_multi_scenario_delayed_deployment()
#' @param save_plot Whether to save the plot (default: FALSE)
#' @param filename Custom filename for saving
#' @param add_contours Whether to add contour lines (default: TRUE)
#' @param contour_alpha Transparency of contour lines (default: 0.6)
#' @param show_infeasible Whether to mark infeasible combinations with X (default: TRUE)
#'
#' @return Combined ggplot object with 5×2 layout
#'
#' @examples
#' # Create multi-scenario dashboard
#' dashboard <- create_multi_scenario_delay_dashboard(
#'   multi_scenario_results = multi_results,
#'   save_plot = FALSE,
#'   add_contours = TRUE
#' )
#' print(dashboard)
#'
#' # Save dashboard
#' create_multi_scenario_delay_dashboard(
#'   multi_scenario_results = multi_results,
#'   save_plot = TRUE,
#'   filename = "multi_scenario_delay_analysis.pdf"
#' )
create_multi_scenario_delay_dashboard <- function(multi_scenario_results,
                                                  save_plot = FALSE,
                                                  filename = NULL,
                                                  add_contours = TRUE,
                                                  contour_alpha = 0.6,
                                                  show_infeasible = TRUE) {
  
  # Extract combined results
  combined_data <- multi_scenario_results$combined_results
  
  if (is.null(combined_data) || nrow(combined_data) == 0) {
    stop("No valid data found in multi_scenario_results")
  }
  
  # Check required columns
  required_cols <- c("scenario", "scenario_short", "mitigation_delay", "cdr_delay", 
                     "peak_temperature", "years_above_1p5", "feasible")
  missing_cols <- setdiff(required_cols, names(combined_data))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Define SSP scenarios in order
  ssp_order <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
  
  # Filter and order data
  plot_data <- combined_data %>%
    filter(!is.na(peak_temperature), !is.na(years_above_1p5)) %>%
    filter(scenario_short %in% ssp_order) %>%
    mutate(scenario_short = factor(scenario_short, levels = ssp_order)) %>%
    arrange(scenario_short, mitigation_delay, cdr_delay)
  
  if (nrow(plot_data) == 0) {
    stop("No valid data after filtering")
  }
  
  # Calculate consistent scales across all scenarios
  temp_limits <- range(plot_data$peak_temperature, na.rm = TRUE)
  years_limits <- range(plot_data$years_above_1p5, na.rm = TRUE)
  
  # Define consistent theme for all plots
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 8),
      plot.title = element_text(size = 9, hjust = 0.5),
      axis.title = element_text(size = 7),
      axis.text = element_text(size = 6),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6),
      legend.key.size = unit(0.3, "cm"),
      plot.margin = margin(2, 2, 2, 2)
    )
  
  # Create function to generate peak temperature heatmap for one scenario
  create_scenario_temp_heatmap <- function(scenario_name, show_legend = FALSE, is_top_row = FALSE) {
    scenario_data <- plot_data %>% filter(scenario_short == scenario_name)
    
    if (nrow(scenario_data) == 0) {
      # Return empty plot if no data
      return(ggplot() + 
               theme_void() + 
               labs(title = paste(scenario_name, "- No Data")))
    }
    
    # Base plot
    p <- ggplot(scenario_data, aes(x = mitigation_delay, y = cdr_delay, fill = peak_temperature)) +
      geom_tile() +
      scale_fill_viridis_c(
        name = "Peak\nTemp\n(°C)",
        option = "plasma",
        direction = -1,
        limits = temp_limits
      ) +
      labs(
        title = if (is_top_row) "Peak Temperature" else "",
        x = if (scenario_name == "SSP5") "Mitigation Delay (years)" else "",
        y = "CDR Delay (years)"
      ) +
      coord_equal() +
      my_theme
    
    # Add contours if requested
    if (add_contours) {
      p <- p + 
        geom_contour(aes(z = peak_temperature), 
                     color = "white", 
                     alpha = contour_alpha, 
                     linewidth = 0.3)
    }
    
    # Add infeasible markers if requested
    if (show_infeasible) {
      infeasible_data <- scenario_data %>% filter(!feasible)
      if (nrow(infeasible_data) > 0) {
        p <- p + 
          geom_point(data = infeasible_data, 
                     aes(x = mitigation_delay, y = cdr_delay), 
                     shape = 4, size = 1, color = "red", alpha = 0.8, 
                     stroke = 0.8, inherit.aes = FALSE)
      }
    }
    
    # Remove legend for all plots except one
    if (!show_legend) {
      p <- p + theme(legend.position = "none")
    }
    
    return(p)
  }
  
  # Create function to generate years above 1.5°C heatmap for one scenario
  create_scenario_years_heatmap <- function(scenario_name, show_legend = FALSE, is_top_row = FALSE) {
    scenario_data <- plot_data %>% filter(scenario_short == scenario_name)
    
    if (nrow(scenario_data) == 0) {
      # Return empty plot if no data
      return(ggplot() + 
               theme_void() + 
               labs(title = paste(scenario_name, "- No Data")))
    }
    
    # Base plot
    p <- ggplot(scenario_data, aes(x = mitigation_delay, y = cdr_delay, fill = years_above_1p5)) +
      geom_tile() +
      scale_fill_viridis_c(
        name = "Years\nAbove\n1.5°C",
        option = "plasma",
        direction = -1,
        limits = years_limits
      ) +
      labs(
        title = if (is_top_row) "Years Above 1.5°C" else "",
        x = if (scenario_name == "SSP5") "Mitigation Delay (years)" else "",
        y = ""  # No y-axis label for right column
      ) +
      coord_equal() +
      my_theme
    
    # Add contours if requested
    if (add_contours) {
      # Calculate appropriate contour breaks for years above 1.5°C
      years_range <- range(scenario_data$years_above_1p5, na.rm = TRUE)
      contour_breaks <- seq(ceiling(years_range[1]), floor(years_range[2]), by = 1)
      
      p <- p + 
        geom_contour(aes(z = years_above_1p5), 
                     breaks = contour_breaks,
                     color = "white", 
                     alpha = contour_alpha, 
                     linewidth = 0.3)
    }
    
    # Add infeasible markers if requested
    if (show_infeasible) {
      infeasible_data <- scenario_data %>% filter(!feasible)
      if (nrow(infeasible_data) > 0) {
        p <- p + 
          geom_point(data = infeasible_data, 
                     aes(x = mitigation_delay, y = cdr_delay), 
                     shape = 4, size = 1, color = "red", alpha = 0.8, 
                     stroke = 0.8, inherit.aes = FALSE)
      }
    }
    
    # Remove legend for all plots except one
    if (!show_legend) {
      p <- p + theme(legend.position = "none")
    }
    
    return(p)
  }
  
  # Create all plots
  available_scenarios <- levels(plot_data$scenario_short)
  
  # Temperature plots (left column) - only first one gets column header
  temp_plots <- map(seq_along(available_scenarios), function(i) {
    scenario <- available_scenarios[i]
    create_scenario_temp_heatmap(scenario, show_legend = FALSE, is_top_row = (i == 1))
  })
  
  # Years above 1.5°C plots (right column) - only first one gets column header  
  years_plots <- map(seq_along(available_scenarios), function(i) {
    scenario <- available_scenarios[i]
    create_scenario_years_heatmap(scenario, show_legend = FALSE, is_top_row = (i == 1))
  })
  
  # Create shared legends
  temp_legend_plot <- create_scenario_temp_heatmap(available_scenarios[1], show_legend = TRUE)
  temp_legend_extracted <- cowplot::get_legend(temp_legend_plot)
  
  years_legend_plot <- create_scenario_years_heatmap(available_scenarios[1], show_legend = TRUE)
  years_legend_extracted <- cowplot::get_legend(years_legend_plot)
  
  # Add SSP labels to the y-axis of temperature plots
  for (i in seq_along(available_scenarios)) {
    temp_plots[[i]] <- temp_plots[[i]] + 
      labs(y = paste(available_scenarios[i], "\nCDR Delay (years)"))
  }
  
  # Combine plots in simple 5×2 layout with legends on the right
  if (length(available_scenarios) == 5) {
    # Create the main 5×2 plot grid
    main_plots <- (temp_plots[[1]] + years_plots[[1]]) /
      (temp_plots[[2]] + years_plots[[2]]) /
      (temp_plots[[3]] + years_plots[[3]]) /
      (temp_plots[[4]] + years_plots[[4]]) /
      (temp_plots[[5]] + years_plots[[5]])
    
    # Combine legends vertically
    legends_combined <- wrap_elements(temp_legend_extracted) / 
      wrap_elements(years_legend_extracted) /
      plot_spacer() /
      plot_spacer() /
      plot_spacer()
    
    # Combine main plots with legends
    combined_plot <- main_plots | legends_combined
    
    # Set relative widths: wide for main plots, narrow for legends
    combined_plot <- combined_plot + plot_layout(widths = c(10, 1))
    
  } else {
    # Handle fewer than 5 scenarios
    plot_pairs <- map(seq_along(available_scenarios), function(i) {
      temp_plots[[i]] + years_plots[[i]]
    })
    
    main_plots <- wrap_plots(plot_pairs, ncol = 1)
    
    # Create legends for fewer scenarios
    legends_needed <- min(2, length(available_scenarios))
    legend_list <- list()
    if (legends_needed >= 1) legend_list[[1]] <- wrap_elements(temp_legend_extracted)
    if (legends_needed >= 2) legend_list[[2]] <- wrap_elements(years_legend_extracted)
    
    # Add spacers for remaining rows
    remaining_rows <- length(available_scenarios) - legends_needed
    for (i in seq_len(remaining_rows)) {
      legend_list[[length(legend_list) + 1]] <- plot_spacer()
    }
    
    legends_combined <- wrap_plots(legend_list, ncol = 1)
    
    combined_plot <- main_plots | legends_combined + plot_layout(widths = c(10, 1))
  }
  
  # Add overall title and subtitle
  n_scenarios <- length(available_scenarios)
  n_total_combinations <- nrow(plot_data)
  n_feasible <- sum(plot_data$feasible, na.rm = TRUE)
  
  final_plot <- combined_plot + 
    plot_annotation(
      title = "Multi-Scenario Delayed Deployment Analysis",
      subtitle = paste0("Peak Temperature and Years Above 1.5°C | ", n_scenarios, " scenarios, ", 
                        n_total_combinations, " combinations, ", n_feasible, " feasible"),
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.margin = margin(10, 5, 5, 5)
      )
    )
  
  # Save if requested (A4 portrait dimensions)
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0("multi_scenario_delay_dashboard_", 
                         format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    }
    
    filepath <- here::here("figs", filename)
    
    # A4 portrait: 210mm x 297mm
    ggsave(
      filename = filepath,
      plot = final_plot,
      width = 210,
      height = 297,
      units = "mm",
      device = cairo_pdf
    )
    
    cat("Multi-scenario delay dashboard saved to:", filepath, "\n")
  }
  
  return(final_plot)
}

#' @title Create Multi-Scenario Peak Temperature Heatmap Dashboard
#' @description
#' Creates a 3×2 dashboard showing peak temperature heatmaps for SSP scenarios 1-5
#' with consistent styling and a shared legend. Each plot is labeled A) through F).
#' Based on the working create_multi_scenario_delay_dashboard pattern.
#' 
#' @param multi_scenario_results Results from run_multi_scenario_delayed_deployment()
#' @param save_plot Whether to save the plot (default: FALSE)
#' @param filename Custom filename for saving
#' @param add_contours Whether to add contour lines (default: TRUE)
#' @param contour_alpha Transparency of contour lines (default: 0.6)
#' @param show_infeasible Whether to mark infeasible combinations with X (default: TRUE)
#'
#' @return Combined ggplot object with 3×2 layout
create_multi_scenario_delay_peaktemp <- function(multi_scenario_results,
                                                 save_plot = FALSE,
                                                 filename = NULL,
                                                 add_contours = TRUE,
                                                 contour_alpha = 0.6,
                                                 show_infeasible = TRUE) {
  
  # Load required libraries
  if (!require(patchwork, quietly = TRUE)) stop("patchwork package required")
  if (!require(viridis, quietly = TRUE)) stop("viridis package required")
  if (!require(cowplot, quietly = TRUE)) stop("cowplot package required for legend extraction")
  
  # Extract combined results
  combined_data <- multi_scenario_results$combined_results
  
  if (is.null(combined_data) || nrow(combined_data) == 0) {
    stop("No valid data found in multi_scenario_results")
  }
  
  # Check required columns
  required_cols <- c("scenario", "scenario_short", "mitigation_delay", "cdr_delay", 
                     "peak_temperature", "feasible")
  missing_cols <- setdiff(required_cols, names(combined_data))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Define SSP scenarios in order (1-5)
  ssp_order <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
  
  # Filter and order data
  plot_data <- combined_data %>%
    filter(!is.na(peak_temperature)) %>%
    filter(scenario_short %in% ssp_order) %>%
    mutate(scenario_short = factor(scenario_short, levels = ssp_order)) %>%
    arrange(scenario_short, mitigation_delay, cdr_delay)
  
  if (nrow(plot_data) == 0) {
    stop("No valid peak temperature data after filtering")
  }
  
  # Calculate consistent scale across all scenarios
  temp_limits <- range(plot_data$peak_temperature, na.rm = TRUE)
  
  # Define consistent theme for all plots (matching the working dashboard)
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.position = "none"
    )
  
  # Create function to generate peak temperature heatmap for one scenario
  create_scenario_temp_heatmap <- function(scenario_name, plot_title) {
    scenario_data <- plot_data %>% filter(scenario_short == scenario_name)
    
    if (nrow(scenario_data) == 0) {
      # Return empty plot if no data
      return(ggplot() + 
               theme_void() + 
               labs(title = plot_title) +
               theme(
                 plot.title = element_text(size = 10, hjust = 0.5),
                 panel.background = element_rect(fill = "white", color = NA)
               ))
    }
    
    # Base plot
    p <- ggplot(scenario_data, aes(x = mitigation_delay, y = cdr_delay, fill = peak_temperature)) +
      geom_tile() +
      scale_fill_viridis_c(
        name = "Peak\nTemp\n(°C)",
        option = "plasma",
        direction = -1,
        limits = temp_limits
      ) +
      labs(
        title = plot_title,
        x = "Mitigation Deployment Delay (years)",
        y = "CDR Deployment Delay (years)"
      ) +
      coord_equal() +
      my_theme
    
    # Add contours if requested
    if (add_contours) {
      p <- p + 
        geom_contour(aes(z = peak_temperature), 
                     color = "white", 
                     alpha = contour_alpha, 
                     linewidth = 0.5)
    }
    
    # Add infeasible markers if requested
    if (show_infeasible) {
      infeasible_data <- scenario_data %>% filter(!feasible)
      if (nrow(infeasible_data) > 0) {
        p <- p + 
          geom_point(data = infeasible_data, 
                     aes(x = mitigation_delay, y = cdr_delay), 
                     shape = 4, size = 2, color = "red", alpha = 0.8, 
                     stroke = 1.5, inherit.aes = FALSE)
      }
    }
    
    return(p)
  }
  
  # Get available scenarios (up to 5)
  available_scenarios <- intersect(ssp_order, unique(plot_data$scenario_short))
  
  # Create individual plots with proper titles
  p1 <- create_scenario_temp_heatmap("SSP1", "A) SSP1")
  p2 <- create_scenario_temp_heatmap("SSP2", "B) SSP2") 
  p3 <- create_scenario_temp_heatmap("SSP3", "C) SSP3")
  p4 <- create_scenario_temp_heatmap("SSP4", "D) SSP4")
  p5 <- create_scenario_temp_heatmap("SSP5", "E) SSP5")
  
  # Create blank plot for F) position - no title
  p6 <- ggplot() + 
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA))
  
  # Create shared legend using the working dashboard approach
  temp_legend_plot <- create_scenario_temp_heatmap("SSP1", "Legend") + 
    theme(legend.position = "right")
  temp_legend_extracted <- cowplot::get_legend(temp_legend_plot)
  
  # Create the main 3×2 plot grid (following the working pattern)
  main_plots <- (p1 + p2 + p3) / (p4 + p5 + p6)
  
  # Combine main plots with legend using the same approach as working dashboard
  combined_plot <- main_plots | temp_legend_extracted
  
  # Set relative widths: wide for main plots, narrow for legend
  combined_plot <- combined_plot + plot_layout(widths = c(10, 1))
  
  # Count scenarios and feasible combinations for subtitle
  n_scenarios <- length(available_scenarios)
  n_total_combinations <- nrow(plot_data)
  n_feasible <- sum(plot_data$feasible, na.rm = TRUE)
  
  # Add overall title and subtitle (matching working dashboard style)
  final_plot <- combined_plot + 
    plot_annotation(
      title = "Peak Temperature Analysis: Multi-Scenario Delayed Deployment",
      subtitle = paste0("SSP scenarios showing peak temperature vs deployment delays | ", 
                        n_scenarios, " scenarios, ", n_total_combinations, " combinations, ", 
                        n_feasible, " feasible"),
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.margin = margin(10, 0, 10, 0)
      )
    )
  
  # Save if requested (using script conventions)
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0("multi_scenario_peaktemp_dashboard_", 
                         format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    }
    
    filepath <- here::here("figs", filename)
    
    # 3×2 layout dimensions (wider than tall)
    ggsave(
      filename = filepath,
      plot = final_plot,
      width = 297,  # A4 landscape width
      height = 210, # A4 landscape height
      units = "mm",
      device = cairo_pdf
    )
    
    cat("Multi-scenario peak temperature dashboard saved to:", filepath, "\n")
  }
  
  return(final_plot)
}

#' @title Create Multi-Scenario Years Above 1.5°C Heatmap Dashboard (Reduced Contours)
#' @description
#' Creates a 3×2 dashboard showing years above 1.5°C heatmaps for SSP scenarios 1-5
#' with consistent styling and a shared legend. Each plot is labeled A) through E).
#' Uses viridis colour scale for years above 1.5°C data with reduced contour lines.
#' 
#' @param multi_scenario_results Results from run_multi_scenario_delayed_deployment()
#' @param save_plot Whether to save the plot (default: FALSE)
#' @param filename Custom filename for saving
#' @param add_contours Whether to add contour lines (default: TRUE)
#' @param contour_alpha Transparency of contour lines (default: 0.6)
#' @param show_infeasible Whether to mark infeasible combinations with X (default: TRUE)
#' @param contour_interval Interval between contour lines in years (default: 2)
#'
#' @return Combined ggplot object with 3×2 layout
#'
#' @examples
#' # Create years above 1.5°C dashboard with reduced contours
#' dashboard <- create_multi_scenario_delay_years1p5(
#'   multi_scenario_results = multi_results,
#'   save_plot = FALSE,
#'   add_contours = TRUE,
#'   contour_interval = 2  # Every 2 years instead of every year
#' )
#' print(dashboard)
#'
#' # Save dashboard with even fewer contours
#' create_multi_scenario_delay_years1p5(
#'   multi_scenario_results = multi_results,
#'   save_plot = TRUE,
#'   filename = "years_above_1p5_multi_scenario.pdf",
#'   contour_interval = 3  # Every 3 years for even cleaner look
#' )
create_multi_scenario_delay_years1p5 <- function(multi_scenario_results,
                                                 save_plot = FALSE,
                                                 filename = NULL,
                                                 add_contours = TRUE,
                                                 contour_alpha = 0.6,
                                                 show_infeasible = TRUE,
                                                 contour_interval = 1) {
  
  # Load required libraries
  if (!require(patchwork, quietly = TRUE)) stop("patchwork package required")
  if (!require(viridis, quietly = TRUE)) stop("viridis package required")
  if (!require(cowplot, quietly = TRUE)) stop("cowplot package required for legend extraction")
  
  # Extract combined results
  combined_data <- multi_scenario_results$combined_results
  
  if (is.null(combined_data) || nrow(combined_data) == 0) {
    stop("No valid data found in multi_scenario_results")
  }
  
  # Check required columns
  required_cols <- c("scenario", "scenario_short", "mitigation_delay", "cdr_delay", 
                     "years_above_1p5", "feasible")
  missing_cols <- setdiff(required_cols, names(combined_data))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Define SSP scenarios in order (1-5)
  ssp_order <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
  
  # Filter and order data
  plot_data <- combined_data %>%
    filter(!is.na(years_above_1p5)) %>%
    filter(scenario_short %in% ssp_order) %>%
    mutate(scenario_short = factor(scenario_short, levels = ssp_order)) %>%
    arrange(scenario_short, mitigation_delay, cdr_delay)
  
  if (nrow(plot_data) == 0) {
    stop("No valid years above 1.5°C data after filtering")
  }
  
  # Calculate consistent scale across all scenarios
  years_limits <- range(plot_data$years_above_1p5, na.rm = TRUE)
  
  # Define consistent theme for all plots
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.position = "none"
    )
  
  # Create function to generate years above 1.5°C heatmap for one scenario
  create_scenario_years_heatmap <- function(scenario_name, plot_title) {
    scenario_data <- plot_data %>% filter(scenario_short == scenario_name)
    
    if (nrow(scenario_data) == 0) {
      # Return empty plot if no data
      return(ggplot() + 
               theme_void() + 
               labs(title = plot_title) +
               theme(
                 plot.title = element_text(size = 10, hjust = 0.5),
                 panel.background = element_rect(fill = "white", color = NA)
               ))
    }
    
    # Base plot
    p <- ggplot(scenario_data, aes(x = mitigation_delay, y = cdr_delay, fill = years_above_1p5)) +
      geom_tile() +
      scale_fill_viridis_c(
        name = "Years\nAbove\n1.5°C",
        option = "viridis",
        direction = -1,
        limits = years_limits
      ) +
      labs(
        title = plot_title,
        x = "Mitigation Deployment Delay (years)",
        y = "CDR Deployment Delay (years)"
      ) +
      coord_equal() +
      my_theme
    
    # Add contours if requested with reduced frequency
    if (add_contours) {
      # Calculate appropriate contour breaks with user-specified interval
      years_range <- range(scenario_data$years_above_1p5, na.rm = TRUE)
      
      # Round start point up to nearest contour_interval multiple
      start_contour <- ceiling(years_range[1] / contour_interval) * contour_interval
      # Round end point down to nearest contour_interval multiple  
      end_contour <- floor(years_range[2] / contour_interval) * contour_interval
      
      contour_breaks <- seq(start_contour, end_contour, by = contour_interval)
      
      # Only add contours if we have meaningful breaks
      if (length(contour_breaks) > 0 && max(contour_breaks) > min(contour_breaks)) {
        p <- p + 
          geom_contour(aes(z = years_above_1p5), 
                       breaks = contour_breaks,
                       color = "white", 
                       alpha = contour_alpha, 
                       linewidth = 0.5)
      }
    }
    
    # Add infeasible markers if requested
    if (show_infeasible) {
      infeasible_data <- scenario_data %>% filter(!feasible)
      if (nrow(infeasible_data) > 0) {
        p <- p + 
          geom_point(data = infeasible_data, 
                     aes(x = mitigation_delay, y = cdr_delay), 
                     shape = 4, size = 2, color = "red", alpha = 0.8, 
                     stroke = 1.5, inherit.aes = FALSE)
      }
    }
    
    return(p)
  }
  
  # Get available scenarios (up to 5)
  available_scenarios <- intersect(ssp_order, unique(plot_data$scenario_short))
  
  # Create individual plots with proper titles
  p1 <- create_scenario_years_heatmap("SSP1", "A) SSP1")
  p2 <- create_scenario_years_heatmap("SSP2", "B) SSP2") 
  p3 <- create_scenario_years_heatmap("SSP3", "C) SSP3")
  p4 <- create_scenario_years_heatmap("SSP4", "D) SSP4")
  p5 <- create_scenario_years_heatmap("SSP5", "E) SSP5")
  
  # Create blank plot for F) position - no title
  p6 <- ggplot() + 
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA))
  
  # Create shared legend
  years_legend_plot <- create_scenario_years_heatmap("SSP1", "Legend") + 
    theme(legend.position = "right")
  years_legend_extracted <- cowplot::get_legend(years_legend_plot)
  
  # Create the main 3×2 plot grid
  main_plots <- (p1 + p2 + p3) / (p4 + p5 + p6)
  
  # Combine main plots with legend
  combined_plot <- main_plots | years_legend_extracted
  
  # Set relative widths: wide for main plots, narrow for legend
  combined_plot <- combined_plot + plot_layout(widths = c(10, 1))
  
  # Count scenarios and feasible combinations for subtitle
  n_scenarios <- length(available_scenarios)
  n_total_combinations <- nrow(plot_data)
  n_feasible <- sum(plot_data$feasible, na.rm = TRUE)
  
  # Add overall title and subtitle
  final_plot <- combined_plot + 
    plot_annotation(
      title = "Years Above 1.5°C Analysis: Multi-Scenario Delayed Deployment",
      subtitle = paste0("SSP scenarios showing years above 1.5°C vs deployment delays | ", 
                        n_scenarios, " scenarios, ", n_total_combinations, " combinations, ", 
                        n_feasible, " feasible"),
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.margin = margin(10, 0, 10, 0)
      )
    )
  
  # Save if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0("multi_scenario_years1p5_dashboard_", 
                         format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    }
    
    filepath <- here::here("figs", filename)
    
    # 3×2 layout dimensions (wider than tall)
    ggsave(
      filename = filepath,
      plot = final_plot,
      width = 297,  # A4 landscape width
      height = 210, # A4 landscape height
      units = "mm",
      device = cairo_pdf
    )
    
    cat("Multi-scenario years above 1.5°C dashboard saved to:", filepath, "\n")
  }
  
  return(final_plot)
}

#' @title Create Multi-Scenario CDR Proportion Heatmap Dashboard
#' @description
#' Creates a 3×2 dashboard showing CDR proportion heatmaps for SSP scenarios 1-5
#' with consistent styling and a shared legend. Each plot is labeled A) through E).
#' Uses cividis colour scale for CDR proportion data (calculated from total_cdr_units and total_mitig_units).
#' 
#' @param multi_scenario_results Results from run_multi_scenario_delayed_deployment()
#' @param save_plot Whether to save the plot (default: FALSE)
#' @param filename Custom filename for saving
#' @param add_contours Whether to add contour lines (default: TRUE)
#' @param contour_alpha Transparency of contour lines (default: 0.6)
#' @param show_infeasible Whether to mark infeasible combinations with X (default: TRUE)
#'
#' @return Combined ggplot object with 3×2 layout
#'
#' @examples
#' # Create CDR proportion dashboard
#' dashboard <- create_multi_scenario_delay_cdr_proportion(
#'   multi_scenario_results = multi_results,
#'   save_plot = FALSE,
#'   add_contours = TRUE
#' )
#' print(dashboard)
#'
#' # Save dashboard
#' create_multi_scenario_delay_cdr_proportion(
#'   multi_scenario_results = multi_results,
#'   save_plot = TRUE,
#'   filename = "cdr_proportion_multi_scenario.pdf"
#' )
create_multi_scenario_delay_cdr_proportion <- function(multi_scenario_results,
                                                       save_plot = FALSE,
                                                       filename = NULL,
                                                       add_contours = TRUE,
                                                       contour_alpha = 0.6,
                                                       show_infeasible = TRUE) {
  
  # Load required libraries
  if (!require(patchwork, quietly = TRUE)) stop("patchwork package required")
  if (!require(viridis, quietly = TRUE)) stop("viridis package required")
  if (!require(cowplot, quietly = TRUE)) stop("cowplot package required for legend extraction")
  
  # Extract combined results
  combined_data <- multi_scenario_results$combined_results
  
  if (is.null(combined_data) || nrow(combined_data) == 0) {
    stop("No valid data found in multi_scenario_results")
  }
  
  # Check required columns
  required_cols <- c("scenario", "scenario_short", "mitigation_delay", "cdr_delay", 
                     "total_cdr_units", "total_mitig_units", "feasible")
  missing_cols <- setdiff(required_cols, names(combined_data))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Define SSP scenarios in order (1-5)
  ssp_order <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
  
  # Filter, calculate CDR proportion, and order data
  plot_data <- combined_data %>%
    filter(!is.na(total_cdr_units), !is.na(total_mitig_units)) %>%
    filter(scenario_short %in% ssp_order) %>%
    mutate(
      # Calculate total abatement
      total_abatement = total_cdr_units + total_mitig_units,
      # Calculate CDR proportion (handle division by zero)
      cdr_proportion = ifelse(total_abatement > 0, total_cdr_units / total_abatement, 0),
      # Convert to percentage
      cdr_percentage = cdr_proportion * 100,
      scenario_short = factor(scenario_short, levels = ssp_order)
    ) %>%
    arrange(scenario_short, mitigation_delay, cdr_delay)
  
  if (nrow(plot_data) == 0) {
    stop("No valid CDR proportion data after filtering")
  }
  
  # Calculate consistent scale across all scenarios (0-100%)
  proportion_limits <- c(0, 100)  # Always use 0-100% for interpretability
  
  # Define consistent theme for all plots
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.position = "none"
    )
  
  # Create function to generate CDR proportion heatmap for one scenario
  create_scenario_cdr_heatmap <- function(scenario_name, plot_title) {
    scenario_data <- plot_data %>% filter(scenario_short == scenario_name)
    
    if (nrow(scenario_data) == 0) {
      # Return empty plot if no data
      return(ggplot() + 
               theme_void() + 
               labs(title = plot_title) +
               theme(
                 plot.title = element_text(size = 10, hjust = 0.5),
                 panel.background = element_rect(fill = "white", color = NA)
               ))
    }
    
    # Base plot
    p <- ggplot(scenario_data, aes(x = mitigation_delay, y = cdr_delay, fill = cdr_percentage)) +
      geom_tile() +
      scale_fill_viridis_c(
        name = "CDR\nProportion\n(%)",
        option = "cividis",
        direction = 1,  # Higher values (more CDR) = lighter/yellow
        limits = proportion_limits
      ) +
      labs(
        title = plot_title,
        x = "Mitigation Deployment Delay (years)",
        y = "CDR Deployment Delay (years)"
      ) +
      coord_equal() +
      my_theme
    
    # Add contours if requested
    if (add_contours) {
      # Calculate appropriate contour breaks for percentage data
      proportion_range <- range(scenario_data$cdr_percentage, na.rm = TRUE)
      contour_breaks <- seq(
        ceiling(proportion_range[1] / 10) * 10,  # Round to nearest 10%
        floor(proportion_range[2] / 10) * 10, 
        by = 10
      )
      
      if (length(contour_breaks) > 1) {
        p <- p + 
          geom_contour(aes(z = cdr_percentage), 
                       breaks = contour_breaks,
                       color = "white", 
                       alpha = contour_alpha, 
                       linewidth = 0.5)
      }
    }
    
    # Add infeasible markers if requested
    if (show_infeasible) {
      infeasible_data <- scenario_data %>% filter(!feasible)
      if (nrow(infeasible_data) > 0) {
        p <- p + 
          geom_point(data = infeasible_data, 
                     aes(x = mitigation_delay, y = cdr_delay), 
                     shape = 4, size = 2, color = "red", alpha = 0.8, 
                     stroke = 1.5, inherit.aes = FALSE)
      }
    }
    
    return(p)
  }
  
  # Get available scenarios (up to 5)
  available_scenarios <- intersect(ssp_order, unique(plot_data$scenario_short))
  
  # Create individual plots with proper titles
  p1 <- create_scenario_cdr_heatmap("SSP1", "A) SSP1")
  p2 <- create_scenario_cdr_heatmap("SSP2", "B) SSP2") 
  p3 <- create_scenario_cdr_heatmap("SSP3", "C) SSP3")
  p4 <- create_scenario_cdr_heatmap("SSP4", "D) SSP4")
  p5 <- create_scenario_cdr_heatmap("SSP5", "E) SSP5")
  
  # Create blank plot for F) position - no title
  p6 <- ggplot() + 
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA))
  
  # Create shared legend
  cdr_legend_plot <- create_scenario_cdr_heatmap("SSP1", "Legend") + 
    theme(legend.position = "right")
  cdr_legend_extracted <- cowplot::get_legend(cdr_legend_plot)
  
  # Create the main 3×2 plot grid
  main_plots <- (p1 + p2 + p3) / (p4 + p5 + p6)
  
  # Combine main plots with legend
  combined_plot <- main_plots | cdr_legend_extracted
  
  # Set relative widths: wide for main plots, narrow for legend
  combined_plot <- combined_plot + plot_layout(widths = c(10, 1))
  
  # Count scenarios and feasible combinations for subtitle
  n_scenarios <- length(available_scenarios)
  n_total_combinations <- nrow(plot_data)
  n_feasible <- sum(plot_data$feasible, na.rm = TRUE)
  
  # Add overall title and subtitle
  final_plot <- combined_plot + 
    plot_annotation(
      title = "CDR Proportion Analysis: Multi-Scenario Delayed Deployment",
      subtitle = paste0("SSP scenarios showing CDR proportion (%) vs deployment delays | ", 
                        n_scenarios, " scenarios, ", n_total_combinations, " combinations, ", 
                        n_feasible, " feasible"),
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.margin = margin(10, 0, 10, 0)
      )
    )
  
  # Save if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0("multi_scenario_cdr_proportion_dashboard_", 
                         format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    }
    
    filepath <- here::here("figs", filename)
    
    # 3×2 layout dimensions (wider than tall)
    ggsave(
      filename = filepath,
      plot = final_plot,
      width = 297,  # A4 landscape width
      height = 210, # A4 landscape height
      units = "mm",
      device = cairo_pdf
    )
    
    cat("Multi-scenario CDR proportion dashboard saved to:", filepath, "\n")
  }
  
  return(final_plot)
}

