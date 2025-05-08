#' @title Visualisation Functions for Climate Optimal Control Results
#' @description
#' This script contains functions to create standardised visualisations of the 
#' optimal control model outputs. It provides functions for plotting temperature
#' anomalies and cumulative emissions across different parameter sets.
#'
#' @author Nina Rynne
#' @project Optimal Control of Temperature Overshoot
#' @date March 2025
#' @license MIT
#' @version 0.1.0
#'
#' @citation If published, include how to cite this work, e.g.:
#' Rynne, N. (2025). Optimal control methods for climate temperature overshoot.
#' Journal of Climate Modeling, XX(X), XXX-XXX. doi:XX.XXXX/XXXXX
#' 
#' @funding Include any grant or funding information here
#'
#' @collaborators List any collaborators or contributors
#'
#' @contact nina.rynne@griffithuni.edu.au
#'
#' @repository https://github.com/nina-rynne/optimal-control-model
#'
#' @references
#' - Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. 
#'   Springer-Verlag New York. ISBN 978-3-319-24277-4
#' - Wickham, H. et al. (2019). Welcome to the tidyverse. Journal of Open 
#'   Source Software, 4(43), 1686. https://doi.org/10.21105/joss.01686
#'
#' @dependencies
#' Required packages: tidyverse, ggplot2, cowplot, patchwork, Cairo
#' This module provides the following key functions:
#' - create_temperature_plot: Creates a line plot of temperature anomalies over time
#' - create_emissions_plot: Creates a line plot of cumulative emissions over time
#' - create_mitigation_plot: Creates a line plot of GtCO2 mitigated over time
#' - create_cdr_plot: Creates a line plot of CtCO2 removed over time
#' - create_costs_plot: Creates a line plot of summed costs over time
#' - create_adjoint_plot: Creates a line plot of adjoint variable over time
#' 

#' @title Create Temperature Anomaly Plot
#' @description
#' Creates a line plot visualizing temperature anomalies over time for different
#' parameter sets in the optimal control model.
#' 
#' @param oc_solution List containing model output for different parameter sets
#'                    Each element should be a named run (e.g., "run_1") containing
#'                    at minimum the vectors "years" and "temperature_anomaly"
#'                   
#' @return A ggplot object showing temperature anomaly trajectories over time
#'         for different parameter sets
#'         
#' @examples
#' # Assuming oc_solution contains multiple model runs:
#' temperature_plot <- create_temperature_plot(oc_solution)
#' # View the plot
#' print(temperature_plot)
#' # Save the plot to a file
#' ggsave("temperature_anomaly.png", temperature_plot, width = 10, height = 6)
#'

create_temperature_plot <- function(oc_solution){
  
  # Step 1: Convert the nested list into a tibble
  # This creates a data frame where each row is one run
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    # Keep only the run_* entries (skipping other elements if any)
    filter(str_detect(run_id, "^run_"))
  
  # Step 2: Extract the needed variables from each run
  # This creates a longer data frame where each row is one year-run combination
  temperature_data <- run_data %>%
    # Extract years and temperature_anomaly from each run
    mutate(
      years = map(run_list, "years"),
      temp_anomaly = map(run_list, "temperature_anomaly")
    ) %>%
    # Unnest to get one row per year per run
    select(run_id, years, temp_anomaly) %>%
    unnest(cols = c(years, temp_anomaly))
  
  # Step 3: Create the plot with consistent styling
  plot_temperature <- ggplot(temperature_data, aes(x = years, y = temp_anomaly, color = run_id)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(
      title = "Temperature Anomaly Across Different Parameter Sets",
      x = "Year",
      y = "Temperature Anomaly (°C)",
      color = "Parameter Set"
    ) +
    theme(legend.position = "bottom")
  
  return(plot_temperature)
}

#' @title Create Cumulative Emissions Plot
#' @description
#' Creates a line plot visualizing cumulative emissions over time for different
#' parameter sets in the optimal control model.
#' 
#' @param oc_solution List containing model output for different parameter sets
#'                    Each element should be a named run (e.g., "run_1") containing
#'                    at minimum the vectors "years" and "cumulative_emissions"
#'                   
#' @return A ggplot object showing cumulative emissions trajectories over time
#'         for different parameter sets
#'         
#' @examples
#' # Assuming oc_solution contains multiple model runs:
#' emissions_plot <- create_emissions_plot(oc_solution)
#' # View the plot
#' print(emissions_plot)
#' # Save the plot to a file
#' ggsave("cumulative_emissions.png", emissions_plot, width = 10, height = 6)
#'

create_emissions_plot <- function(oc_solution){
  
  # Step 1: Convert the nested list into a tibble
  # This creates a data frame where each row is one run
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    # Keep only the run_* entries (skipping other elements if any)
    filter(str_detect(run_id, "^run_"))
  
  # Step 2: Extract the needed variables from each run
  # This creates a longer data frame where each row is one year-run combination
  emissions_data <- run_data %>%
    # Extract years and cumulative_emissions from each run
    mutate(
      years = map(run_list, "years"),
      cum_emissions = map(run_list, "cumulative_emissions")
    ) %>%
    # Unnest to get one row per year per run
    select(run_id, years, cum_emissions) %>%
    unnest(cols = c(years, cum_emissions))
  
  # Step 3: Create the plot with consistent styling
  plot_emissions <- ggplot(emissions_data, aes(x = years, y = cum_emissions, color = run_id)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(
      title = "Cumulative Emissions Across Different Parameter Sets",
      x = "Year",
      y = "Cumulative Emissions (GtC)",
      color = "Parameter Set"
    ) +
    theme(legend.position = "bottom")
  
  return(plot_emissions)
}

#' @title Create Mitigation Quantity Plot
#' @description
#' Creates a line plot visualizing mitigation quantities over time for different
#' parameter sets in the optimal control model.
#' 
#' @param oc_solution List containing model output for different parameter sets
#'                    Each element should be a named run (e.g., "run_1") containing
#'                    at minimum the vectors "years" and "qty_mitig"
#'                   
#' @return A ggplot object showing mitigation quantity trajectories over time
#'         for different parameter sets
#'         
#' @examples
#' # Assuming oc_solution contains multiple model runs:
#' mitigation_plot <- create_mitigation_plot(oc_solution)
#' # View the plot
#' print(mitigation_plot)
#' # Save the plot to a file
#' ggsave("mitigation_quantity.png", mitigation_plot, width = 10, height = 6)
#'

create_mitigation_plot <- function(oc_solution){
  
  # Step 1: Convert the nested list into a tibble
  # This creates a data frame where each row is one run
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    # Keep only the run_* entries (skipping other elements if any)
    filter(str_detect(run_id, "^run_"))
  
  # Step 2: Extract the needed variables from each run
  # This creates a longer data frame where each row is one year-run combination
  mitigation_data <- run_data %>%
    # Extract years and mitigation quantities from each run
    mutate(
      years = map(run_list, "years"),
      qty_mitig = map(run_list, "qty_mitig")
    ) %>%
    # Unnest to get one row per year per run
    select(run_id, years, qty_mitig) %>%
    unnest(cols = c(years, qty_mitig))
  
  # Step 3: Create the plot with consistent styling
  plot_mitigation <- ggplot(mitigation_data, aes(x = years, y = qty_mitig, color = run_id)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(
      title = "Mitigation Quantity Across Different Parameter Sets",
      x = "Year",
      y = "Mitigation Quantity (GtC/yr)",
      color = "Parameter Set"
    ) +
    theme(legend.position = "bottom")
  
  return(plot_mitigation)
}

#' @title Create Carbon Dioxide Removal Plot
#' @description
#' Creates a line plot visualizing carbon dioxide removal quantities over time for different
#' parameter sets in the optimal control model.
#' 
#' @param oc_solution List containing model output for different parameter sets
#'                    Each element should be a named run (e.g., "run_1") containing
#'                    at minimum the vectors "years" and "qty_remov"
#'                   
#' @return A ggplot object showing carbon dioxide removal trajectories over time
#'         for different parameter sets
#'         
#' @examples
#' # Assuming oc_solution contains multiple model runs:
#' cdr_plot <- create_cdr_plot(oc_solution)
#' # View the plot
#' print(cdr_plot)
#' # Save the plot to a file
#' ggsave("carbon_dioxide_removal.png", cdr_plot, width = 10, height = 6)
#'

create_cdr_plot <- function(oc_solution){
  
  # Step 1: Convert the nested list into a tibble
  # This creates a data frame where each row is one run
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    # Keep only the run_* entries (skipping other elements if any)
    filter(str_detect(run_id, "^run_"))
  
  # Step 2: Extract the needed variables from each run
  # This creates a longer data frame where each row is one year-run combination
  cdr_data <- run_data %>%
    # Extract years and carbon dioxide removal quantities from each run
    mutate(
      years = map(run_list, "years"),
      qty_remov = map(run_list, "qty_remov")
    ) %>%
    # Unnest to get one row per year per run
    select(run_id, years, qty_remov) %>%
    unnest(cols = c(years, qty_remov))
  
  # Step 3: Create the plot with consistent styling
  plot_cdr <- ggplot(cdr_data, aes(x = years, y = qty_remov, color = run_id)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(
      title = "Carbon Dioxide Removal Across Different Parameter Sets",
      x = "Year",
      y = "Carbon Dioxide Removal (GtC/yr)",
      color = "Parameter Set"
    ) +
    theme(legend.position = "bottom")
  
  return(plot_cdr)
}

#' @title Create Cumulative Total Cost Plot
#' @description
#' Creates a line plot visualizing cumulative total costs over time for different
#' parameter sets in the optimal control model.
#' 
#' @param oc_solution List containing model output for different parameter sets
#'                    Each element should be a named run (e.g., "run_1") containing
#'                    at minimum the vectors "years" and "cost_total_cumul"
#'                   
#' @return A ggplot object showing cumulative total cost trajectories over time
#'         for different parameter sets
#'         
#' @examples
#' # Assuming oc_solution contains multiple model runs:
#' cost_plot <- create_cost_plot(oc_solution)
#' # View the plot
#' print(cost_plot)
#' # Save the plot to a file
#' ggsave("cumulative_costs.png", cost_plot, width = 10, height = 6)
#'

create_cost_plot <- function(oc_solution){
  
  # Step 1: Convert the nested list into a tibble
  # This creates a data frame where each row is one run
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    # Keep only the run_* entries (skipping other elements if any)
    filter(str_detect(run_id, "^run_"))
  
  # Step 2: Extract the needed variables from each run
  # This creates a longer data frame where each row is one year-run combination
  cost_data <- run_data %>%
    # Extract years and cumulative total costs from each run
    mutate(
      years = map(run_list, "years"),
      cost_total_cumul = map(run_list, "cost_total_cumul")
    ) %>%
    # Unnest to get one row per year per run
    select(run_id, years, cost_total_cumul) %>%
    unnest(cols = c(years, cost_total_cumul))
  
  # Step 3: Create the plot with consistent styling
  plot_cost <- ggplot(cost_data, aes(x = years, y = cost_total_cumul, color = run_id)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(
      title = "Cumulative Total Costs Across Different Parameter Sets",
      x = "Year",
      y = "Cumulative Total Costs ($ Trillion)",
      color = "Parameter Set"
    ) +
    theme(legend.position = "bottom")
  
  return(plot_cost)
}

#' @title Create Stacked Cumulative Cost Plot (Average Across All Runs)
#' @description
#' Creates a stacked area plot visualizing the average cumulative mitigation, removal and
#' residual costs across all runs over time.
#' 
#' @param oc_solution List containing model output for different parameter sets
#' @param normalize Logical indicating whether to normalize costs to make scale more manageable
#'                 (default: FALSE)
#'                   
#' @return A ggplot object showing stacked average cumulative costs over time
create_stacked_cost_plot <- function(oc_solution, normalize = FALSE) {
  # Extract and prepare data
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  # Extract cost components for each run
  cost_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      cost_mitig_cumul = map(run_list, "cost_mitig_cumul"),
      cost_remov_cumul = map(run_list, "cost_remov_cumul"),
      cost_resid_cumul = map(run_list, "cost_resid_cumul")
    ) %>%
    select(run_id, years, cost_mitig_cumul, cost_remov_cumul, cost_resid_cumul) %>%
    unnest(cols = c(years, cost_mitig_cumul, cost_remov_cumul, cost_resid_cumul))
  
  # Calculate average across all runs for each year and cost component
  cost_avg_by_year <- cost_data %>%
    group_by(years) %>%
    summarize(
      cost_mitig_cumul_avg = mean(cost_mitig_cumul),
      cost_remov_cumul_avg = mean(cost_remov_cumul),
      cost_resid_cumul_avg = mean(cost_resid_cumul),
      .groups = "drop"
    )
  
  # Reshape to long format for stacked area plot
  cost_avg_long <- cost_avg_by_year %>%
    pivot_longer(
      cols = c(cost_mitig_cumul_avg, cost_remov_cumul_avg, cost_resid_cumul_avg),
      names_to = "cost_type",
      values_to = "cost_value"
    ) %>%
    # Create more readable labels for cost types
    mutate(cost_type = case_when(
      cost_type == "cost_mitig_cumul_avg" ~ "Mitigation",
      cost_type == "cost_remov_cumul_avg" ~ "Removal",
      cost_type == "cost_resid_cumul_avg" ~ "Residual",
      TRUE ~ cost_type
    ))
  
  # Optionally normalize costs for better readability
  y_label <- "Average Cumulative Costs ($ Trillion)"
  if (normalize) {
    max_value <- max(cost_avg_long$cost_value)
    normalization_factor <- 10^(floor(log10(max_value)))
    
    cost_avg_long <- cost_avg_long %>%
      mutate(cost_value = cost_value / normalization_factor)
    
    y_label <- paste0("Average Cumulative Costs ($ Trillion × 10^", log10(normalization_factor), ")")
  }
  
  # Get number of runs for the subtitle
  n_runs <- length(unique(cost_data$run_id))
  
  # Create the stacked area plot with clear labeling
  plot_stacked_cost <- ggplot(cost_avg_long, 
                              aes(x = years, y = cost_value, 
                                  fill = cost_type)) +
    geom_area(position = "stack", alpha = 0.8) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Cumulative Total Costs",
      subtitle = paste("Average across", n_runs, "runs"),
      x = "Year",
      y = y_label,
      fill = "Cost Component"
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
    )
  
  return(plot_stacked_cost)
}

#' @title Create Adjoint Variable Plot
#' @description
#' Creates a line plot visualizing the adjoint variable over time for different
#' parameter sets in the optimal control model. The adjoint variable represents
#' the shadow price or marginal value of state variables in the optimization.
#' 
#' @param oc_solution List containing model output for different parameter sets
#'                    Each element should be a named run (e.g., "run_1") containing
#'                    at minimum the vectors "years" and "adjoint_var"
#'                   
#' @return A ggplot object showing adjoint variable trajectories over time
#'         for different parameter sets
#'         
#' @examples
#' # Assuming oc_solution contains multiple model runs:
#' adjoint_plot <- create_adjoint_plot(oc_solution)
#' # View the plot
#' print(adjoint_plot)
#' # Save the plot to a file
#' ggsave("adjoint_variable.png", adjoint_plot, width = 10, height = 6)
#'

create_adjoint_plot <- function(oc_solution){
  
  # Step 1: Convert the nested list into a tibble
  # This creates a data frame where each row is one run
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    # Keep only the run_* entries (skipping other elements if any)
    filter(str_detect(run_id, "^run_"))
  
  # Step 2: Extract the needed variables from each run
  # This creates a longer data frame where each row is one year-run combination
  adjoint_data <- run_data %>%
    # Extract years and adjoint variable values from each run
    mutate(
      years = map(run_list, "years"),
      adjoint_var = map(run_list, "adjoint_var")
    ) %>%
    # Unnest to get one row per year per run
    select(run_id, years, adjoint_var) %>%
    unnest(cols = c(years, adjoint_var))
  
  # Step 3: Create the plot with consistent styling
  plot_adjoint <- ggplot(adjoint_data, aes(x = years, y = adjoint_var, color = run_id)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(
      title = "Adjoint Variable Across Different Parameter Sets",
      x = "Year",
      y = "Adjoint Variable Value",
      color = "Parameter Set"
    ) +
    theme(legend.position = "bottom")
  
  return(plot_adjoint)
}

#' @title Create Density-Based Temperature Anomaly Plot
#' @description
#' Creates a density plot visualizing temperature anomalies over time for different
#' parameter sets in the optimal control model. Areas where many trajectories
#' overlap appear darker/more opaque.
#' 
#' @param oc_solution List containing model output for different parameter sets
#' @return A ggplot object showing temperature anomaly density over time
create_temperature_density_plot <- function(oc_solution) {
  # Extract data as in the original function
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  temperature_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      temp_anomaly = map(run_list, "temperature_anomaly")
    ) %>%
    select(run_id, years, temp_anomaly) %>%
    unnest(cols = c(years, temp_anomaly))
  
  # Create the density-based plot
  plot_temperature <- ggplot(temperature_data, aes(x = years, y = temp_anomaly, group = run_id)) +
    # Add a transparent line for each run
    geom_line(color = "blue", alpha = 0.2) +
    # Add a density estimate layer
    stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE,
                    alpha = 0.6, h = c(5, 0.05)) +
    scale_fill_gradient(low = "white", high = "blue") +
    # Add a smoothed summary line to show central tendency
    geom_smooth(aes(group = 1), color = "darkblue", se = FALSE, linewidth = 1.2)
    theme_minimal() +
    labs(
      title = "Temperature Anomaly Density",
      x = "Year",
      y = "Temperature Anomaly (°C)"
    ) +
    theme(legend.position = "none")
  
  return(plot_temperature)
}

#' @title Create Density-Based Cumulative Emissions Plot
#' @description
#' Creates a density plot visualizing cumulative emissions over time for different
#' parameter sets in the optimal control model. Areas where many trajectories
#' overlap appear darker/more opaque.
#' 
#' @param oc_solution List containing model output for different parameter sets
#' @return A ggplot object showing cumulative emissions density over time
create_emissions_density_plot <- function(oc_solution) {
  # Extract data as in the original function
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  emissions_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      cum_emissions = map(run_list, "cumulative_emissions")
    ) %>%
    select(run_id, years, cum_emissions) %>%
    unnest(cols = c(years, cum_emissions))
  
  # Create the density-based plot
  plot_emissions <- ggplot(emissions_data, aes(x = years, y = cum_emissions, group = run_id)) +
    # Add a transparent line for each run
    geom_line(color = "forestgreen", alpha = 0.2) +
    # Add a density estimate layer
    stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE,
                    alpha = 0.6, h = c(5, 100)) +
    scale_fill_gradient(low = "white", high = "forestgreen") +
    # Add a smoothed summary line to show central tendency
    geom_smooth(aes(group = 1), color = "darkgreen", se = FALSE, linewidth = 1.2) +
    theme_minimal() +
    labs(
      title = "Cumulative Emissions Density",
      x = "Year",
      y = "Cumulative Emissions (GtC)"
    ) +
    theme(legend.position = "none")
  
  return(plot_emissions)
}

#' @title Create Temperature Range Plot (Reference Style)
#' @description
#' Creates a plot showing the range of possible temperature anomalies over time
#' styled to match the reference example.
#' 
#' @param oc_solution List containing model output for different parameter sets
#' @return A ggplot object showing temperature anomaly range and central line

create_temperature_range_plot <- function(oc_solution) {
  # Extract data as in the original function
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  temperature_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      temp_anomaly = map(run_list, "temperature_anomaly")
    ) %>%
    select(run_id, years, temp_anomaly) %>%
    unnest(cols = c(years, temp_anomaly))
  
  # Calculate summary statistics at each year
  temp_summary <- temperature_data %>%
    group_by(years) %>%
    summarize(
      min_temp = min(temp_anomaly),
      max_temp = max(temp_anomaly),
      mean_temp = mean(temp_anomaly),
      .groups = "drop"
    )
  
  # Create the range envelope plot with styling similar to the reference
  plot_temperature <- ggplot(temp_summary, aes(x = years)) +
    # Add a horizontal reference line at y=0
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.3) +
    # Shaded area from min to max - use a purple/blue color similar to reference
    geom_ribbon(aes(ymin = min_temp, ymax = max_temp), 
                fill = "steelblue", alpha = 0.25) +
    # Mean line - darker blue
    geom_line(aes(y = mean_temp), color = "navy", linewidth = 0.8) +
    # Min and max border lines - match ribbon color
    geom_line(aes(y = min_temp), color = "steelblue", linewidth = 0.4) +
    geom_line(aes(y = max_temp), color = "steelblue", linewidth = 0.4) +
    # Use a cleaner theme with more white space
    theme_minimal() +
    # Remove grid lines to match reference
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      axis.line = element_line(color = "black", linewidth = 0.3)
    ) +
    # Labels
    labs(
      title = NULL,  # No title to match reference
      x = "Year",
      y = "Temperature Anomaly (°C)"
    )
  
  return(plot_temperature)
}

#' @title Create Temperature Anomaly Envelope Plot with Clear Boundaries
#' @description
#' Creates a publication-quality plot showing the range of possible temperature anomalies 
#' over time with quantile bands to highlight the distribution of outcomes. Solid lines
#' appear only on the outside edges for better visual clarity.
#' 
#' @param oc_solution List containing model output for different parameter sets
#' @param color_base Base color for the plot (default: "steelblue")
#' @param show_individual_lines Logical indicating whether to show individual trajectories (default: FALSE)
#' @param show_full_range Logical indicating whether to show the full min-max range (default: TRUE)
#' @param line_size Line width for the median line (default: 1.0)
#'                   
#' @return A ggplot object showing temperature anomaly ranges over time
create_temperature_envelope_plot <- function(oc_solution, 
                                             color_base = "steelblue",
                                             show_individual_lines = FALSE,
                                             show_full_range = TRUE,
                                             line_size = 1.0) {
  # Step 1: Convert the nested list into a tibble
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    # Keep only the run_* entries (skipping other elements if any)
    filter(str_detect(run_id, "^run_"))
  
  # Step 2: Extract the needed variables from each run
  temperature_data <- run_data %>%
    # Extract years and temperature anomalies from each run
    mutate(
      years = map(run_list, "years"),
      temp_anomaly = map(run_list, "temperature_anomaly")
    ) %>%
    # Unnest to get one row per year per run
    select(run_id, years, temp_anomaly) %>%
    unnest(cols = c(years, temp_anomaly))
  
  # Step 3: Calculate summary statistics for each year
  temp_summary <- temperature_data %>%
    group_by(years) %>%
    summarize(
      min_val = min(temp_anomaly),            # Minimum value
      max_val = max(temp_anomaly),            # Maximum value
      median_val = median(temp_anomaly),      # Median (50th percentile)
      q25 = quantile(temp_anomaly, 0.25),     # 25th percentile
      q75 = quantile(temp_anomaly, 0.75),     # 75th percentile
      q10 = quantile(temp_anomaly, 0.10),     # 10th percentile
      q90 = quantile(temp_anomaly, 0.90),     # 90th percentile
      .groups = "drop"
    )
  
  # Create a darker version of the base color for the median line
  darker_color <- colorspace::darken(color_base, 0.5)
  lighter_color <- colorspace::lighten(color_base, 0.3)
  
  # Step 4: Create the plot with multiple layers
  plot_temperature <- ggplot()
  
  # Add a horizontal reference line at zero
  plot_temperature <- plot_temperature +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.3, linetype = "solid")
  
  # Add full min-max range if requested (lightest shading)
  if (show_full_range) {
    plot_temperature <- plot_temperature +
      geom_ribbon(data = temp_summary, 
                  aes(x = years, ymin = min_val, ymax = max_val), 
                  fill = lighter_color, alpha = 0.15, color = NA)
  }
  
  # Add 10-90th percentile range (medium shading)
  plot_temperature <- plot_temperature +
    geom_ribbon(data = temp_summary, 
                aes(x = years, ymin = q10, ymax = q90), 
                fill = lighter_color, alpha = 0.25, color = NA)
  
  # Add interquartile range (darkest shading)
  plot_temperature <- plot_temperature +
    geom_ribbon(data = temp_summary, 
                aes(x = years, ymin = q25, ymax = q75), 
                fill = color_base, alpha = 0.35, color = NA)
  
  # Add individual trajectories if requested
  if (show_individual_lines) {
    plot_temperature <- plot_temperature +
      geom_line(data = temperature_data, 
                aes(x = years, y = temp_anomaly, group = run_id), 
                color = darker_color, alpha = 0.05, linewidth = 0.2)
  }
  
  # Add median line
  plot_temperature <- plot_temperature +
    geom_line(data = temp_summary, 
              aes(x = years, y = median_val), 
              color = darker_color, linewidth = line_size, linetype = "solid")
  
  # Add ONLY outer boundary lines (min and max if showing full range, otherwise q10 and q90)
  if (show_full_range) {
    plot_temperature <- plot_temperature +
      geom_line(data = temp_summary,
                aes(x = years, y = min_val),
                color = color_base, linewidth = 0.6, linetype = "solid") +
      geom_line(data = temp_summary,
                aes(x = years, y = max_val),
                color = color_base, linewidth = 0.6, linetype = "solid")
  } else {
    plot_temperature <- plot_temperature +
      geom_line(data = temp_summary,
                aes(x = years, y = q10),
                color = color_base, linewidth = 0.6, linetype = "solid") +
      geom_line(data = temp_summary,
                aes(x = years, y = q90),
                color = color_base, linewidth = 0.6, linetype = "solid")
  }
  
  # Apply publication-quality theme
  plot_temperature <- plot_temperature +
    theme_minimal() +
    theme(
      text = element_text(family = "serif", size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
    ) +
    # Add labels
    labs(
      x = "Year",
      y = "Temperature Anomaly (°C)"
    )
  
  return(plot_temperature)
}

#' @title Create Mitigation Envelope Plot with Quantile Bands
#' @description
#' Creates a publication-quality plot showing the range of possible mitigation quantities 
#' over time with quantile bands to highlight the distribution of outcomes.
#' 
#' @param oc_solution List containing model output for different parameter sets
#'                    Each element should be a named run (e.g., "run_1") containing
#'                    at minimum the vectors "years" and "qty_mitig"
#' @param color_base Base color for the plot (default: "forestgreen")
#' @param show_individual_lines Logical indicating whether to show individual 
#'                             trajectory lines (default: FALSE)
#' @param show_full_range Logical indicating whether to show the full min-max 
#'                        range (default: TRUE)
#' @param line_size Line width for the median line (default: 0.8)
#'                   
#' @return A ggplot object showing mitigation quantity ranges over time
#'         
#' @examples
#' # Basic usage:
#' mitigation_plot <- create_mitigation_envelope_plot(oc_solution)
#' 
#' # Customized color:
#' mitigation_plot <- create_mitigation_envelope_plot(oc_solution, color_base = "steelblue")
#' 
#' # With individual trajectory lines:
#' mitigation_plot <- create_mitigation_envelope_plot(oc_solution, show_individual_lines = TRUE)
#'
create_mitigation_envelope_plot <- function(oc_solution, 
                                            color_base = "forestgreen",
                                            show_individual_lines = FALSE,
                                            show_full_range = TRUE,
                                            line_size = 0.8) {
  # Step 1: Convert the nested list into a tibble
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    # Keep only the run_* entries (skipping other elements if any)
    filter(str_detect(run_id, "^run_"))
  
  # Step 2: Extract the needed variables from each run
  mitigation_data <- run_data %>%
    # Extract years and mitigation quantities from each run
    mutate(
      years = map(run_list, "years"),
      qty_mitig = map(run_list, "qty_mitig")
    ) %>%
    # Unnest to get one row per year per run
    select(run_id, years, qty_mitig) %>%
    unnest(cols = c(years, qty_mitig))
  
  # Step 3: Calculate summary statistics for each year
  mitig_summary <- mitigation_data %>%
    group_by(years) %>%
    summarize(
      min_val = min(qty_mitig),               # Minimum value
      max_val = max(qty_mitig),               # Maximum value
      median_val = median(qty_mitig),         # Median (50th percentile)
      q25 = quantile(qty_mitig, 0.25),        # 25th percentile
      q75 = quantile(qty_mitig, 0.75),        # 75th percentile
      q10 = quantile(qty_mitig, 0.10),        # 10th percentile
      q90 = quantile(qty_mitig, 0.90),        # 90th percentile
      .groups = "drop"
    )
  
  # Create a darker version of the base color for the median line
  darker_color <- colorspace::darken(color_base, 0.3)
  
  # Step 4: Create the plot with multiple layers
  plot_mitigation <- ggplot()
  
  # Add full min-max range if requested (lightest shading)
  if (show_full_range) {
    plot_mitigation <- plot_mitigation +
      geom_ribbon(data = mitig_summary, 
                  aes(x = years, ymin = min_val, ymax = max_val), 
                  fill = color_base, alpha = 0.15)
  }
  
  # Add 10-90th percentile range (medium shading)
  plot_mitigation <- plot_mitigation +
    geom_ribbon(data = mitig_summary, 
                aes(x = years, ymin = q10, ymax = q90), 
                fill = color_base, alpha = 0.25)
  
  # Add interquartile range (darkest shading)
  plot_mitigation <- plot_mitigation +
    geom_ribbon(data = mitig_summary, 
                aes(x = years, ymin = q25, ymax = q75), 
                fill = color_base, alpha = 0.4)
  
  # Add individual trajectories if requested
  if (show_individual_lines) {
    plot_mitigation <- plot_mitigation +
      geom_line(data = mitigation_data, 
                aes(x = years, y = qty_mitig, group = run_id), 
                color = darker_color, alpha = 0.08, linewidth = 0.2)
  }
  
  # Add median line
  plot_mitigation <- plot_mitigation +
    geom_line(data = mitig_summary, 
              aes(x = years, y = median_val), 
              color = darker_color, linewidth = line_size)
  
  # Add boundary lines for the 10-90 percentile range
  plot_mitigation <- plot_mitigation +
    geom_line(data = mitig_summary,
              aes(x = years, y = q10),
              color = color_base, linewidth = 0.4, linetype = "solid") +
    geom_line(data = mitig_summary,
              aes(x = years, y = q90),
              color = color_base, linewidth = 0.4, linetype = "solid")
  
  # Apply publication-quality theme
  plot_mitigation <- plot_mitigation +
    theme_minimal() +
    theme(
      text = element_text(family = "serif", size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
    ) +
    # Add labels
    labs(
      x = "Year",
      y = "Mitigation Quantity (GtC/yr)"
    )
  
  return(plot_mitigation)
}

#' @title Create Mitigation Density Heatmap
#' @description
#' Creates a density heatmap visualizing the distribution of mitigation quantities
#' over time, highlighting areas where many trajectories overlap.
#' 
#' @param oc_solution List containing model output for different parameter sets
#'                    Each element should be a named run (e.g., "run_1") containing
#'                    at minimum the vectors "years" and "qty_mitig"
#'                   
#' @return A ggplot object showing mitigation quantity density over time
#'         
#' @examples
#' # Assuming oc_solution contains multiple model runs:
#' mitigation_plot <- create_mitigation_heatmap(oc_solution)
#' # View the plot
#' print(mitigation_plot)
#' # Save the plot to a file
#' ggsave("mitigation_heatmap.png", mitigation_plot, width = 10, height = 6)
#'
create_mitigation_heatmap <- function(oc_solution) {
  # Step 1: Convert the nested list into a tibble
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    # Keep only the run_* entries (skipping other elements if any)
    filter(str_detect(run_id, "^run_"))
  
  # Step 2: Extract the needed variables from each run
  mitigation_data <- run_data %>%
    # Extract years and mitigation quantities from each run
    mutate(
      years = map(run_list, "years"),
      qty_mitig = map(run_list, "qty_mitig")
    ) %>%
    # Unnest to get one row per year per run
    select(run_id, years, qty_mitig) %>%
    unnest(cols = c(years, qty_mitig))
  
  # Step 3: Calculate summary statistics for reference lines
  mitig_summary <- mitigation_data %>%
    group_by(years) %>%
    summarize(
      median_val = median(qty_mitig),
      .groups = "drop"
    )
  
  # Step 4: Randomly sample a few trajectories to highlight
  set.seed(123) # For reproducibility
  sample_runs <- sample(unique(mitigation_data$run_id), 
                        min(5, length(unique(mitigation_data$run_id))))
  
  sample_data <- mitigation_data %>%
    filter(run_id %in% sample_runs)
  
  # Step 5: Create the density heatmap
  plot_mitigation <- ggplot(mitigation_data, aes(x = years, y = qty_mitig)) +
    # Create a 2D density estimate
    stat_density_2d(aes(fill = after_stat(density)), 
                    geom = "tile", contour = FALSE, n = 100) +
    # Use a sequential color palette - viridis is colorblind-friendly
    scale_fill_viridis_c(option = "viridis", name = "Density") +
    # Add the median line
    geom_line(data = mitig_summary,
              aes(x = years, y = median_val),
              color = "white", linewidth = 1) +
    # Add a few representative individual trajectories
    geom_line(data = sample_data,
              aes(x = years, y = qty_mitig, group = run_id),
              color = "white", alpha = 0.7, linewidth = 0.4) +
    # Apply a clean theme
    theme_minimal() +
    # Add labels
    labs(
      title = "Mitigation Quantity Density",
      x = "Year",
      y = "Mitigation Quantity (GtC/yr)"
    )
  
  return(plot_mitigation)
}

#' @title Create Mitigation Quantity Spaghetti Plot
#' @description
#' Creates a spaghetti plot visualizing mitigation quantities over time with all individual
#' runs shown as faint background lines, and median, min, and max as thicker guide lines.
#' 
#' @param oc_solution List containing model output for different parameter sets
#' @param color_base Base color for the plot (default: "forestgreen")
#' @param alpha_background Transparency of background lines (default: 0.1)
#' @param median_width Line width for the median line (default: 0.7)
#' @param bounds_width Line width for the min/max boundary lines (default: 0.5)
#'                   
#' @return A ggplot object showing mitigation quantity trajectories over time
#'         
#' @examples
#' # Basic usage:
#' mitigation_plot <- create_mitigation_spaghetti_plot(oc_solution)
#' 
#' # Customized:
#' mitigation_plot <- create_mitigation_spaghetti_plot(
#'   oc_solution, 
#'   color_base = "darkred",
#'   alpha_background = 0.05, 
#'   median_width = 1.0,
#'   bounds_width = 0.4
#' )
create_mitigation_spaghetti_plot <- function(oc_solution, 
                                             color_base = "forestgreen",
                                             alpha_background = 0.1,
                                             median_width = 0.7,
                                             bounds_width = 0.5) {
  # Extract and prepare data
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  mitigation_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      qty_mitig = map(run_list, "qty_mitig")
    ) %>%
    select(run_id, years, qty_mitig) %>%
    unnest(cols = c(years, qty_mitig))
  
  # Calculate summary statistics for each year
  mitig_summary <- mitigation_data %>%
    group_by(years) %>%
    summarize(
      min_val = min(qty_mitig),
      max_val = max(qty_mitig),
      median_val = median(qty_mitig),
      .groups = "drop"
    )
  
  # Create the plot
  plot_mitigation <- ggplot() +
    # Background lines (all individual runs faded)
    geom_line(data = mitigation_data,
              aes(x = years, y = qty_mitig, group = run_id),
              color = color_base, alpha = alpha_background, linewidth = 0.2) +
    
    # Min and max boundary lines (thinner than median)
    geom_line(data = mitig_summary,
              aes(x = years, y = min_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    geom_line(data = mitig_summary,
              aes(x = years, y = max_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    
    # Median line (thicker than boundary lines)
    geom_line(data = mitig_summary,
              aes(x = years, y = median_val),
              color = colorspace::darken(color_base, 0.3), 
              alpha = 1.0, 
              linewidth = median_width) +
    
    # Clean, publication-ready theme
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      text = element_text(family = "serif")
    ) +
    labs(
      x = "Year",
      y = "Mitigation Quantity (GtC/yr)"
    )
  
  return(plot_mitigation)
}

#' @title Create Carbon Dioxide Removal Spaghetti Plot
#' @description
#' Creates a spaghetti plot visualizing carbon dioxide removal quantities over time with all individual
#' runs shown as faint background lines, and median, min, and max as thicker guide lines.
#' 
#' @param oc_solution List containing model output for different parameter sets
#' @param color_base Base color for the plot (default: "purple4")
#' @param alpha_background Transparency of background lines (default: 0.1)
#' @param median_width Line width for the median line (default: 0.7)
#' @param bounds_width Line width for the min/max boundary lines (default: 0.5)
#'                   
#' @return A ggplot object showing carbon dioxide removal trajectories over time
#'         
#' @examples
#' # Basic usage:
#' cdr_plot <- create_cdr_spaghetti_plot(oc_solution)
#' 
#' # Customized:
#' cdr_plot <- create_cdr_spaghetti_plot(
#'   oc_solution, 
#'   color_base = "darkviolet",
#'   alpha_background = 0.05, 
#'   median_width = 1.0,
#'   bounds_width = 0.4
#' )
create_cdr_spaghetti_plot <- function(oc_solution, 
                                      color_base = "purple4",
                                      alpha_background = 0.1,
                                      median_width = 0.7,
                                      bounds_width = 0.5) {
  # Extract and prepare data
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  cdr_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      qty_remov = map(run_list, "qty_remov")
    ) %>%
    select(run_id, years, qty_remov) %>%
    unnest(cols = c(years, qty_remov))
  
  # Calculate summary statistics for each year
  cdr_summary <- cdr_data %>%
    group_by(years) %>%
    summarize(
      min_val = min(qty_remov),
      max_val = max(qty_remov),
      median_val = median(qty_remov),
      .groups = "drop"
    )
  
  # Create the plot
  plot_cdr <- ggplot() +
    # Background lines (all individual runs faded)
    geom_line(data = cdr_data,
              aes(x = years, y = qty_remov, group = run_id),
              color = color_base, alpha = alpha_background, linewidth = 0.2) +
    
    # Min and max boundary lines (thinner than median)
    geom_line(data = cdr_summary,
              aes(x = years, y = min_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    geom_line(data = cdr_summary,
              aes(x = years, y = max_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    
    # Median line (thicker than boundary lines)
    geom_line(data = cdr_summary,
              aes(x = years, y = median_val),
              color = colorspace::darken(color_base, 0.3), 
              alpha = 1.0, 
              linewidth = median_width) +
    
    # Clean, publication-ready theme
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      text = element_text(family = "serif")
    ) +
    labs(
      x = "Year",
      y = "Carbon Dioxide Removal (GtC/yr)"
    )
  
  return(plot_cdr)
}

#' @title Create Cumulative Emissions Spaghetti Plot
#' @description
#' Creates a spaghetti plot visualizing cumulative emissions over time with all individual
#' runs shown as faint background lines, and median, min, and max as thicker guide lines.
#' 
#' @param oc_solution List containing model output for different parameter sets
#' @param color_base Base color for the plot (default: "firebrick")
#' @param alpha_background Transparency of background lines (default: 0.1)
#' @param median_width Line width for the median line (default: 0.7)
#' @param bounds_width Line width for the min/max boundary lines (default: 0.5)
#'                   
#' @return A ggplot object showing cumulative emissions trajectories over time
create_emissions_spaghetti_plot <- function(oc_solution, 
                                            color_base = "firebrick",
                                            alpha_background = 0.1,
                                            median_width = 0.7,
                                            bounds_width = 0.5) {
  # Extract and prepare data
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  emissions_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      cum_emissions = map(run_list, "cumulative_emissions")
    ) %>%
    select(run_id, years, cum_emissions) %>%
    unnest(cols = c(years, cum_emissions))
  
  # Calculate summary statistics for each year
  emissions_summary <- emissions_data %>%
    group_by(years) %>%
    summarize(
      min_val = min(cum_emissions),
      max_val = max(cum_emissions),
      median_val = median(cum_emissions),
      .groups = "drop"
    )
  
  # Create the plot
  plot_emissions <- ggplot() +
    # Background lines (all individual runs faded)
    geom_line(data = emissions_data,
              aes(x = years, y = cum_emissions, group = run_id),
              color = color_base, alpha = alpha_background, linewidth = 0.2) +
    
    # Min and max boundary lines (thinner than median)
    geom_line(data = emissions_summary,
              aes(x = years, y = min_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    geom_line(data = emissions_summary,
              aes(x = years, y = max_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    
    # Median line (thicker than boundary lines)
    geom_line(data = emissions_summary,
              aes(x = years, y = median_val),
              color = colorspace::darken(color_base, 0.3), 
              alpha = 1.0, 
              linewidth = median_width) +
    
    # Clean, publication-ready theme
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      text = element_text(family = "serif")
    ) +
    labs(
      x = "Year",
      y = "Cumulative Emissions (GtC)"
    )
  
  return(plot_emissions)
}

#' @title Create Adjoint Variable Spaghetti Plot
#' @description
#' Creates a spaghetti plot visualizing adjoint variable values over time with all individual
#' runs shown as faint background lines, and median, min, and max as thicker guide lines.
#' 
#' @param oc_solution List containing model output for different parameter sets
#' @param color_base Base color for the plot (default: "darkorange")
#' @param alpha_background Transparency of background lines (default: 0.1)
#' @param median_width Line width for the median line (default: 0.7)
#' @param bounds_width Line width for the min/max boundary lines (default: 0.5)
#'                   
#' @return A ggplot object showing adjoint variable trajectories over time
create_adjoint_spaghetti_plot <- function(oc_solution, 
                                          color_base = "darkorange",
                                          alpha_background = 0.1,
                                          median_width = 0.7,
                                          bounds_width = 0.5) {
  # Extract and prepare data
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  adjoint_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      adjoint_var = map(run_list, "adjoint_var")
    ) %>%
    select(run_id, years, adjoint_var) %>%
    unnest(cols = c(years, adjoint_var))
  
  # Calculate summary statistics for each year
  adjoint_summary <- adjoint_data %>%
    group_by(years) %>%
    summarize(
      min_val = min(adjoint_var),
      max_val = max(adjoint_var),
      median_val = median(adjoint_var),
      .groups = "drop"
    )
  
  # Create the plot
  plot_adjoint <- ggplot() +
    # Background lines (all individual runs faded)
    geom_line(data = adjoint_data,
              aes(x = years, y = adjoint_var, group = run_id),
              color = color_base, alpha = alpha_background, linewidth = 0.2) +
    
    # Min and max boundary lines (thinner than median)
    geom_line(data = adjoint_summary,
              aes(x = years, y = min_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    geom_line(data = adjoint_summary,
              aes(x = years, y = max_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    
    # Median line (thicker than boundary lines)
    geom_line(data = adjoint_summary,
              aes(x = years, y = median_val),
              color = colorspace::darken(color_base, 0.3), 
              alpha = 1.0, 
              linewidth = median_width) +
    
    # Clean, publication-ready theme
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      text = element_text(family = "serif")
    ) +
    labs(
      x = "Year",
      y = "Adjoint Variable Value"
    )
  
  return(plot_adjoint)
}

### --------- TEMPERATURE PLOT SAMPLES --------- ###

create_temperature_simple_plot <- function(oc_solution, 
                                           color_base = "steelblue") {
  # Extract and prepare data (same as before)
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  temperature_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      temp_anomaly = map(run_list, "temperature_anomaly")
    ) %>%
    select(run_id, years, temp_anomaly) %>%
    unnest(cols = c(years, temp_anomaly))
  
  # Calculate summary statistics
  temp_summary <- temperature_data %>%
    group_by(years) %>%
    summarize(
      min_val = min(temp_anomaly),
      max_val = max(temp_anomaly),
      median_val = median(temp_anomaly),
      .groups = "drop"
    )
  
  # Create a simple plot with just median line and min-max band
  plot_temperature <- ggplot(temp_summary, aes(x = years)) +
    # Reference line at zero
    geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
    # Min-max shaded area
    geom_ribbon(aes(ymin = min_val, ymax = max_val), 
                fill = color_base, alpha = 0.2) +
    # Median line
    geom_line(aes(y = median_val), 
              color = color_base, linewidth = 1) +
    # Simple theme
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
    ) +
    labs(
      x = "Year",
      y = "Temperature Anomaly (°C)"
    )
  
  return(plot_temperature)
}

create_temperature_ci_plot <- function(oc_solution, 
                                       color_base = "steelblue") {
  # Same data preparation steps as before
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  temperature_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      temp_anomaly = map(run_list, "temperature_anomaly")
    ) %>%
    select(run_id, years, temp_anomaly) %>%
    unnest(cols = c(years, temp_anomaly))
  
  # Calculate summary statistics using 95% confidence interval
  temp_summary <- temperature_data %>%
    group_by(years) %>%
    summarize(
      median_val = median(temp_anomaly),
      lower_ci = quantile(temp_anomaly, 0.025),  # 2.5 percentile
      upper_ci = quantile(temp_anomaly, 0.975),  # 97.5 percentile
      .groups = "drop"
    )
  
  # Create the plot
  plot_temperature <- ggplot(temp_summary, aes(x = years)) +
    # Reference line at zero
    geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
    # 95% CI band
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
                fill = color_base, alpha = 0.2) +
    # Median line
    geom_line(aes(y = median_val), 
              color = color_base, linewidth = 1) +
    # Simple theme
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
    ) +
    labs(
      x = "Year",
      y = "Temperature Anomaly (°C)"
    )
  
  return(plot_temperature)
}

create_temperature_spaghetti_plot <- function(oc_solution, 
                                              color_base = "steelblue",
                                              alpha_background = 0.1,
                                              median_width = 0.7,
                                              bounds_width = 0.5) {
  # Extract and prepare data
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  temperature_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      temp_anomaly = map(run_list, "temperature_anomaly")
    ) %>%
    select(run_id, years, temp_anomaly) %>%
    unnest(cols = c(years, temp_anomaly))
  
  # Calculate summary statistics for each year
  temp_summary <- temperature_data %>%
    group_by(years) %>%
    summarize(
      min_val = min(temp_anomaly),
      max_val = max(temp_anomaly),
      median_val = median(temp_anomaly),
      .groups = "drop"
    )
  
  # Create the plot
  plot_temperature <- ggplot() +
    # Reference line at zero
    geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
    
    # Background lines (all individual runs faded)
    geom_line(data = temperature_data,
              aes(x = years, y = temp_anomaly, group = run_id),
              color = color_base, alpha = alpha_background, linewidth = 0.2) +
    
    # Min and max boundary lines (thinner than median)
    geom_line(data = temp_summary,
              aes(x = years, y = min_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    geom_line(data = temp_summary,
              aes(x = years, y = max_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    
    # Median line (thicker than boundary lines)
    geom_line(data = temp_summary,
              aes(x = years, y = median_val),
              color = colorspace::darken(color_base, 0.3), 
              alpha = 1.0, 
              linewidth = median_width) +
    
    # Clean, publication-ready theme
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      text = element_text(family = "serif")
    ) +
    labs(
      x = "Year",
      y = "Temperature Anomaly (°C)"
    )
  
  return(plot_temperature)
}

create_temperature_iqr_plot <- function(oc_solution, 
                                        color_base = "steelblue") {
  # Same data preparation steps
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  temperature_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      temp_anomaly = map(run_list, "temperature_anomaly")
    ) %>%
    select(run_id, years, temp_anomaly) %>%
    unnest(cols = c(years, temp_anomaly))
  
  # Calculate summary statistics - just median and IQR
  temp_summary <- temperature_data %>%
    group_by(years) %>%
    summarize(
      median_val = median(temp_anomaly),
      q25 = quantile(temp_anomaly, 0.25),
      q75 = quantile(temp_anomaly, 0.75),
      .groups = "drop"
    )
  
  # Create the plot
  plot_temperature <- ggplot(temp_summary, aes(x = years)) +
    # Reference line at zero
    geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
    # Interquartile range
    geom_ribbon(aes(ymin = q25, ymax = q75), 
                fill = color_base, alpha = 0.3) +
    # Median line
    geom_line(aes(y = median_val), 
              color = color_base, linewidth = 1) +
    # Simple theme
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
    ) +
    labs(
      x = "Year",
      y = "Temperature Anomaly (°C)"
    )
  
  return(plot_temperature)
}




#' @title Create Combined Model Output Dashboard
#' @description
#' Creates a comprehensive dashboard with five plots arranged in a 2×3 grid showing key model outputs:
#' emissions, temperature, mitigation, removal, adjoint variable, with the bottom-right area left blank.
#' Uses a consistent theme and styling across all plots.
#' 
#' @param oc_solution List containing model output for different parameter sets
#' @param save_plot Logical indicating whether to save the plot (default: TRUE)
#' @param show_legend Logical indicating whether to display a legend (default: FALSE)
#' @param width Plot width in mm (default: 190)
#' @param height Plot height in mm (default: 260)
#' @param filename Custom filename (default: generated based on current time)
#'                   
#' @return A ggplot object combining five plots in a 2×3 layout with bottom-right space blank
#'         
#' @examples
#' # Create and display the combined plot
#' dashboard <- create_combined_dashboard(oc_solution)
#' # With legend
#' dashboard <- create_combined_dashboard(oc_solution, show_legend = TRUE)
#'

create_combined_dashboard <- function(oc_solution, 
                                      save_plot = TRUE,
                                      show_legend = FALSE,
                                      width = 190,
                                      height = 260,
                                      filename = NULL) {
  
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
  
  # Create the individual plots
  # 1. Emissions plot (top-left)
  p1 <- create_emissions_spaghetti_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Cumulative Emissions")
  
  # 2. Temperature plot (top-right)
  p2 <- create_temperature_spaghetti_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Temperature Anomaly")
  
  # 3. Mitigation plot (middle-left)
  p3 <- create_mitigation_spaghetti_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Mitigation Quantity")
  
  # 4. Carbon Dioxide Removal plot (middle-right)
  p4 <- create_cdr_spaghetti_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Carbon Dioxide Removal")
  
  # 5. Adjoint Variable plot (bottom-left)
  p5 <- create_adjoint_spaghetti_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Adjoint Variable")
  
  # 6. Create a blank plot for the bottom-right area
  p6 <- ggplot() + 
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA))
  
  # Combine all plots using patchwork
  combined_plot <- (p1 + p2) /
    (p3 + p4) /
    (p5 + p6)
  
  # Add a shared legend if requested
  if (show_legend) {
    # Create a shared legend
    legend_plot <- create_emissions_plot(oc_solution) +
      my_theme +
      theme(legend.position = "bottom")
    
    # Extract the legend
    legend <- cowplot::get_legend(legend_plot)
    
    # Add the legend as a separate row
    combined_plot <- combined_plot / 
      patchwork::wrap_elements(full = legend) +
      patchwork::plot_layout(heights = c(1, 1, 1, 0.2))
  }
  
  # Add annotation
  final_plot <- combined_plot + 
    patchwork::plot_annotation(
      theme = theme(plot.margin = margin(10, 0, 10, 0))
    )
  
  # Save the plot if requested
  if (save_plot) {
    # Generate filename if not provided
    if (is.null(filename)) {
      filename <- paste0("combined_plot_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".pdf")
    }
    
    # Full path to save the file
    filepath <- here::here("figs", filename)
    
    # Save using Cairo device for better quality
    ggsave(filepath,
           final_plot,
           width = width,
           height = height,
           units = "mm",
           device = cairo_pdf)
    
    message(paste("Plot saved as", filepath))
  }
  
  return(final_plot)
}


#' @title Create Combined Model Output Dashboard WITH TITLE
#' 
create_combined_dashboard_title <- function(oc_solution, 
                                      save_plot = TRUE,
                                      show_legend = FALSE,
                                      width = 190,
                                      height = 260,
                                      filename = NULL) {
  
  # Extract discount rate from parameters
  # Find the name of the run element (it always starts with "run_")
  run_names <- names(oc_solution)[grep("^run_", names(oc_solution))]
  
  # For safety, just use the first run if multiple are found
  run_name <- run_names[1]
  
  # Extract the discount rate from the run's parameters
  disc_rate <- oc_solution[[run_name]]$parameters$disc_rate
  
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
  
  # Create the individual plots
  # 1. Emissions plot (top-left)
  p1 <- create_emissions_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Cumulative Emissions")
  
  # 2. Temperature plot (top-right)
  p2 <- create_temperature_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Temperature Anomaly")
  
  # 3. Mitigation plot (middle-left)
  p3 <- create_mitigation_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Mitigation Quantity")
  
  # 4. Carbon Dioxide Removal plot (middle-right)
  p4 <- create_cdr_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Carbon Dioxide Removal")
  
  # 5. Adjoint Variable plot (bottom-left)
  p5 <- create_adjoint_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Adjoint Variable")
  
  # 6. Cost plot (bottom-right) - WITH legend
  p6 <- create_stacked_cost_plot(oc_solution) +
    my_theme +
    theme(legend.position = "bottom",
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
          legend.key.size = unit(0.5, "cm")) +
    labs(title = "Cumulative Total Costs")
  
  # Combine all plots using patchwork
  combined_plot <- (p1 + p2) /
    (p3 + p4) /
    (p5 + p6)
  
  # Add a shared legend if requested
  if (show_legend) {
    # Create a shared legend
    legend_plot <- create_emissions_plot(oc_solution) +
      my_theme +
      theme(legend.position = "bottom")
    
    # Extract the legend
    legend <- cowplot::get_legend(legend_plot)
    
    # Add the legend as a separate row
    combined_plot <- combined_plot / 
      patchwork::wrap_elements(full = legend) +
      patchwork::plot_layout(heights = c(1, 1, 1, 0.2))
  }
  
  # Add annotation with title that includes discount rate
  final_plot <- combined_plot + 
    patchwork::plot_annotation(
      title = paste("Costs x 1000. Discount rate:", sprintf("%.1f%%", disc_rate * 100)),
      theme = theme(
        plot.margin = margin(10, 0, 10, 0),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
      )
    )
  
  # Save the plot if requested
  if (save_plot) {
    # Generate filename if not provided
    if (is.null(filename)) {
      filename <- paste0("combined_plot_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".pdf")
    }
    
    # Full path to save the file
    filepath <- here::here("figs", filename)
    
    # Save using Cairo device for better quality
    ggsave(filepath,
           final_plot,
           width = width,
           height = height,
           units = "mm",
           device = cairo_pdf)
    
    message(paste("Plot saved as", filepath))
  }
  
  return(final_plot)
}