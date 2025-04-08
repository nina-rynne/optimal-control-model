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

#' @title Create Stacked Cumulative Cost Plot
#' @description
#' Creates a stacked area plot visualizing cumulative mitigation, removal and
#' residual costs over time for different parameter sets in the optimal control model.
#' 
#' @param oc_solution List containing model output for different parameter sets
#'                    Each element should be a named run (e.g., "run_1") containing
#'                    at minimum the vectors "years", "cost_mitig_cumul",
#'                    "cost_remov_cumul" and "cost_resid_cumul"
#'                   
#' @return A ggplot object showing stacked cumulative costs over time
#'         for different parameter sets
#'         
#' @examples
#' # Assuming oc_solution contains multiple model runs:
#' stacked_cost_plot <- create_stacked_cost_plot(oc_solution)
#' # View the plot
#' print(stacked_cost_plot)
#' # Save the plot to a file
#' ggsave("stacked_cumulative_costs.png", stacked_cost_plot, width = 10, height = 6)
#'

create_stacked_cost_plot <- function(oc_solution) {
  
  # Step 1: Convert the nested list into a tibble
  # This creates a data frame where each row is one run
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    # Keep only the run_* entries (skipping other elements if any)
    filter(str_detect(run_id, "^run_"))
  
  # Step 2: Extract the needed variables from each run
  # Extract years and individual cost components
  cost_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      cost_mitig_cumul = map(run_list, "cost_mitig_cumul"),
      cost_remov_cumul = map(run_list, "cost_remov_cumul"),
      cost_resid_cumul = map(run_list, "cost_resid_cumul")
    ) %>%
    # Unnest to get one row per year per run
    select(run_id, years, cost_mitig_cumul, cost_remov_cumul, cost_resid_cumul) %>%
    unnest(cols = c(years, cost_mitig_cumul, cost_remov_cumul, cost_resid_cumul))
  
  # Step 3: Reshape the data to long format for stacked area plot
  cost_data_long <- cost_data %>%
    pivot_longer(
      cols = c(cost_mitig_cumul, cost_remov_cumul, cost_resid_cumul),
      names_to = "cost_type",
      values_to = "cost_value"
    ) %>%
    # Create more readable labels for cost types
    mutate(cost_type = case_when(
      cost_type == "cost_mitig_cumul" ~ "Mitigation",
      cost_type == "cost_remov_cumul" ~ "Removal",
      cost_type == "cost_resid_cumul" ~ "Residual",
      TRUE ~ cost_type
    ))
  
  # Step 4: Create the stacked area plot with consistent styling
  plot_stacked_cost <- ggplot(cost_data_long, 
                              aes(x = years, y = cost_value, 
                                  fill = cost_type, group = interaction(run_id, cost_type))) +
    geom_area(position = "stack", alpha = 0.7) +
    #facet_wrap(~ run_id) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Stacked Cumulative Costs Across Different Parameter Sets",
      x = "Year",
      y = "Cumulative Costs ($ Trillion)",
      fill = "Cost Component"
    ) +
    theme(
      legend.position = "bottom")
  
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

#' @title Create Combined Model Output Dashboard
#' @description
#' Creates a comprehensive dashboard with six plots arranged in a 2×3 grid showing key model outputs:
#' emissions, temperature, mitigation, removal, adjoint variable, and costs.
#' Uses a consistent theme and styling across all plots.
#' 
#' @param oc_solution List containing model output for different parameter sets
#' @param save_plot Logical indicating whether to save the plot (default: TRUE)
#' @param show_legend Logical indicating whether to display a legend (default: FALSE)
#' @param width Plot width in mm (default: 190)
#' @param height Plot height in mm (default: 260)
#' @param filename Custom filename (default: generated based on current time)
#'                   
#' @return A ggplot object combining all six plots in a 2×3 layout
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