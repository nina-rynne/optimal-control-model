#' @title Dynamic Programming Solution for Climate Optimal Control
#' @description
#' Implements backward induction dynamic programming to solve the climate
#' optimal control problem. This approach differs from Pontryagin's principle
#' by evaluating the entire state space and finding global optima, including
#' corner solutions.
#' 
#' @param parameter_df Data frame containing model parameters
#' @param emissions_df Data frame containing emissions scenario data
#' @param economic_df Data frame containing economic data
#' @param scenario Scenario string identifier
#' @param state_grid_size Number of discretized state values (default: 100)
#' @param control_grid_size Number of discretized control values (default: 25)
#' @param verbose Logical; whether to print progress (default: TRUE)
#' 
#' @return List containing optimal control paths and all state variables

climate_dynamic_programming <- function(parameter_df, 
                                        emissions_df, 
                                        economic_df, 
                                        scenario,
                                        state_grid_size = 100,
                                        control_grid_size = 25, 
                                        verbose = TRUE) {
  # Load required packages
  library(dplyr)
  library(tidyr)
  
  # Step 1: Create necessary vectors using your existing function
  vector_list <- create_vector_list(
    parameter_df = parameter_df,
    emissions_df = emissions_df,
    economic_df = economic_df,
    scenario = scenario
  )
  
  # Extract key variables and parameters
  years <- vector_list$years
  years_rel <- vector_list$years_rel
  n_years <- vector_list$n_years
  baseline_annual_emissions <- vector_list$baseline_annual_emissions
  baseline_annual_gwp <- vector_list$baseline_annual_gwp
  
  # Extract cost parameters
  clim_temp_init <- parameter_df$clim_temp_init
  tcre <- parameter_df$tcre
  cost_mitig_unit <- parameter_df$cost_mitig_unit
  cost_remov_unit <- parameter_df$cost_remov_unit
  econ_dam_pct <- parameter_df$econ_dam_pct
  disc_rate <- parameter_df$disc_rate
  exp_mitig <- parameter_df$exp_mitig
  exp_remov <- parameter_df$exp_remov
  exp_temp_anom <- parameter_df$exp_temp_anom
  
  # Terminal state target (if applicable)
  co2_target_2100 <- ifelse(!is.null(parameter_df$co2_target_2100), 
                            parameter_df$co2_target_2100, 
                            Inf)  # Use Inf if no target specified
  
  # Step 2: Set up state and control grids
  if (verbose) cat("Setting up state and control grids...\n")
  
  # Determine state bounds based on doing nothing and extreme CDR
  max_emissions <- vector_list$cumulative_emissions[1]
  for (i in 2:n_years) {
    max_emissions <- max_emissions + baseline_annual_emissions[i-1]
  }
  
  # State grid (cumulative emissions)
  # Start with initial emissions level and go up to max possible
  state_min <- vector_list$cumulative_emissions[1]
  state_max <- max(max_emissions, co2_target_2100 * 1.5)  # Allow some buffer
  state_grid <- seq(from = state_min, to = state_max, length.out = state_grid_size)
  
  # Control grids (mitigation and CDR)
  # Mitigation can be between 0 and baseline emissions
  mitig_grid <- list()
  for (t in 1:n_years) {
    mitig_grid[[t]] <- seq(from = 0, to = baseline_annual_emissions[t], 
                           length.out = control_grid_size)
  }
  
  # CDR can be between 0 and some maximum technical potential
  # Assuming max CDR is 100 GtCO2/yr (can be adjusted)
  max_cdr <- 100
  remov_grid <- seq(from = 0, to = max_cdr, length.out = control_grid_size)
  
  # Step 3: Initialize value function for final period
  if (verbose) cat("Initializing value function...\n")
  
  # Value function will store minimum cost-to-go from each state and time
  value_function <- array(Inf, dim = c(n_years, state_grid_size))
  
  # Optimal policy functions
  opt_mitig <- array(NA, dim = c(n_years, state_grid_size))
  opt_remov <- array(NA, dim = c(n_years, state_grid_size))
  
  # Terminal condition: penalize states based on distance from target
  # (if target exists, otherwise use cost of residual damage)
  for (s in 1:state_grid_size) {
    current_state <- state_grid[s]
    
    # If terminal target exists, penalize deviation
    if (is.finite(co2_target_2100)) {
      # Calculate penalty for missing target
      terminal_penalty <- 1e6 * abs(current_state - co2_target_2100)^2
      value_function[n_years, s] <- terminal_penalty
    } else {
      # No target, just use residual damage
      temp_anomaly <- clim_temp_init + ((current_state/1000) * tcre)
      residual_damage <- baseline_annual_gwp[n_years] * econ_dam_pct * 
        (temp_anomaly^exp_temp_anom) * exp(-disc_rate * years_rel[n_years])
      value_function[n_years, s] <- residual_damage
    }
  }
  
  # Step 4: Backward induction - core dynamic programming
  if (verbose) cat("Starting backward induction...\n")
  
  for (t in (n_years-1):1) {
    if (verbose && t %% 5 == 0) cat(sprintf("Processing time step %d of %d...\n", t, n_years-1))
    
    for (s in 1:state_grid_size) {
      current_state <- state_grid[s]
      
      # Initialize best cost and actions
      best_cost <- Inf
      best_mitig <- 0
      best_remov <- 0
      
      # Calculate temperature for current state
      temp_anomaly <- clim_temp_init + ((current_state/1000) * tcre)
      
      # Calculate residual damage for current state and time
      residual_damage <- baseline_annual_gwp[t] * econ_dam_pct * 
        (temp_anomaly^exp_temp_anom) * exp(-disc_rate * years_rel[t])
      
      # Loop through all possible control combinations
      for (m in 1:length(mitig_grid[[t]])) {
        mitig_action <- mitig_grid[[t]][m]
        
        # Skip if mitigation exceeds baseline emissions
        if (mitig_action > baseline_annual_emissions[t]) next
        
        # Calculate mitigation cost
        mitig_cost <- cost_mitig_unit * (mitig_action^exp_mitig) * 
          exp(-disc_rate * years_rel[t])
        
        for (r in 1:length(remov_grid)) {
          remov_action <- remov_grid[r]
          
          # Calculate removal cost
          remov_cost <- cost_remov_unit * (remov_action^exp_remov) * 
            exp(-disc_rate * years_rel[t])
          
          # Calculate net emissions and next state
          net_emissions <- baseline_annual_emissions[t] - mitig_action - remov_action
          next_state <- current_state + net_emissions
          
          # Skip if next state is outside bounds
          if (next_state < state_min || next_state > state_max) next
          
          # Find index of next state in grid (closest point)
          next_s <- which.min(abs(state_grid - next_state))
          
          # Calculate total cost: immediate cost + future cost
          immediate_cost <- mitig_cost + remov_cost + residual_damage
          future_cost <- value_function[t+1, next_s]
          
          total_cost <- immediate_cost + future_cost
          
          # Update best policy if this is better
          if (total_cost < best_cost) {
            best_cost <- total_cost
            best_mitig <- mitig_action
            best_remov <- remov_action
          }
        }
      }
      
      # Store optimal value and policy
      value_function[t, s] <- best_cost
      opt_mitig[t, s] <- best_mitig
      opt_remov[t, s] <- best_remov
    }
  }
  
  # Step 5: Forward simulation to recover optimal trajectory
  if (verbose) cat("Simulating optimal trajectory...\n")
  
  # Initialize result vectors
  optimal_emissions <- numeric(n_years)
  optimal_temperature <- numeric(n_years)
  optimal_mitig <- numeric(n_years)
  optimal_remov <- numeric(n_years)
  optimal_cost_mitig <- numeric(n_years)
  optimal_cost_remov <- numeric(n_years)
  optimal_cost_resid <- numeric(n_years)
  optimal_cost_total <- numeric(n_years)
  
  # Start with initial state
  optimal_emissions[1] <- vector_list$cumulative_emissions[1]
  optimal_temperature[1] <- clim_temp_init + ((optimal_emissions[1]/1000) * tcre)
  
  # Simulate forward through time
  for (t in 1:(n_years-1)) {
    # Find current state index (closest point in grid)
    current_s <- which.min(abs(state_grid - optimal_emissions[t]))
    
    # Get optimal controls for this state and time
    optimal_mitig[t] <- opt_mitig[t, current_s]
    optimal_remov[t] <- opt_remov[t, current_s]
    
    # Calculate costs
    optimal_cost_mitig[t] <- cost_mitig_unit * (optimal_mitig[t]^exp_mitig) * 
      exp(-disc_rate * years_rel[t])
    optimal_cost_remov[t] <- cost_remov_unit * (optimal_remov[t]^exp_remov) * 
      exp(-disc_rate * years_rel[t])
    optimal_cost_resid[t] <- baseline_annual_gwp[t] * econ_dam_pct * 
      (optimal_temperature[t]^exp_temp_anom) * exp(-disc_rate * years_rel[t])
    optimal_cost_total[t] <- optimal_cost_mitig[t] + optimal_cost_remov[t] + 
      optimal_cost_resid[t]
    
    # Update state for next period
    net_emissions <- baseline_annual_emissions[t] - optimal_mitig[t] - optimal_remov[t]
    optimal_emissions[t+1] <- optimal_emissions[t] + net_emissions
    optimal_temperature[t+1] <- clim_temp_init + ((optimal_emissions[t+1]/1000) * tcre)
  }
  
  # Calculate final period costs
  t <- n_years
  optimal_cost_mitig[t] <- 0  # No control in terminal period
  optimal_cost_remov[t] <- 0  # No control in terminal period
  optimal_cost_resid[t] <- baseline_annual_gwp[t] * econ_dam_pct * 
    (optimal_temperature[t]^exp_temp_anom) * exp(-disc_rate * years_rel[t])
  optimal_cost_total[t] <- optimal_cost_resid[t]
  
  # Calculate cumulative costs
  optimal_cost_mitig_cumul <- cumsum(optimal_cost_mitig)
  optimal_cost_remov_cumul <- cumsum(optimal_cost_remov)
  optimal_cost_resid_cumul <- cumsum(optimal_cost_resid)
  optimal_cost_total_cumul <- cumsum(optimal_cost_total)
  
  # Step 6: Build result list with same structure as forward-backward sweep
  if (verbose) cat("Building result list...\n")
  
  result <- list(
    # Time variables
    years = years,
    years_rel = years_rel,
    n_years = n_years,
    
    # State variables
    baseline_annual_emissions = baseline_annual_emissions,
    temperature_anomaly = optimal_temperature,
    cumulative_emissions = optimal_emissions,
    
    # Control variables
    qty_mitig = optimal_mitig,
    qty_remov = optimal_remov,
    
    # No adjoint variable in DP
    adjoint_var = rep(NA, n_years),
    
    # Cost components
    baseline_annual_gwp = baseline_annual_gwp,
    cost_mitig_cumul = optimal_cost_mitig_cumul,
    cost_remov_cumul = optimal_cost_remov_cumul,
    cost_resid_cumul = optimal_cost_resid_cumul,
    cost_total_cumul = optimal_cost_total_cumul,
    
    # Additional diagnostic information
    dp_value_function = value_function,
    dp_opt_mitig_policy = opt_mitig,
    dp_opt_remov_policy = opt_remov,
    converged = TRUE,
    error = FALSE,
    error_message = NULL
  )
  
  if (verbose) cat("Dynamic programming solution complete.\n")
  
  return(result)
}

#' @title Create Dashboard for Dynamic Programming Solution
#' @description
#' Creates a dashboard visualizing the results of the dynamic programming solution,
#' matching exactly the style of your existing create_combined_dashboard function.
#' 
#' @param dp_result The dynamic programming result list
#' @param save_plot Logical indicating whether to save the plot (default: TRUE)
#' @param show_legend Logical indicating whether to display a legend (default: FALSE)
#' @param width Plot width in mm (default: 190)
#' @param height Plot height in mm (default: 260)
#' @param filename Custom filename (default: generated based on current time)
#'                   
#' @return A ggplot object with layout matching create_combined_dashboard

create_dp_dashboard <- function(dp_result, 
                                save_plot = TRUE,
                                show_legend = FALSE,
                                width = 190,
                                height = 260,
                                filename = NULL) {
  
  # Required packages
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  
  # Define theme for consistent styling (exactly matching your existing theme)
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
  
  # Create a single-run list structure that the existing plot functions expect
  # Since dp_result is a single result, we'll wrap it in a list with a run_id
  wrapped_result <- list()
  wrapped_result[["run_dp"]] <- dp_result
  
  # Create the individual plots using your existing functions
  # 1. Emissions plot (top-left)
  p1 <- ggplot(data.frame(years = dp_result$years, 
                          cumulative_emissions = dp_result$cumulative_emissions),
               aes(x = years, y = cumulative_emissions)) +
    geom_line(color = "firebrick", linewidth = 1) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Cumulative Emissions",
         x = "Year",
         y = "Cumulative Emissions (GtC)")
  
  # 2. Temperature plot (top-right)
  p2 <- ggplot(data.frame(years = dp_result$years, 
                          temperature_anomaly = dp_result$temperature_anomaly),
               aes(x = years, y = temperature_anomaly)) +
    geom_line(color = "steelblue", linewidth = 1) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Temperature Anomaly",
         x = "Year",
         y = "Temperature Anomaly (°C)")
  
  # 3. Mitigation plot (middle-left)
  p3 <- ggplot(data.frame(years = dp_result$years, 
                          qty_mitig = dp_result$qty_mitig),
               aes(x = years, y = qty_mitig)) +
    geom_line(color = "forestgreen", linewidth = 1) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Mitigation Quantity",
         x = "Year",
         y = "Mitigation Quantity (GtC/yr)")
  
  # 4. Carbon Dioxide Removal plot (middle-right)
  p4 <- ggplot(data.frame(years = dp_result$years, 
                          qty_remov = dp_result$qty_remov),
               aes(x = years, y = qty_remov)) +
    geom_line(color = "purple4", linewidth = 1) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Carbon Dioxide Removal",
         x = "Year",
         y = "Carbon Dioxide Removal (GtC/yr)")
  
  # 5. Adjoint Variable plot (bottom-left)
  p5 <- ggplot(data.frame(years = dp_result$years, 
                          adjoint_var = dp_result$adjoint_var),
               aes(x = years, y = adjoint_var)) +
    geom_line(color = "darkorange", linewidth = 1) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Adjoint Variable",
         x = "Year",
         y = "Adjoint Variable Value")
  
  # 6. Create a blank plot for the bottom-right area, exactly as in your original
  p6 <- ggplot() + 
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA))
  
  # Combine all plots using patchwork, matching exactly your layout
  combined_plot <- (p1 + p2) /
    (p3 + p4) /
    (p5 + p6)
  
  # Add a shared legend if requested
  if (show_legend) {
    # Create a Legend
    legend_df <- data.frame(x = 1:5, y = 1:5, method = "Dynamic Programming")
    legend_plot <- ggplot(legend_df, aes(x=x, y=y, color=method)) +
      geom_line() +
      scale_color_manual(values=c("Dynamic Programming"="black")) +
      my_theme +
      theme(legend.position = "bottom")
    
    # Extract the legend
    legend <- cowplot::get_legend(legend_plot)
    
    # Add the legend as a separate row
    combined_plot <- combined_plot / 
      patchwork::wrap_elements(full = legend) +
      patchwork::plot_layout(heights = c(1, 1, 1, 0.2))
  }
  
  # Add title with total cost
  total_cost <- tail(dp_result$cost_total_cumul, 1)
  
  final_plot <- combined_plot + 
    patchwork::plot_annotation(
      title = "Dynamic Programming Solution",
      subtitle = paste("Total cost at end of period:", 
                       format(round(total_cost, 2), big.mark=","), "$ Trillion"),
      theme = theme(
        plot.margin = margin(10, 0, 10, 0),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5)
      )
    )
  
  # Save the plot if requested
  if (save_plot) {
    # Generate filename if not provided
    if (is.null(filename)) {
      filename <- paste0("dp_solution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
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