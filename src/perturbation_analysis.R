#' @title Perturbation Analysis for Climate Optimal Control
#' @description
#' This script contains functions for performing perturbation analysis on optimal control
#' solutions to verify optimality. It implements control variable perturbation and adjoint 
#' variable perturbation methods.
#'
#' @author Nina Rynne
#' @project Optimal Control of Temperature Overshoot
#' @date April 2025
#' @license MIT
#' @version 0.1.0
#'
#' @dependencies 
#' Required packages: dplyr, tidyr, here, ggplot2
#' 
#' This module provides the following key functions:
#' - perturb_control_variables: Perturbs optimal control trajectories and evaluates resulting costs
#' - perturb_adjoint_initial: Perturbs initial adjoint variable and evaluates resulting solutions
#' - visualize_perturbation_results: Creates plots showing the effects of perturbations
#'

#' @title Perturb Control Variables
#' @description
#' Applies small perturbations to the optimal control variables (mitigation and CDR)
#' and calculates the resulting cost to verify if the original solution is optimal.
#' 
#' @param optimal_solution List containing the optimal control solution
#' @param parameter_df Data frame containing model parameters
#' @param perturbation_pct Percentage perturbation to apply (default: 5)
#' @param n_perturbations Number of perturbation time points (default: 5)
#' @param symmetric Logical; whether to apply both positive and negative perturbations (default: TRUE)
#' @return Data frame with perturbation results and cost differences
#'
perturb_control_variables <- function(optimal_solution, 
                                      parameter_df,
                                      perturbation_pct = 5,
                                      n_perturbations = 5,
                                      symmetric = TRUE) {
  
  # Input validation
  if (!is.list(optimal_solution)) {
    stop("optimal_solution must be a list")
  }
  
  if (is.null(optimal_solution$qty_mitig) || is.null(optimal_solution$qty_remov)) {
    stop("optimal_solution must contain qty_mitig and qty_remov")
  }
  
  # Extract relevant vectors from optimal_solution
  years <- optimal_solution$years
  n_years <- optimal_solution$n_years
  qty_mitig <- optimal_solution$qty_mitig
  qty_remov <- optimal_solution$qty_remov
  baseline_annual_emissions <- optimal_solution$baseline_annual_emissions
  baseline_annual_gwp <- optimal_solution$baseline_annual_gwp
  
  # Calculate optimal cost
  optimal_cost_total <- utils::tail(optimal_solution$cost_total_cumul, 1)
  
  # Select time points for perturbation
  # Use evenly spaced indices (excluding first and last points)
  if (n_years <= n_perturbations + 2) {
    perturbation_indices <- 2:(n_years-1)
  } else {
    perturbation_indices <- round(seq(2, n_years-1, length.out = n_perturbations))
  }
  
  # Create data frame to store results
  results <- data.frame()
  
  # Determine perturbation magnitudes (either symmetric or just positive)
  perturbation_multipliers <- if (symmetric) c(-perturbation_pct/100, perturbation_pct/100) else perturbation_pct/100
  
  # Loop through each time point
  for (t_idx in perturbation_indices) {
    year_value <- years[t_idx]
    
    # Loop through each control variable (mitigation and removal)
    for (control_type in c("mitigation", "removal")) {
      
      # Get the original control values
      original_control <- if (control_type == "mitigation") qty_mitig else qty_remov
      
      # Loop through perturbation directions (positive and/or negative)
      for (perturb_mult in perturbation_multipliers) {
        
        # Create a copy of the optimal solution for modification
        perturbed_solution <- optimal_solution
        
        # Apply perturbation to the control variable at time t_idx
        # We'll create a "bump" perturbation centered at t_idx
        if (control_type == "mitigation") {
          # Calculate perturbation size (as a percentage of the current value)
          perturb_amount <- perturb_mult * original_control[t_idx]
          
          # Apply perturbation with bounds checking (can't be negative or exceed baseline)
          perturbed_solution$qty_mitig[t_idx] <- max(0, min(
            original_control[t_idx] + perturb_amount,
            baseline_annual_emissions[t_idx]
          ))
        } else { # removal
          # Calculate perturbation size (as a percentage of the current value or a minimum value)
          # For CDR, if current value is very small, use a minimum perturbation
          perturb_amount <- perturb_mult * max(original_control[t_idx], 0.1)
          
          # Apply perturbation with bounds checking (can't be negative)
          perturbed_solution$qty_remov[t_idx] <- max(0, original_control[t_idx] + perturb_amount)
        }
        
        # Run forward simulation with perturbed controls
        perturbed_solution <- evaluate_perturbed_cost(perturbed_solution, parameter_df)
        
        # Extract the new total cost
        perturbed_cost_total <- utils::tail(perturbed_solution$cost_total_cumul, 1)
        
        # Calculate cost difference (negative means perturbation improved solution)
        cost_difference <- perturbed_cost_total - optimal_cost_total
        cost_difference_pct <- (cost_difference / optimal_cost_total) * 100
        
        # Record results
        result_row <- data.frame(
          year = year_value,
          time_index = t_idx,
          control_type = control_type,
          perturbation_direction = if (perturb_mult > 0) "increase" else "decrease",
          perturbation_size_pct = perturb_mult * 100,
          original_value = original_control[t_idx],
          perturbed_value = if (control_type == "mitigation") 
            perturbed_solution$qty_mitig[t_idx] else
              perturbed_solution$qty_remov[t_idx],
          original_cost = optimal_cost_total,
          perturbed_cost = perturbed_cost_total,
          cost_difference = cost_difference,
          cost_difference_pct = cost_difference_pct
        )
        
        results <- rbind(results, result_row)
      }
    }
  }
  
  return(results)
}

#' @title Evaluate Costs with Perturbed Controls
#' @description
#' Forward-simulates the system with perturbed control variables and calculates
#' resulting costs. This is used internally by perturb_control_variables.
#' 
#' @param perturbed_solution List containing the solution with perturbed controls
#' @param parameter_df Data frame containing model parameters
#' @return Updated perturbed_solution with recalculated states and costs
#'
evaluate_perturbed_cost <- function(perturbed_solution, parameter_df) {
  
  # Extract required variables from parameter_df
  tcre <- parameter_df$tcre
  clim_temp_init <- parameter_df$clim_temp_init
  cost_mitig_unit <- parameter_df$cost_mitig_unit
  cost_remov_unit <- parameter_df$cost_remov_unit
  econ_dam_pct <- parameter_df$econ_dam_pct
  disc_rate <- parameter_df$disc_rate
  exp_mitig <- parameter_df$exp_mitig  
  exp_remov <- parameter_df$exp_remov
  exp_temp_anom <- parameter_df$exp_temp_anom
  
  # Extract vectors from perturbed_solution
  years <- perturbed_solution$years
  years_rel <- perturbed_solution$years_rel
  n_years <- perturbed_solution$n_years
  baseline_annual_emissions <- perturbed_solution$baseline_annual_emissions
  baseline_annual_gwp <- perturbed_solution$baseline_annual_gwp
  qty_mitig <- perturbed_solution$qty_mitig
  qty_remov <- perturbed_solution$qty_remov
  
  # Reset state variables
  cumulative_emissions <- rep(0, n_years)
  temperature_anomaly <- rep(NA, n_years)
  
  # Set initial state
  cumulative_emissions[1] <- baseline_annual_emissions[1]
  temperature_anomaly[1] <- clim_temp_init + ((cumulative_emissions[1]/1000) * tcre)
  
  # Forward sweep with perturbed controls to update states
  for (i in 2:n_years) {
    # Calculate net emissions rate (could be negative with CDR)
    annual_net_emissions <- baseline_annual_emissions[i-1] - qty_mitig[i-1] - qty_remov[i-1]
    
    # Update cumulative emissions
    cumulative_emissions[i] <- cumulative_emissions[i-1] + annual_net_emissions
    
    # Update temperature based on cumulative emissions
    temperature_anomaly[i] <- clim_temp_init + ((cumulative_emissions[i]/1000) * tcre)
  }
  
  # Update perturbed_solution with new state variables
  perturbed_solution$cumulative_emissions <- cumulative_emissions
  perturbed_solution$temperature_anomaly <- temperature_anomaly
  
  # Recalculate costs with the new state and control trajectories
  # Ensure we use positive values for qty_mitig and qty_remov to avoid numerical issues
  safe_qty_mitig <- pmax(qty_mitig, 0)
  safe_qty_remov <- pmax(qty_remov, 0)
  safe_temp <- pmax(temperature_anomaly, 0)
  
  # Calculate individual cost components
  cost_mitig <- (cost_mitig_unit * safe_qty_mitig^exp_mitig) * exp(-disc_rate*years_rel)
  cost_remov <- (cost_remov_unit * safe_qty_remov^exp_remov) * exp(-disc_rate*years_rel)
  cost_resid <- (baseline_annual_gwp * econ_dam_pct * ((safe_temp)^exp_temp_anom)) * exp(-disc_rate*years_rel)
  
  # Cumulative costs
  cost_mitig_cumul <- cumsum(cost_mitig)
  cost_remov_cumul <- cumsum(cost_remov)
  cost_resid_cumul <- cumsum(cost_resid)
  cost_total_cumul <- cost_mitig_cumul + cost_remov_cumul + cost_resid_cumul
  
  # Update cost components in perturbed_solution
  perturbed_solution$cost_mitig_cumul <- cost_mitig_cumul
  perturbed_solution$cost_remov_cumul <- cost_remov_cumul
  perturbed_solution$cost_resid_cumul <- cost_resid_cumul
  perturbed_solution$cost_total_cumul <- cost_total_cumul
  
  return(perturbed_solution)
}

#' @title Visualize Perturbation Results
#' @description
#' Creates plots to visualize the results of perturbation analysis, showing
#' how cost changes with different perturbations to validate optimality.
#' 
#' @param perturbation_results Data frame with perturbation analysis results from perturb_control_variables
#' @param save_path Optional path to save the plots (default: NULL, no saving)
#' @param plot_height Height of the plot in inches (default: 7)
#' @param plot_width Width of the plot in inches (default: 10)
#' @return List of ggplot objects with the visualizations
#'
visualize_perturbation_results <- function(perturbation_results,
                                           save_path = NULL,
                                           plot_height = 7,
                                           plot_width = 10) {
  
  # Ensure required packages are loaded
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is needed for this function to work. Please install it.")
  }
  
  # Create a list to store plot objects
  plots <- list()
  
  # 1. Cost change by year and control type
  plots$by_year <- ggplot2::ggplot(perturbation_results, 
                                   ggplot2::aes(x = year, 
                                                y = cost_difference_pct, 
                                                color = control_type,
                                                shape = perturbation_direction)) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::facet_wrap(~ control_type) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::labs(
      title = "Cost Change from Perturbations by Year",
      subtitle = "Positive values indicate perturbations increase cost (solution is optimal)",
      x = "Year",
      y = "Cost Change (%)",
      color = "Control Type",
      shape = "Direction"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  
  # 2. Cost change distribution
  plots$distribution <- ggplot2::ggplot(perturbation_results, 
                                        ggplot2::aes(x = cost_difference_pct, 
                                                     fill = control_type)) +
    ggplot2::geom_histogram(position = "dodge", bins = 20, alpha = 0.7) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::labs(
      title = "Distribution of Cost Changes from Perturbations",
      subtitle = "Optimal solution should show predominantly positive cost changes",
      x = "Cost Change (%)",
      y = "Count",
      fill = "Control Type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  
  # 3. Cost change by perturbation size
  plots$by_size <- ggplot2::ggplot(perturbation_results, 
                                   ggplot2::aes(x = perturbation_size_pct, 
                                                y = cost_difference_pct, 
                                                color = control_type,
                                                shape = factor(time_index))) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::labs(
      title = "Cost Change vs. Perturbation Size",
      subtitle = "Relationship between perturbation magnitude and cost impact",
      x = "Perturbation Size (%)",
      y = "Cost Change (%)",
      color = "Control Type",
      shape = "Time Index"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  
  # Save plots if a path is provided
  if (!is.null(save_path)) {
    # Ensure the directory exists
    dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    
    # Save each plot
    for (plot_name in names(plots)) {
      file_path <- file.path(dirname(save_path), 
                             paste0(basename(save_path), "_", plot_name, ".pdf"))
      
      ggplot2::ggsave(
        filename = file_path,
        plot = plots[[plot_name]],
        height = plot_height,
        width = plot_width,
        device = cairo_pdf
      )
    }
  }
  
  return(plots)
}

#' @title Perturb Adjoint Initial Value
#' @description
#' Varies the initial adjoint variable (transversality condition) and evaluates
#' the resulting solutions to verify sensitivity and optimality.
#' 
#' @param parameter_df Data frame containing model parameters
#' @param vector_list List of vectors required for optimal control calculation
#' @param trans_value Converged transversality value from shooting method
#' @param perturbation_range Percentage range around trans_value to test (default: 10)
#' @param n_points Number of test points within range (default: 5)
#' @return Data frame with results of adjoint perturbation analysis
#'
perturb_adjoint_initial <- function(parameter_df,
                                    vector_list,
                                    trans_value,
                                    perturbation_range = 10,
                                    n_points = 5) {
  
  # Calculate perturbation range
  delta <- trans_value * (perturbation_range / 100)
  
  # Generate test points
  if (trans_value == 0) {
    # Handle the case where trans_value is zero
    test_values <- seq(-delta, delta, length.out = n_points)
  } else {
    # Regular case with non-zero trans_value
    test_values <- seq(trans_value - delta, trans_value + delta, length.out = n_points)
  }
  
  # Create data frame to store results
  results <- data.frame()
  
  # Run forward-backward sweep for each test value
  for (test_val in test_values) {
    # Create a copy of parameter_df to avoid modifying the original
    test_params <- parameter_df
    
    # Source the forward-backward sweep function (assuming it's available)
    # Run the sweep with the test transversality value
    result <- forward_backward_sweep(test_params, vector_list, test_val)
    
    # Extract key metrics
    if (!result$error) {
      # Extract values for successful runs
      final_emission <- utils::tail(result$cumulative_emissions, 1)
      final_temp <- utils::tail(result$temperature_anomaly, 1)
      total_cost <- utils::tail(result$cost_total_cumul, 1)
      mitig_cost <- utils::tail(result$cost_mitig_cumul, 1)
      remov_cost <- utils::tail(result$cost_remov_cumul, 1)
      resid_cost <- utils::tail(result$cost_resid_cumul, 1)
      converged <- result$converged
      iterations <- result$iterations
      
      # Store the results
      result_row <- data.frame(
        trans_value = test_val,
        perturbation_pct = ((test_val - trans_value) / trans_value) * 100,
        final_emission = final_emission,
        final_temp = final_temp,
        total_cost = total_cost,
        mitig_cost = mitig_cost,
        remov_cost = remov_cost,
        resid_cost = resid_cost,
        converged = converged,
        iterations = iterations,
        error = FALSE
      )
    } else {
      # Handle error cases
      result_row <- data.frame(
        trans_value = test_val,
        perturbation_pct = ((test_val - trans_value) / trans_value) * 100,
        final_emission = NA,
        final_temp = NA,
        total_cost = NA,
        mitig_cost = NA,
        remov_cost = NA,
        resid_cost = NA,
        converged = FALSE,
        iterations = NA,
        error = TRUE,
        error_message = result$error_message
      )
    }
    
    results <- rbind(results, result_row)
  }
  
  return(results)
}

#' @title Visualize Adjoint Perturbation Results
#' @description
#' Creates plots to visualize how varying the initial adjoint value affects
#' key outcomes like final emissions, temperature, and costs.
#' 
#' @param adjoint_results Data frame with results from perturb_adjoint_initial
#' @param save_path Optional path to save the plots (default: NULL, no saving)
#' @param plot_height Height of the plot in inches (default: 7)
#' @param plot_width Width of the plot in inches (default: 10)
#' @return List of ggplot objects with the visualizations
#'
visualize_adjoint_results <- function(adjoint_results,
                                      save_path = NULL,
                                      plot_height = 7,
                                      plot_width = 10) {
  
  # Ensure required packages are loaded
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is needed for this function to work. Please install it.")
  }
  
  # Remove rows with errors for plotting
  plot_data <- adjoint_results[!adjoint_results$error, ]
  
  if (nrow(plot_data) == 0) {
    warning("No valid data points to plot. All runs resulted in errors.")
    return(NULL)
  }
  
  # Create a list to store plot objects
  plots <- list()
  
  # 1. Final emissions vs transversality value
  plots$emissions <- ggplot2::ggplot(plot_data, 
                                     ggplot2::aes(x = trans_value, y = final_emission)) +
    ggplot2::geom_point(size = 3, color = "steelblue") +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::labs(
      title = "Final Cumulative Emissions vs Transversality Value",
      x = "Transversality Value",
      y = "Final Cumulative Emissions"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
  
  # 2. Final temperature vs transversality value
  plots$temperature <- ggplot2::ggplot(plot_data, 
                                       ggplot2::aes(x = trans_value, y = final_temp)) +
    ggplot2::geom_point(size = 3, color = "firebrick") +
    ggplot2::geom_line(color = "firebrick") +
    ggplot2::labs(
      title = "Final Temperature vs Transversality Value",
      x = "Transversality Value",
      y = "Final Temperature (°C)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
  
  # 3. Costs vs transversality value
  # Reshape data for plotting different cost components
  costs_data <- tidyr::pivot_longer(
    plot_data,
    cols = c("total_cost", "mitig_cost", "remov_cost", "resid_cost"),
    names_to = "cost_type",
    values_to = "cost_value"
  )
  
  plots$costs <- ggplot2::ggplot(costs_data, 
                                 ggplot2::aes(x = trans_value, y = cost_value, 
                                              color = cost_type)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line() +
    ggplot2::scale_color_brewer(palette = "Set1", 
                                labels = c("Mitigation", "Removal", "Residual", "Total")) +
    ggplot2::labs(
      title = "Costs vs Transversality Value",
      x = "Transversality Value",
      y = "Cost",
      color = "Cost Type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
  
  # 4. Number of iterations vs transversality value
  plots$iterations <- ggplot2::ggplot(plot_data, 
                                      ggplot2::aes(x = trans_value, y = iterations)) +
    ggplot2::geom_point(size = 3, color = "darkgreen") +
    ggplot2::geom_line(color = "darkgreen") +
    ggplot2::labs(
      title = "Iterations Required vs Transversality Value",
      x = "Transversality Value",
      y = "Number of Iterations"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
  
  # Save plots if a path is provided
  if (!is.null(save_path)) {
    # Ensure the directory exists
    dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    
    # Save each plot
    for (plot_name in names(plots)) {
      file_path <- file.path(dirname(save_path), 
                             paste0(basename(save_path), "_adjoint_", plot_name, ".pdf"))
      
      ggplot2::ggsave(
        filename = file_path,
        plot = plots[[plot_name]],
        height = plot_height,
        width = plot_width,
        device = cairo_pdf
      )
    }
  }
  
  return(plots)
}

#' @title Run Complete Perturbation Analysis
#' @description
#' Performs a comprehensive perturbation analysis on an optimal control solution,
#' combining both control variable perturbation and adjoint variable perturbation.
#' 
#' @param optimal_solution List containing the optimal control solution
#' @param parameter_df Data frame containing model parameters
#' @param vector_list List of vectors required for optimal control calculation
#' @param save_path Optional path to save the plots (default: NULL, no saving)
#' @param control_perturbation_pct Percentage perturbation for control variables (default: 5)
#' @param adjoint_perturbation_range Percentage range for adjoint perturbation (default: 10)
#' @return List with results of all perturbation analyses
#'
run_perturbation_analysis <- function(optimal_solution,
                                      parameter_df,
                                      vector_list,
                                      save_path = NULL,
                                      control_perturbation_pct = 5,
                                      adjoint_perturbation_range = 10) {
  
  # 1. Control variable perturbation
  control_results <- perturb_control_variables(
    optimal_solution = optimal_solution,
    parameter_df = parameter_df,
    perturbation_pct = control_perturbation_pct
  )
  
  # 2. Visualize control perturbation results
  control_plots <- visualize_perturbation_results(
    perturbation_results = control_results,
    save_path = if (!is.null(save_path)) file.path(save_path, "control_perturbation") else NULL
  )
  
  # 3. Adjoint variable perturbation (if trans_value is available)
  adjoint_results <- NULL
  adjoint_plots <- NULL
  
  if (!is.null(optimal_solution$trans_value)) {
    adjoint_results <- perturb_adjoint_initial(
      parameter_df = parameter_df,
      vector_list = vector_list,
      trans_value = optimal_solution$trans_value,
      perturbation_range = adjoint_perturbation_range
    )
    
    # 4. Visualize adjoint perturbation results
    adjoint_plots <- visualize_adjoint_results(
      adjoint_results = adjoint_results,
      save_path = if (!is.null(save_path)) file.path(save_path, "adjoint_perturbation") else NULL
    )
  }
  
  # Summarize results
  summary <- summarize_perturbation_results(control_results, adjoint_results)
  
  # Return all results and plots
  return(list(
    control_perturbation = control_results,
    control_plots = control_plots,
    adjoint_perturbation = adjoint_results,
    adjoint_plots = adjoint_plots,
    summary = summary
  ))
}

#' @title Summarize Perturbation Analysis Results
#' @description
#' Creates a summary of perturbation analysis results, highlighting key findings
#' about optimality and sensitivity.
#' 
#' @param control_results Data frame with control perturbation results
#' @param adjoint_results Data frame with adjoint perturbation results
#' @return List with summary statistics and recommendations
#'
summarize_perturbation_results <- function(control_results, adjoint_results = NULL) {
  
  # Summary for control perturbation
  control_summary <- list()
  
  if (!is.null(control_results) && nrow(control_results) > 0) {
    # Calculate percentage of perturbations that increased cost
    pct_increased_cost <- mean(control_results$cost_difference > 0) * 100
    
    # Find the most sensitive control type and time point
    max_impact <- control_results[which.max(abs(control_results$cost_difference_pct)), ]
    
    # Separate analysis for mitigation and removal
    mitigation_results <- control_results[control_results$control_type == "mitigation", ]
    removal_results <- control_results[control_results$control_type == "removal", ]
    
    pct_mitigation_increased <- mean(mitigation_results$cost_difference > 0) * 100
    pct_removal_increased <- mean(removal_results$cost_difference > 0) * 100
    
    # Create summary
    control_summary <- list(
      total_perturbations = nrow(control_results),
      pct_increased_cost = pct_increased_cost,
      pct_mitigation_increased = pct_mitigation_increased,
      pct_removal_increased = pct_removal_increased,
      max_impact = max_impact,
      avg_cost_difference_pct = mean(control_results$cost_difference_pct),
      sd_cost_difference_pct = sd(control_results$cost_difference_pct),
      min_cost_difference_pct = min(control_results$cost_difference_pct),
      max_cost_difference_pct = max(control_results$cost_difference_pct)
    )
    
    # Add optimality assessment
    if (pct_increased_cost >= 95) {
      control_summary$optimality_assessment <- "Strong evidence of optimality: Over 95% of perturbations increased cost."
    } else if (pct_increased_cost >= 80) {
      control_summary$optimality_assessment <- "Good evidence of optimality: 80-95% of perturbations increased cost."
    } else if (pct_increased_cost >= 60) {
      control_summary$optimality_assessment <- "Moderate evidence of optimality: 60-80% of perturbations increased cost."
    } else {
      control_summary$optimality_assessment <- "Weak evidence of optimality: Less than 60% of perturbations increased cost."
    }
  }
  
  # Summary for adjoint perturbation
  adjoint_summary <- list()
  
  if (!is.null(adjoint_results) && nrow(adjoint_results) > 0) {
    # Filter out error cases
    valid_results <- adjoint_results[!adjoint_results$error, ]
    
    if (nrow(valid_results) > 0) {
      # Calculate ranges for key metrics
      emissions_range <- range(valid_results$final_emission)
      temp_range <- range(valid_results$final_temp)
      cost_range <- range(valid_results$total_cost)
      
      # Calculate sensitivity metrics
      emissions_sensitivity <- diff(emissions_range) / mean(emissions_range) * 100
      temp_sensitivity <- diff(temp_range) / mean(temp_range) * 100
      cost_sensitivity <- diff(cost_range) / mean(cost_range) * 100
      
      # Find optimal transversality value (lowest cost)
      optimal_trans <- valid_results$trans_value[which.min(valid_results$total_cost)]
      
      # Create summary
      adjoint_summary <- list(
        total_tests = nrow(adjoint_results),
        valid_tests = nrow(valid_results),
        error_tests = sum(adjoint_results$error),
        emissions_range = emissions_range,
        temp_range = temp_range,
        cost_range = cost_range,
        emissions_sensitivity_pct = emissions_sensitivity,
        temp_sensitivity_pct = temp_sensitivity,
        cost_sensitivity_pct = cost_sensitivity,
        optimal_trans_value = optimal_trans
      )
      
      # Add sensitivity assessment
      if (cost_sensitivity < 1) {
        adjoint_summary$sensitivity_assessment <- "Low sensitivity to adjoint perturbation: Solution is robust."
      } else if (cost_sensitivity < 5) {
        adjoint_summary$sensitivity_assessment <- "Moderate sensitivity to adjoint perturbation: Solution is generally stable."
      } else {
        adjoint_summary$sensitivity_assessment <- "High sensitivity to adjoint perturbation: Solution is sensitive to initial conditions."
      }
    }
  }
  
  # Combined summary and recommendations
  overall_summary <- list(
    control_summary = control_summary,
    adjoint_summary = adjoint_summary
  )
  
  # Create overall assessment
  if (length(control_summary) > 0) {
    if (!is.null(control_summary$optimality_assessment)) {
      if (grepl("Strong evidence", control_summary$optimality_assessment)) {
        overall_summary$overall_assessment <- "High confidence in solution optimality."
      } else if (grepl("Good evidence", control_summary$optimality_assessment)) {
        overall_summary$overall_assessment <- "Good confidence in solution optimality."
      } else {
        overall_summary$overall_assessment <- "Limited confidence in solution optimality."
      }
    }
    
    # Add recommendations
    if (is.null(adjoint_summary) || length(adjoint_summary) == 0) {
      overall_summary$recommendations <- "Consider running adjoint variable perturbation analysis for a more complete assessment."
    } else if (!is.null(adjoint_summary$sensitivity_assessment)) {
      if (grepl("High sensitivity", adjoint_summary$sensitivity_assessment)) {
        overall_summary$recommendations <- "Solution is sensitive to initial conditions. Consider using multiple starting points or refining convergence criteria."
      } else if (control_summary$pct_increased_cost < 80) {
        overall_summary$recommendations <- "Solution may not be optimal. Consider refining the model or using alternative solution methods."
      } else {
        overall_summary$recommendations <- "Solution appears optimal and robust."
      }
    }
  }
  
  return(overall_summary)
}