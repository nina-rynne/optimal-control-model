#' @title Optimised Delayed Deployment Analysis for Climate Optimal Control
#' @description
#' High-performance implementation of delayed deployment analysis with minimal
#' function call overhead and pre-filtered data. Designed to match the speed
#' of the original step6_delayed_deployment.R while maintaining clean code structure.
#'
#' This script combines all performance optimisations:
#' - Single data filtering operation
#' - Streamlined function calls
#' - Efficient memory management
#' - Optional parallel processing only when beneficial
#'
#' @author Nina Rynne
#' @project Optimal Control of Temperature Overshoot  
#' @date June 2025
#' @version 1.0.0

library(dplyr)
library(ggplot2)
library(viridis)
library(foreach)
library(doParallel)
library(parallel)

#' @title Fast Forward-Backward Sweep for Delayed Deployment
#' @description
#' Streamlined implementation of the core algorithm specifically optimised for
#' delay analysis. Minimal function call overhead and pre-computed constants.
#'
#' @param parameters List of model parameters (pre-extracted from data frame)
#' @param baseline_emissions Pre-filtered baseline emissions vector
#' @param baseline_gwp Pre-filtered baseline economic data vector
#' @param years Pre-filtered years vector
#' @param target_emissions Target cumulative emissions constraint
#' @param terminal_adjoint Terminal adjoint value from shooting method
#' @param mitigation_start_idx Index where mitigation deployment begins
#' @param cdr_start_idx Index where CDR deployment begins
#' @param max_iterations Maximum iterations (default: 500)
#' @param tolerance Convergence tolerance (default: 0.001)
#' @param epsilon Strict inequality parameter (default: 0.01)
#'
#' @return List with convergence info and key results
fast_forward_backward_sweep <- function(parameters,
                                        baseline_emissions,
                                        baseline_gwp,
                                        years,
                                        target_emissions,
                                        terminal_adjoint,
                                        mitigation_start_idx,
                                        cdr_start_idx,
                                        max_iterations = 500,
                                        tolerance = 0.001,
                                        epsilon = 0.01) {
  
  # Pre-extract parameters to avoid repeated lookups
  cost_mitig_unit <- parameters$cost_mitig_unit
  cost_remov_unit <- parameters$cost_remov_unit
  exp_mitig <- parameters$exp_mitig
  exp_remov <- parameters$exp_remov
  exp_temp_anom <- parameters$exp_temp_anom
  clim_temp_init <- parameters$clim_temp_init
  tcre <- parameters$tcre
  econ_dam_pct <- parameters$econ_dam_pct
  disc_rate <- parameters$disc_rate
  
  # Pre-compute constants
  n_years <- length(years)
  years_rel <- years - years[1]
  dt <- 1
  update_weight <- 0.05
  tcre_scaled <- tcre / 1000
  
  # Pre-compute discount factors (avoid repeated exp() calls)
  discount_factors <- exp(-disc_rate * years_rel)
  
  # Initialize variables
  cumulative_emissions <- numeric(n_years)
  temperature_anomaly <- rep(clim_temp_init, n_years)
  qty_mitig <- rep(1, n_years)
  qty_remov <- rep(0.1, n_years)
  adjoint_var <- numeric(n_years)
  adjoint_var[n_years] <- terminal_adjoint
  
  # Pre-compute deployment masks for efficiency
  mitig_active <- seq_len(n_years) >= mitigation_start_idx
  cdr_active <- seq_len(n_years) >= cdr_start_idx
  
  # Forward-Backward Sweep Loop
  for (iter in seq_len(max_iterations)) {
    
    # Store previous values (only what we need for convergence)
    prev_mitig <- qty_mitig
    prev_remov <- qty_remov
    prev_cumulative <- cumulative_emissions
    prev_adjoint <- adjoint_var
    
    # === FORWARD SWEEP ===
    # Vectorised cumulative sum for efficiency
    annual_net <- baseline_emissions - qty_mitig - qty_remov
    cumulative_emissions[1] <- annual_net[1]
    for (i in 2:n_years) {
      cumulative_emissions[i] <- cumulative_emissions[i-1] + annual_net[i]
    }
    
    # Vectorised temperature calculation
    temperature_anomaly <- pmax(clim_temp_init + cumulative_emissions * tcre_scaled, 0.1)
    
    # === BACKWARD SWEEP ===
    # Vectorised backward integration
    for (i in (n_years-1):1) {
      temp_base <- temperature_anomaly[i]
      adjoint_derivative <- -(exp_temp_anom * baseline_gwp[i] * econ_dam_pct * 
                                tcre_scaled * discount_factors[i] * 
                                (temp_base^(exp_temp_anom - 1)))
      
      if (is.finite(adjoint_derivative)) {
        adjoint_var[i] <- adjoint_var[i+1] - adjoint_derivative
      } else {
        adjoint_var[i] <- adjoint_var[i+1]
      }
    }
    
    # === UPDATE CONTROLS ===
    # Vectorised control updates with pre-computed masks
    new_mitig <- numeric(n_years)
    new_remov <- numeric(n_years)
    
    # Mitigation control (only where active)
    mitig_indices <- which(mitig_active & adjoint_var > 0)
    if (length(mitig_indices) > 0) {
      mitig_denominator <- exp_mitig * cost_mitig_unit * discount_factors[mitig_indices]
      um_unconstrained <- adjoint_var[mitig_indices] / mitig_denominator
      
      # Apply bounds with strict inequality
      um_bounded <- pmax(0, pmin(um_unconstrained, baseline_emissions[mitig_indices] - epsilon))
      new_mitig[mitig_indices] <- um_bounded
    }
    
    # CDR control (only where active)
    cdr_indices <- which(cdr_active & adjoint_var > 0)
    if (length(cdr_indices) > 0) {
      remov_denominator <- exp_remov * cost_remov_unit * discount_factors[cdr_indices]
      ur_unconstrained <- adjoint_var[cdr_indices] / remov_denominator
      new_remov[cdr_indices] <- pmax(0, ur_unconstrained)
    }
    
    # Smoothed updates
    qty_mitig <- update_weight * new_mitig + (1 - update_weight) * prev_mitig
    qty_remov <- update_weight * new_remov + (1 - update_weight) * prev_remov
    
    # Fast convergence check
    total_change <- sum(abs(qty_mitig - prev_mitig)) + 
      sum(abs(qty_remov - prev_remov)) +
      sum(abs(cumulative_emissions - prev_cumulative)) +
      sum(abs(adjoint_var - prev_adjoint))
    
    if (total_change < tolerance) {
      break
    }
  }
  
  # Calculate final metrics
  final_emissions <- cumulative_emissions[n_years]
  peak_temperature <- max(temperature_anomaly)
  years_above_1p5 <- sum(temperature_anomaly > 1.5)
  total_cdr_units <- sum(qty_remov)
  
  # Fast cost calculation
  mitig_costs <- sum(cost_mitig_unit * qty_mitig^exp_mitig * discount_factors)
  remov_costs <- sum(cost_remov_unit * qty_remov^exp_remov * discount_factors)
  temp_costs <- sum(baseline_gwp * econ_dam_pct * (temperature_anomaly^exp_temp_anom) * discount_factors)
  
  return(list(
    converged = (total_change < tolerance),
    iterations = iter,
    final_emissions = final_emissions,
    emission_gap = final_emissions - target_emissions,
    peak_temperature = peak_temperature,
    total_cost = mitig_costs + remov_costs + temp_costs,
    mitig_cost = mitig_costs,
    remov_cost = remov_costs,
    temp_cost = temp_costs,
    years_above_1p5 = years_above_1p5,
    total_cdr_units = total_cdr_units
  ))
}

#' @title Fast Shooting Method for Delayed Deployment
#' @description
#' Streamlined shooting method with reduced iterations for delay analysis efficiency.
#' Pre-filters data and uses optimised parameter passing.
#'
#' @param parameters List of pre-extracted model parameters
#' @param baseline_emissions Pre-filtered baseline emissions vector
#' @param baseline_gwp Pre-filtered baseline economic data vector  
#' @param years Pre-filtered years vector
#' @param target_emissions Target cumulative emissions constraint
#' @param mitigation_delay_years Years to delay mitigation deployment
#' @param cdr_delay_years Years to delay CDR deployment
#' @param max_iterations Reduced iteration limit for efficiency (default: 50)
#' @param tolerance Relaxed tolerance for efficiency (default: 3.0)
#'
#' @return Best result found within iteration limit
fast_shooting_method <- function(parameters,
                                 baseline_emissions,
                                 baseline_gwp, 
                                 years,
                                 target_emissions,
                                 mitigation_delay_years,
                                 cdr_delay_years,
                                 max_iterations = 50,
                                 tolerance = 3.0) {
  
  # Calculate deployment start indices (more efficient than year comparisons)
  mitigation_start_idx <- mitigation_delay_years + 1
  cdr_start_idx <- cdr_delay_years + 1
  
  # Initial bounds - matched to original working values
  lambda_low <- 0
  lambda_high <- 5000
  
  # Test bounds
  result_low <- fast_forward_backward_sweep(
    parameters, baseline_emissions, baseline_gwp, years, target_emissions, 
    lambda_low, mitigation_start_idx, cdr_start_idx
  )
  
  result_high <- fast_forward_backward_sweep(
    parameters, baseline_emissions, baseline_gwp, years, target_emissions,
    lambda_high, mitigation_start_idx, cdr_start_idx
  )
  
  # Handle convergence failures gracefully
  if (!result_low$converged || !result_high$converged) {
    best_result <- if (abs(result_low$emission_gap) < abs(result_high$emission_gap)) result_low else result_high
    return(list(
      converged = best_result$converged,
      iterations = best_result$iterations,
      final_emissions = best_result$final_emissions,
      emission_gap = best_result$emission_gap,
      peak_temperature = best_result$peak_temperature,
      total_cost = best_result$total_cost,
      mitig_cost = best_result$mitig_cost,
      remov_cost = best_result$remov_cost,
      temp_cost = best_result$temp_cost,
      years_above_1p5 = best_result$years_above_1p5,
      total_cdr_units = best_result$total_cdr_units
    ))
  }
  
  emission_gap_low <- result_low$emission_gap
  emission_gap_high <- result_high$emission_gap
  
  # Adjust bounds if needed
  if (sign(emission_gap_low) == sign(emission_gap_high)) {
    if (emission_gap_low > 0 && emission_gap_high > 0) {
      lambda_low <- lambda_low / 2
      lambda_high <- lambda_low + 1000
    } else {
      lambda_high <- lambda_high * 2
      lambda_low <- lambda_high - 1000
    }
  }
  
  # Main shooting loop with tracking of best result
  best_result <- NULL
  best_gap <- Inf
  
  for (iteration in seq_len(max_iterations)) {
    
    # Secant method update with numerical safety
    if (abs(emission_gap_high - emission_gap_low) < 1e-10) break
    
    lambda_new <- lambda_low - emission_gap_low * (lambda_high - lambda_low) / 
      (emission_gap_high - emission_gap_low)
    
    result_new <- fast_forward_backward_sweep(
      parameters, baseline_emissions, baseline_gwp, years, target_emissions,
      lambda_new, mitigation_start_idx, cdr_start_idx
    )
    
    # Fallback to bisection if convergence fails
    if (!result_new$converged) {
      lambda_new <- (lambda_low + lambda_high) / 2
      result_new <- fast_forward_backward_sweep(
        parameters, baseline_emissions, baseline_gwp, years, target_emissions,
        lambda_new, mitigation_start_idx, cdr_start_idx
      )
    }
    
    emission_gap_new <- result_new$emission_gap
    
    # Track best result
    if (abs(emission_gap_new) < abs(best_gap)) {
      best_result <- result_new
      best_gap <- emission_gap_new
    }
    
    # Check convergence
    if (abs(emission_gap_new) <= tolerance) {
      return(list(
        converged = result_new$converged,
        iterations = result_new$iterations,
        final_emissions = result_new$final_emissions,
        emission_gap = result_new$emission_gap,
        peak_temperature = result_new$peak_temperature,
        total_cost = result_new$total_cost,
        mitig_cost = result_new$mitig_cost,
        remov_cost = result_new$remov_cost,
        temp_cost = result_new$temp_cost,
        years_above_1p5 = result_new$years_above_1p5,
        total_cdr_units = result_new$total_cdr_units
      ))
    }
    
    # Update bounds
    if (sign(emission_gap_new) == sign(emission_gap_low)) {
      lambda_low <- lambda_new
      emission_gap_low <- emission_gap_new
    } else {
      lambda_high <- lambda_new
      emission_gap_high <- emission_gap_new
    }
  }
  
  # Return best result found if no convergence achieved
  return(list(
    converged = best_result$converged,
    iterations = best_result$iterations,
    final_emissions = best_result$final_emissions,
    emission_gap = best_result$emission_gap,
    peak_temperature = best_result$peak_temperature,
    total_cost = best_result$total_cost,
    mitig_cost = best_result$mitig_cost,
    remov_cost = best_result$remov_cost,
    temp_cost = best_result$temp_cost,
    years_above_1p5 = best_result$years_above_1p5,
    total_cdr_units = best_result$total_cdr_units
  ))
}

#' @title High-Performance Delayed Deployment Analysis
#' @description
#' Optimised implementation that pre-filters data once and uses streamlined
#' algorithms to achieve maximum performance for delay analysis.
#'
#' @param parameter_df Single-row data frame with model parameters
#' @param emissions_df Data frame with emissions scenario data
#' @param economic_df Data frame with economic scenario data
#' @param scenario Scenario name to filter data
#' @param target_emissions Target cumulative emissions (default: uses co2_target_2100)
#' @param max_delay_years Maximum delay to test (default: 40)
#' @param delay_step_size Step size for delay grid (default: 5)
#' @param use_parallel Use parallel processing for large analyses (default: TRUE)
#' @param parallel_threshold Minimum combinations to trigger parallel processing (default: 100)
#' @param n_cores Number of cores for parallel processing (default: auto)
#' @param verbose Print progress information (default: TRUE)
#'
#' @return List containing results grid, summary statistics, and run information
#'
#' @examples
#' # Standard delay analysis
#' results <- run_optimised_delayed_deployment(
#'   parameter_df = my_params,
#'   emissions_df = emissions_data,
#'   economic_df = economic_data,
#'   scenario = "SSP3-Baseline"
#' )
#'
#' # High-resolution analysis (yearly steps to 70 years)
#' results <- run_optimised_delayed_deployment(
#'   parameter_df = my_params,
#'   emissions_df = emissions_data,
#'   economic_df = economic_data,
#'   scenario = "SSP3-Baseline",
#'   max_delay_years = 70,
#'   delay_step_size = 1
#' )
run_optimised_delayed_deployment <- function(parameter_df,
                                             emissions_df,
                                             economic_df,
                                             scenario,
                                             target_emissions = NULL,
                                             max_delay_years = 40,
                                             delay_step_size = 5,
                                             use_parallel = TRUE,
                                             parallel_threshold = 100,
                                             n_cores = NULL,
                                             verbose = TRUE) {
  
  # Input validation
  if (!is.data.frame(parameter_df) || nrow(parameter_df) != 1) {
    stop("parameter_df must be a single-row data frame")
  }
  
  if (is.null(target_emissions)) {
    target_emissions <- parameter_df$co2_target_2100
  }
  
  if (verbose) {
    cat("=== OPTIMISED DELAYED DEPLOYMENT ANALYSIS ===\n")
    cat("Scenario:", scenario, "\n")
    cat("Target emissions:", target_emissions, "GtCO2\n")
    cat("Max delay:", max_delay_years, "years, Step:", delay_step_size, "years\n")
  }
  
  start_time <- Sys.time()
  
  # === CRITICAL OPTIMISATION: FILTER DATA ONCE ===
  emissions_scenario <- emissions_df %>%
    filter(Scenario == scenario) %>%
    arrange(Year)
  
  economic_scenario <- economic_df %>%
    filter(Scenario == scenario) %>%
    arrange(Year)
  
  if (nrow(emissions_scenario) == 0 || nrow(economic_scenario) == 0) {
    stop("No data found for scenario: ", scenario)
  }
  
  # Pre-extract vectors for maximum efficiency
  years <- emissions_scenario$Year
  baseline_emissions <- emissions_scenario$Value
  baseline_gwp <- economic_scenario$Value
  
  # Pre-extract parameters to avoid repeated data frame lookups
  parameters <- list(
    cost_mitig_unit = parameter_df$cost_mitig_unit,
    cost_remov_unit = parameter_df$cost_remov_unit,
    exp_mitig = parameter_df$exp_mitig,
    exp_remov = parameter_df$exp_remov,
    exp_temp_anom = parameter_df$exp_temp_anom,
    clim_temp_init = parameter_df$clim_temp_init,
    tcre = parameter_df$tcre,
    econ_dam_pct = parameter_df$econ_dam_pct,
    disc_rate = parameter_df$disc_rate
  )
  
  # Create delay sequences and combinations
  delay_sequence <- seq(0, max_delay_years, by = delay_step_size)
  n_combinations <- length(delay_sequence)^2
  
  if (verbose) {
    cat("Total combinations:", n_combinations, "\n")
    cat("Delay values:", paste(delay_sequence, collapse = ", "), "\n")
  }
  
  # Determine whether to use parallel processing
  use_parallel_actual <- use_parallel && n_combinations >= parallel_threshold
  
  if (use_parallel_actual) {
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }
    if (verbose) cat("Using parallel processing with", n_cores, "cores\n")
  } else {
    if (verbose) cat("Using serial processing\n")
  }
  
  cat("\nStarting analysis...\n")
  
  # Define the single combination function (closure captures pre-filtered data)
  run_combination <- function(mitig_delay, cdr_delay) {
    tryCatch({
      result <- fast_shooting_method(
        parameters = parameters,
        baseline_emissions = baseline_emissions,
        baseline_gwp = baseline_gwp,
        years = years,
        target_emissions = target_emissions,
        mitigation_delay_years = mitig_delay,
        cdr_delay_years = cdr_delay
      )
      
      data.frame(
        mitigation_delay = mitig_delay,
        cdr_delay = cdr_delay,
        peak_temperature = result$peak_temperature,
        final_emissions = result$final_emissions,
        emission_gap = result$emission_gap,
        total_cost = result$total_cost,
        mitig_cost = result$mitig_cost,
        remov_cost = result$remov_cost,
        temp_cost = result$temp_cost,
        converged = result$converged,
        feasible = abs(result$emission_gap) <= 50,
        years_above_1p5 = result$years_above_1p5,
        total_cdr_units = result$total_cdr_units,
        stringsAsFactors = FALSE
      )
      
    }, error = function(e) {
      data.frame(
        mitigation_delay = mitig_delay,
        cdr_delay = cdr_delay,
        peak_temperature = NA,
        final_emissions = NA,
        emission_gap = NA,
        total_cost = NA,
        mitig_cost = NA,
        remov_cost = NA,
        temp_cost = NA,
        converged = FALSE,
        feasible = FALSE,
        years_above_1p5 = NA,
        total_cdr_units = NA,
        stringsAsFactors = FALSE
      )
    })
  }
  
  # Execute analysis
  if (use_parallel_actual) {
    # Parallel execution
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    # Export required objects and functions
    clusterExport(cl, c("fast_shooting_method", "fast_forward_backward_sweep", 
                        "parameters", "baseline_emissions", "baseline_gwp", 
                        "years", "target_emissions"), 
                  envir = environment())
    
    results_df <- foreach(mitig_delay = rep(delay_sequence, each = length(delay_sequence)),
                          cdr_delay = rep(delay_sequence, times = length(delay_sequence)),
                          .combine = rbind,
                          .packages = "dplyr") %dopar% {
                            run_combination(mitig_delay, cdr_delay)
                          }
    
    stopCluster(cl)
    
  } else {
    # Serial execution with progress reporting
    results_list <- list()
    combination_count <- 0
    
    for (mitig_delay in delay_sequence) {
      for (cdr_delay in delay_sequence) {
        combination_count <- combination_count + 1
        
        # Progress reporting
        if (verbose && (combination_count %% 50 == 1 || combination_count <= 5)) {
          elapsed <- difftime(Sys.time(), start_time, units = "mins")
          cat("Combination", combination_count, "of", n_combinations,
              sprintf("(%.1f%%) - Elapsed: %.1f min\n", 
                      100 * combination_count / n_combinations, elapsed))
        }
        
        results_list[[combination_count]] <- run_combination(mitig_delay, cdr_delay)
      }
    }
    
    results_df <- do.call(rbind, results_list)
  }
  
  # Calculate summary statistics
  total_time <- difftime(Sys.time(), start_time, units = "mins")
  n_converged <- sum(results_df$converged, na.rm = TRUE)
  n_feasible <- sum(results_df$feasible, na.rm = TRUE)
  
  feasible_results <- results_df[results_df$feasible & !is.na(results_df$peak_temperature), ]
  
  if (verbose) {
    cat("\n=== ANALYSIS COMPLETE ===\n")
    cat("Total time:", sprintf("%.2f", total_time), "minutes\n")
    cat("Combinations:", n_combinations, "\n")
    cat("Converged:", n_converged, sprintf("(%.1f%%)", 100 * n_converged / n_combinations), "\n")
    cat("Feasible:", n_feasible, sprintf("(%.1f%%)", 100 * n_feasible / n_combinations), "\n")
    
    if (nrow(feasible_results) > 0) {
      cat("Peak temperature range:", sprintf("%.2f - %.2f°C",
                                             min(feasible_results$peak_temperature),
                                             max(feasible_results$peak_temperature)), "\n")
      cat("Performance:", sprintf("%.1f combinations/minute", n_combinations / as.numeric(total_time)), "\n")
    }
  }
  
  # Return comprehensive results
  return(list(
    results = results_df,
    summary_stats = list(
      n_combinations = n_combinations,
      n_converged = n_converged,
      n_feasible = n_feasible,
      convergence_rate = n_converged / n_combinations,
      feasibility_rate = n_feasible / n_combinations,
      combinations_per_minute = n_combinations / as.numeric(total_time)
    ),
    feasible_results = feasible_results,
    run_info = list(
      scenario = scenario,
      target_emissions = target_emissions,
      max_delay_years = max_delay_years,
      delay_step_size = delay_step_size,
      use_parallel = use_parallel_actual,
      n_cores = if (use_parallel_actual) n_cores else 1,
      start_time = start_time,
      end_time = Sys.time(),
      total_time_minutes = as.numeric(total_time)
    )
  ))
}

#' @title Create Optimised Peak Temperature Heatmap
#' @description
#' Fast visualisation function for delay analysis results with sensible defaults.
#'
#' @param delay_results Results list from run_optimised_delayed_deployment
#' @param title_suffix Additional text for plot title (default: "")
#' @param save_plot Whether to save plot to file (default: FALSE)
#' @param filename Optional filename for saved plot
#'
#' @return ggplot object
create_delay_heatmap <- function(delay_results, 
                                 title_suffix = "",
                                 save_plot = FALSE,
                                 filename = NULL) {
  
  results_df <- delay_results$results
  
  # Filter for valid results
  plot_data <- results_df %>%
    filter(!is.na(peak_temperature)) %>%
    mutate(
      peak_temp_capped = pmin(peak_temperature, 8.0),
      feasible_factor = ifelse(feasible, "Feasible", "Infeasible")
    )
  
  if (nrow(plot_data) == 0) {
    warning("No valid data for plotting")
    return(NULL)
  }
  
  # Create heatmap
  p <- ggplot(plot_data, aes(x = mitigation_delay, y = cdr_delay, fill = peak_temp_capped)) +
    geom_tile() +
    scale_fill_viridis_c(
      name = "Peak\nTemperature\n(°C)",
      option = "plasma",
      direction = -1
    ) +
    labs(
      title = paste("Peak Temperature by Deployment Delay", title_suffix),
      subtitle = paste("Target:", delay_results$run_info$target_emissions, "GtCO2 |",
                       sum(plot_data$feasible, na.rm = TRUE), "feasible combinations"),
      x = "Mitigation Deployment Delay (years)",
      y = "CDR Deployment Delay (years)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 10),
      panel.grid = element_blank()
    ) +
    coord_equal()
  
  # Add infeasible markers
  infeasible_data <- plot_data %>% filter(!feasible)
  if (nrow(infeasible_data) > 0) {
    p <- p + 
      geom_point(data = infeasible_data,
                 aes(x = mitigation_delay, y = cdr_delay),
                 shape = 4, size = 1, color = "red", alpha = 0.8, inherit.aes = FALSE) +
      labs(caption = "Red X marks indicate infeasible combinations")
  }
  
  # Save if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0("delay_heatmap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    }
    ggsave(filename, p, width = 10, height = 8, dpi = 300)
    cat("Heatmap saved as:", filename, "\n")
  }
  
  return(p)
}

# Example usage:
# results <- run_optimised_delayed_deployment(
#   parameter_df = my_params,
#   emissions_df = emissions_data,
#   economic_df = economic_data,
#   scenario = "SSP3-Baseline",
#   max_delay_years = 70,
#   delay_step_size = 1
# )
# 
# heatmap <- create_delay_heatmap(results)
# print(heatmap)