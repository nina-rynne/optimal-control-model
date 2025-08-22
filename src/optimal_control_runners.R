#' @title Optimal Control Runner Functions for Climate Temperature Overshoot
#' @description
#' This module provides high-level wrapper functions that apply the core optimal
#' control algorithm to different use cases and analysis patterns. These runners
#' handle data preparation, parameter management, and result aggregation while
#' calling the core algorithm from optimal_control_core.R.
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
#' @contact nina.rynne@griffithuni.edu.au
#' @repository https://github.com/nina-rynne/optimal-control-model
#'
#' @dependencies
#' Required packages: dplyr, parallel, foreach, doParallel
#' Required modules: optimal_control_core.R
#'
#' This module provides the following key functions:
#' - run_single_optimization: Single parameter set optimization
#' - run_sensitivity_analysis: Multiple parameter sets with LHS
#' - run_delayed_deployment_analysis: Policy analysis with deployment delays
#' - run_scenario_comparison: Compare across multiple SSP scenarios

# Load required libraries
if (!requireNamespace("parallel", quietly = TRUE)) {
  stop("Package 'parallel' is required but not installed")
}

if (!requireNamespace("foreach", quietly = TRUE)) {
  stop("Package 'foreach' is required but not installed")
}

if (!requireNamespace("doParallel", quietly = TRUE)) {
  stop("Package 'doParallel' is required but not installed")
}

#' @title Run Single Parameter Set Optimization
#' @description
#' Runs the optimal control algorithm for a single parameter set and scenario.
#' This replaces the functionality from step6_strict_inequality_V2.R for 
#' demonstration runs and single-case analysis.
#'
#' @param parameter_df Single-row data frame containing all model parameters
#' @param emissions_df Data frame with emissions scenario data  
#' @param economic_df Data frame with economic scenario data
#' @param scenario Scenario name to analyze (e.g., "SSP3-Baseline")
#' @param target_emissions Target cumulative emissions (default: uses co2_target_2100 from parameter_df)
#' @param mitigation_delay_years Years to delay mitigation deployment (default: 0)
#' @param cdr_delay_years Years to delay CDR deployment (default: 0)
#' @param use_shooting Whether to use shooting method (TRUE) or provide terminal_adjoint (FALSE)
#' @param terminal_adjoint Fixed terminal adjoint value (used if use_shooting=FALSE)
#' @param verbose Print progress information (default: TRUE)
#'
#' @return List containing:
#'   - solution: Complete solution from optimal_control_solve/shooting
#'   - success: Boolean indicating if optimization succeeded
#'   - scenario: Scenario name used
#'   - parameters: Parameter set used
#'   - run_info: Metadata about the run
#'
#' @examples
#' # Single run with shooting method (typical use)
#' result <- run_single_optimization(
#'   parameter_df = my_params,
#'   emissions_df = emissions_data,
#'   economic_df = economic_data,
#'   scenario = "SSP3-Baseline"
#' )
#'
#' # Single run with fixed terminal condition (for testing)
#' result <- run_single_optimization(
#'   parameter_df = my_params,
#'   emissions_df = emissions_data,
#'   economic_df = economic_data,
#'   scenario = "SSP3-Baseline",
#'   use_shooting = FALSE,
#'   terminal_adjoint = 1000
#' )
run_single_optimization <- function(parameter_df,
                                    emissions_df,
                                    economic_df,
                                    scenario,
                                    target_emissions = NULL,
                                    mitigation_delay_years = 0,
                                    cdr_delay_years = 0,
                                    use_shooting = TRUE,
                                    terminal_adjoint = 1000,
                                    verbose = TRUE) {
  
  # Input validation
  if (!is.data.frame(parameter_df) || nrow(parameter_df) != 1) {
    stop("parameter_df must be a single-row data frame")
  }
  
  if (verbose) {
    cat("=== SINGLE OPTIMIZATION RUN ===\n")
    cat("Scenario:", scenario, "\n")
    cat("Mitigation delay:", mitigation_delay_years, "years\n")
    cat("CDR delay:", cdr_delay_years, "years\n")
    cat("Using shooting method:", use_shooting, "\n\n")
  }
  
  # Record start time
  start_time <- Sys.time()
  
  # Run optimization
  tryCatch({
    if (use_shooting) {
      # Use shooting method to find optimal terminal condition
      solution <- optimal_control_shooting(
        parameter_df = parameter_df,
        emissions_df = emissions_df,
        economic_df = economic_df,
        scenario = scenario,
        target_emissions = target_emissions,
        mitigation_delay_years = mitigation_delay_years,
        cdr_delay_years = cdr_delay_years,
        verbose = verbose
      )
    } else {
      # Use fixed terminal condition
      # Filter data by scenario first
      emissions_scenario <- emissions_df %>%
        dplyr::filter(Scenario == scenario) %>%
        dplyr::arrange(Year)
      
      economic_scenario <- economic_df %>%
        dplyr::filter(Scenario == scenario) %>%
        dplyr::arrange(Year)
      
      if (is.null(target_emissions)) {
        target_emissions <- parameter_df$co2_target_2100
      }
      
      solution <- optimal_control_solve(
        parameter_df = parameter_df,
        emissions_df = emissions_scenario,
        economic_df = economic_scenario,
        target_emissions = target_emissions,
        terminal_adjoint = terminal_adjoint,
        mitigation_delay_years = mitigation_delay_years,
        cdr_delay_years = cdr_delay_years,
        verbose = verbose
      )
    }
    
    # Calculate run time
    run_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    
    if (verbose) {
      cat("\n=== RUN COMPLETE ===\n")
      cat("Success:", solution$converged, "\n")
      cat("Runtime:", sprintf("%.2f", run_time), "minutes\n")
      if (solution$converged) {
        cat("Final emissions:", round(solution$final_emissions, 1), "GtCO2\n")
        cat("Final temperature:", round(solution$final_temperature, 2), "°C\n")
        cat("Total cost:", sprintf("%.1f", solution$total_cost), "trillion $\n")
      }
    }
    
    # Return structured result
    return(list(
      solution = solution,
      success = solution$converged,
      scenario = scenario,
      parameters = parameter_df,
      run_info = list(
        start_time = start_time,
        end_time = Sys.time(),
        run_time_minutes = run_time,
        use_shooting = use_shooting,
        mitigation_delay_years = mitigation_delay_years,
        cdr_delay_years = cdr_delay_years
      )
    ))
    
  }, error = function(e) {
    if (verbose) {
      cat("ERROR:", e$message, "\n")
    }
    
    return(list(
      solution = NULL,
      success = FALSE,
      scenario = scenario,
      parameters = parameter_df,
      error = e$message,
      run_info = list(
        start_time = start_time,
        end_time = Sys.time(),
        run_time_minutes = as.numeric(difftime(Sys.time(), start_time, units = "mins")),
        use_shooting = use_shooting,
        mitigation_delay_years = mitigation_delay_years,
        cdr_delay_years = cdr_delay_years
      )
    ))
  })
}

#' @title Run Sensitivity Analysis with Multiple Parameter Sets
#' @description
#' Runs the optimal control algorithm for multiple parameter sets (typically from
#' Latin Hypercube Sampling) to explore the solution space. This replaces the
#' functionality from step6_multiple_sweeps.R.
#'
#' @param parameter_df Data frame with multiple parameter sets (one per row)
#' @param emissions_df Data frame with emissions scenario data
#' @param economic_df Data frame with economic scenario data  
#' @param scenario Scenario name to analyze (e.g., "SSP3-Baseline")
#' @param use_parallel Whether to use parallel processing (default: TRUE)
#' @param n_cores Number of cores for parallel processing (default: auto-detect)
#' @param save_intermediate Save results periodically during run (default: TRUE)
#' @param output_prefix Prefix for intermediate save files (default: "sensitivity")
#' @param verbose Print progress information (default: TRUE)
#'
#' @return List containing:
#'   - successful_runs: List of successful optimization results
#'   - failed_runs: List of failed runs with error information
#'   - summary: Data frame with key metrics from successful runs
#'   - run_info: Metadata about the sensitivity analysis
#'
#' @examples
#' # Standard sensitivity analysis with LHS parameter sets
#' results <- run_sensitivity_analysis(
#'   parameter_df = lhs_parameters,
#'   emissions_df = emissions_data,
#'   economic_df = economic_data,
#'   scenario = "SSP3-Baseline"
#' )
#'
#' # Serial processing for debugging
#' results <- run_sensitivity_analysis(
#'   parameter_df = lhs_parameters[1:10, ],
#'   emissions_df = emissions_data,
#'   economic_df = economic_data,
#'   scenario = "SSP3-Baseline",
#'   use_parallel = FALSE
#' )
run_sensitivity_analysis <- function(parameter_df,
                                     emissions_df,
                                     economic_df,
                                     scenario,
                                     use_parallel = TRUE,
                                     n_cores = NULL,
                                     save_intermediate = TRUE,
                                     output_prefix = "sensitivity",
                                     verbose = TRUE) {
  
  # Input validation
  if (!is.data.frame(parameter_df) || nrow(parameter_df) == 0) {
    stop("parameter_df must be a data frame with at least one row")
  }
  
  n_samples <- nrow(parameter_df)
  
  if (verbose) {
    cat("=== SENSITIVITY ANALYSIS ===\n")
    cat("Number of parameter samples:", n_samples, "\n")
    cat("Scenario:", scenario, "\n")
    cat("Using parallel processing:", use_parallel, "\n")
    if (use_parallel) {
      if (is.null(n_cores)) {
        n_cores <- max(1, parallel::detectCores() - 1)
      }
      cat("Number of cores:", n_cores, "\n")
    }
    cat("\n")
  }
  
  # Record start time
  start_time <- Sys.time()
  
  # Initialize storage
  successful_runs <- list()
  failed_runs <- list()
  
  # Define single run function for parallel/serial execution
  run_single_param_set <- function(i) {
    param_row <- parameter_df[i, ]
    
    tryCatch({
      result <- optimal_control_shooting(
        parameter_df = param_row,
        emissions_df = emissions_df,
        economic_df = economic_df,
        scenario = scenario,
        verbose = FALSE
      )
      
      return(list(
        success = TRUE,
        param_id = i,
        result = result,
        parameters = param_row
      ))
      
    }, error = function(e) {
      return(list(
        success = FALSE,
        param_id = i,
        error = e$message,
        parameters = param_row
      ))
    })
  }
  
  # Execute runs (parallel or serial)
  if (use_parallel && n_samples > 1) {
    # Parallel execution
    if (verbose) cat("Setting up parallel processing...\n")
    
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    
    # Export necessary functions and variables to workers
    parallel::clusterEvalQ(cl, {
      library(dplyr)
    })
    
    parallel::clusterExport(cl, c("optimal_control_shooting", "optimal_control_solve",
                                  "emissions_df", "economic_df", "scenario"),
                            envir = environment())
    
    if (verbose) cat("Running", n_samples, "optimizations in parallel...\n")
    
    # Run parallel optimization
    all_results <- foreach::foreach(i = 1:n_samples, .packages = "dplyr") %dopar% {
      run_single_param_set(i)
    }
    
    # Clean up parallel processing
    parallel::stopCluster(cl)
    
  } else {
    # Serial execution
    if (verbose) cat("Running", n_samples, "optimizations in serial...\n")
    
    all_results <- list()
    for (i in 1:n_samples) {
      if (verbose && (i %% 10 == 0 || i <= 5)) {
        elapsed <- difftime(Sys.time(), start_time, units = "mins")
        cat("Sample", i, "of", n_samples, "- Elapsed:", round(elapsed, 1), "minutes\n")
      }
      
      all_results[[i]] <- run_single_param_set(i)
      
      # Intermediate save if requested
      if (save_intermediate && i %% 50 == 0) {
        temp_file <- paste0(output_prefix, "_temp_", i, ".rds")
        saveRDS(all_results[1:i], temp_file)
        if (verbose) cat("Intermediate save:", temp_file, "\n")
      }
    }
  }
  
  # Process results
  if (verbose) cat("Processing results...\n")
  
  for (i in 1:length(all_results)) {
    result <- all_results[[i]]
    
    if (result$success) {
      run_name <- paste0("run_", result$param_id)
      successful_runs[[run_name]] <- result$result
    } else {
      failed_name <- paste0("failed_", result$param_id)
      failed_runs[[failed_name]] <- result
    }
  }
  
  # Create summary data frame
  summary_df <- NULL
  if (length(successful_runs) > 0) {
    summary_data <- list()
    
    for (i in 1:length(successful_runs)) {
      run <- successful_runs[[i]]
      summary_data[[i]] <- data.frame(
        run_id = names(successful_runs)[i],
        final_emissions = run$final_emissions,
        final_temperature = run$final_temperature,
        peak_temperature = max(run$temperature_anomaly),
        total_cost = run$total_cost,
        mitig_cost = run$mitig_cost,
        remov_cost = run$remov_cost,
        temp_cost = run$temp_cost,
        total_mitigation_units = sum(run$qty_mitig),
        total_cdr_units = sum(run$qty_remov),
        years_above_1p5 = sum(run$temperature_anomaly > 1.5),
        converged = run$converged,
        iterations = run$iterations,
        stringsAsFactors = FALSE
      )
    }
    
    summary_df <- do.call(rbind, summary_data)
  }
  
  # Calculate runtime
  total_time <- difftime(Sys.time(), start_time, units = "mins")
  
  if (verbose) {
    cat("\n=== SENSITIVITY ANALYSIS COMPLETE ===\n")
    cat("Total time:", round(total_time, 1), "minutes\n")
    cat("Successful runs:", length(successful_runs), "out of", n_samples, 
        "(", round(100 * length(successful_runs) / n_samples, 1), "%)\n")
    cat("Failed runs:", length(failed_runs), "\n")
    
    if (!is.null(summary_df)) {
      cat("\nSummary statistics:\n")
      cat("Peak temperature range:", sprintf("%.2f - %.2f°C", 
                                             min(summary_df$peak_temperature),
                                             max(summary_df$peak_temperature)), "\n")
      cat("Total cost range:", sprintf("%.1f - %.1f trillion $", 
                                       min(summary_df$total_cost),
                                       max(summary_df$total_cost)), "\n")
    }
  }
  
  # Return comprehensive results
  return(list(
    successful_runs = successful_runs,
    failed_runs = failed_runs,
    summary = summary_df,
    run_info = list(
      n_samples = n_samples,
      n_successful = length(successful_runs),
      n_failed = length(failed_runs),
      scenario = scenario,
      use_parallel = use_parallel,
      n_cores = if (use_parallel) n_cores else 1,
      start_time = start_time,
      end_time = Sys.time(),
      total_time_minutes = as.numeric(total_time)
    )
  ))
}

#' @title Run Delayed Deployment Analysis  
#' @description
#' Runs a comprehensive analysis of different deployment delay combinations to
#' understand policy implications. This replaces the functionality from 
#' step6_delayed_deployment.R with the heatmap analysis capability.
#'
#' @param parameter_df Single-row data frame containing model parameters
#' @param emissions_df Data frame with emissions scenario data
#' @param economic_df Data frame with economic scenario data
#' @param scenario Scenario name to analyze (e.g., "SSP3-Baseline")
#' @param max_delay_years Maximum delay to test for both mitigation and CDR (default: 40)
#' @param delay_step_size Step size for delay grid (default: 5 years)
#' @param target_emissions Target cumulative emissions (default: uses co2_target_2100 from parameter_df)
#' @param use_parallel Whether to use parallel processing (default: TRUE)
#' @param n_cores Number of cores for parallel processing (default: auto-detect)
#' @param verbose Print progress information (default: TRUE)
#'
#' @return List containing:
#'   - results_grid: Data frame with results for each delay combination
#'   - summary_stats: Summary statistics across all combinations
#'   - feasible_combinations: Subset of results that met the emissions target
#'   - run_info: Metadata about the analysis
#'
#' @examples
#' # Standard delayed deployment analysis
#' delay_results <- run_delayed_deployment_analysis(
#'   parameter_df = my_params,
#'   emissions_df = emissions_data,
#'   economic_df = economic_data,
#'   scenario = "SSP3-Baseline"
#' )
#'
#' # High resolution analysis
#' delay_results <- run_delayed_deployment_analysis(
#'   parameter_df = my_params,
#'   emissions_df = emissions_data,
#'   economic_df = economic_data,
#'   scenario = "SSP3-Baseline",
#'   max_delay_years = 60,
#'   delay_step_size = 2
#' )
run_delayed_deployment_analysis <- function(parameter_df,
                                            emissions_df,
                                            economic_df,
                                            scenario,
                                            max_delay_years = 40,
                                            delay_step_size = 5,
                                            target_emissions = NULL,
                                            use_parallel = TRUE,
                                            n_cores = NULL,
                                            verbose = TRUE) {
  
  # Input validation
  if (!is.data.frame(parameter_df) || nrow(parameter_df) != 1) {
    stop("parameter_df must be a single-row data frame")
  }
  
  # Use target from parameter_df if not specified
  if (is.null(target_emissions)) {
    target_emissions <- parameter_df$co2_target_2100
  }
  
  # Create delay sequences
  delay_sequence <- seq(0, max_delay_years, by = delay_step_size)
  n_combinations <- length(delay_sequence)^2
  
  if (verbose) {
    cat("=== DELAYED DEPLOYMENT ANALYSIS ===\n")
    cat("Scenario:", scenario, "\n")
    cat("Target emissions:", target_emissions, "GtCO2\n")
    cat("Maximum delay:", max_delay_years, "years\n")
    cat("Step size:", delay_step_size, "years\n")
    cat("Total combinations:", n_combinations, "\n")
    cat("Delay values:", delay_sequence, "\n")
    cat("Using parallel processing:", use_parallel, "\n\n")
  }
  
  # Set up parallel processing if requested
  if (use_parallel) {
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }
    if (verbose) cat("Setting up parallel processing with", n_cores, "cores...\n")
    
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    
    # Export necessary functions and variables
    parallel::clusterEvalQ(cl, {
      library(dplyr)
    })
    
    parallel::clusterExport(cl, c("optimal_control_shooting", "optimal_control_solve",
                                  "parameter_df", "emissions_df", "economic_df", 
                                  "scenario", "target_emissions"),
                            envir = environment())
  }
  
  # Record start time
  start_time <- Sys.time()
  
  # Create all combinations
  combinations <- expand.grid(
    mitigation_delay = delay_sequence,
    cdr_delay = delay_sequence
  )
  
  # Define function for single combination
  run_single_combination <- function(i) {
    mitig_delay <- combinations$mitigation_delay[i]
    cdr_delay <- combinations$cdr_delay[i]
    
    tryCatch({
      result <- optimal_control_shooting(
        parameter_df = parameter_df,
        emissions_df = emissions_df,
        economic_df = economic_df,
        scenario = scenario,
        target_emissions = target_emissions,
        mitigation_delay_years = mitig_delay,
        cdr_delay_years = cdr_delay,
        max_shooting_iterations = 50,  # Reduced for efficiency
        shooting_tolerance = 3.0,      # Slightly relaxed
        verbose = FALSE
      )
      
      return(data.frame(
        combination_id = i,
        mitigation_delay = mitig_delay,
        cdr_delay = cdr_delay,
        peak_temperature = max(result$temperature_anomaly),
        final_emissions = result$final_emissions,
        emission_gap = result$emission_gap,
        total_cost = result$total_cost,
        mitig_cost = result$mitig_cost,
        remov_cost = result$remov_cost,
        temp_cost = result$temp_cost,
        converged = result$converged,
        feasible = abs(result$emission_gap) <= 50  # Define feasibility threshold
      ))
      
    }, error = function(e) {
      return(data.frame(
        combination_id = i,
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
        feasible = FALSE
      ))
    })
  }
  
  # Execute analysis
  if (use_parallel && n_combinations > 1) {
    if (verbose) cat("Running", n_combinations, "combinations in parallel...\n")
    
    results_list <- foreach::foreach(i = 1:n_combinations, .packages = "dplyr",
                                     .combine = rbind) %dopar% {
                                       run_single_combination(i)
                                     }
    
    parallel::stopCluster(cl)
    
  } else {
    if (verbose) cat("Running", n_combinations, "combinations in serial...\n")
    
    results_list <- list()
    for (i in 1:n_combinations) {
      if (verbose && (i %% 20 == 1 || i <= 5)) {
        elapsed <- difftime(Sys.time(), start_time, units = "mins")
        cat("Combination", i, "of", n_combinations, 
            sprintf("(%.1f%%) - Elapsed: %.1f min\n", 
                    100*i/n_combinations, elapsed))
      }
      
      results_list[[i]] <- run_single_combination(i)
    }
    
    results_list <- do.call(rbind, results_list)
  }
  
  # Process results
  total_time <- difftime(Sys.time(), start_time, units = "mins")
  
  # Calculate summary statistics
  n_converged <- sum(results_list$converged, na.rm = TRUE)
  n_feasible <- sum(results_list$feasible, na.rm = TRUE)
  
  feasible_results <- results_list[results_list$feasible & !is.na(results_list$peak_temperature), ]
  
  summary_stats <- list(
    n_combinations = n_combinations,
    n_converged = n_converged,
    n_feasible = n_feasible,
    convergence_rate = n_converged / n_combinations,
    feasibility_rate = n_feasible / n_combinations
  )
  
  if (nrow(feasible_results) > 0) {
    summary_stats$peak_temp_range <- c(
      min = min(feasible_results$peak_temperature, na.rm = TRUE),
      max = max(feasible_results$peak_temperature, na.rm = TRUE)
    )
    summary_stats$cost_range <- c(
      min = min(feasible_results$total_cost, na.rm = TRUE),
      max = max(feasible_results$total_cost, na.rm = TRUE)
    )
  }
  
  if (verbose) {
    cat("\n=== DELAYED DEPLOYMENT ANALYSIS COMPLETE ===\n")
    cat("Total time:", sprintf("%.1f", total_time), "minutes\n")
    cat("Total combinations:", n_combinations, "\n")
    cat("Converged solutions:", n_converged, sprintf("(%.1f%%)", 100*n_converged/n_combinations), "\n")
    cat("Feasible solutions:", n_feasible, sprintf("(%.1f%%)", 100*n_feasible/n_combinations), "\n")
    
    if (nrow(feasible_results) > 0) {
      cat("Peak temperature range:", sprintf("%.2f - %.2f°C", 
                                             summary_stats$peak_temp_range["min"],
                                             summary_stats$peak_temp_range["max"]), "\n")
    }
  }
  
  return(list(
    results_grid = results_list,
    summary_stats = summary_stats,
    feasible_combinations = feasible_results,
    run_info = list(
      scenario = scenario,
      target_emissions = target_emissions,
      max_delay_years = max_delay_years,
      delay_step_size = delay_step_size,
      n_combinations = n_combinations,
      use_parallel = use_parallel,
      n_cores = if (use_parallel) n_cores else 1,
      start_time = start_time,
      end_time = Sys.time(),
      total_time_minutes = as.numeric(total_time)
    )
  ))
}

#' @title Run Scenario Comparison Analysis
#' @description
#' Runs the optimal control algorithm across multiple SSP scenarios to compare
#' the effects of different baseline emission pathways. This provides the
#' multi-scenario comparison functionality needed for publication figures.
#'
#' @param parameter_df Single-row data frame containing model parameters, or
#'                     multiple rows for sensitivity analysis across scenarios
#' @param emissions_df Data frame with emissions data for multiple scenarios
#' @param economic_df Data frame with economic data for multiple scenarios  
#' @param scenarios Vector of scenario names to compare (e.g., c("SSP2-Baseline", "SSP3-Baseline"))
#' @param use_parallel Whether to use parallel processing across scenarios (default: TRUE)
#' @param verbose Print progress information (default: TRUE)
#'
#' @return List containing:
#'   - scenario_results: Named list of results for each scenario
#'   - comparison_summary: Data frame comparing key metrics across scenarios
#'   - year_first_1p5C: Vector of years when temperature first reaches 1.5°C for each scenario
#'   - year_peak_temp: Vector of years when peak temperature occurs for each scenario
#'   - year_mitig_capped: Vector of years when mitigation is first capped by emissions limit for each scenario
#'   - run_info: Metadata about the comparison analysis
#'
#' @examples
#' # Compare across 5 SSP scenarios with single parameter set
#' comparison <- run_scenario_comparison(
#'   parameter_df = my_params,
#'   emissions_df = all_emissions_data,
#'   economic_df = all_economic_data,
#'   scenarios = c("SSP1-Baseline", "SSP2-Baseline", "SSP3-Baseline", 
#'                 "SSP4-Baseline", "SSP5-Baseline")
#' )
run_scenario_comparison <- function(parameter_df,
                                    emissions_df,
                                    economic_df,
                                    scenarios,
                                    use_parallel = TRUE,
                                    verbose = TRUE) {
  
  # Input validation
  if (!is.data.frame(parameter_df) || nrow(parameter_df) == 0) {
    stop("parameter_df must be a data frame with at least one row")
  }
  
  if (length(scenarios) == 0) {
    stop("At least one scenario must be specified")
  }
  
  # Check if scenarios exist in data
  available_scenarios <- unique(emissions_df$Scenario)
  missing_scenarios <- setdiff(scenarios, available_scenarios)
  if (length(missing_scenarios) > 0) {
    stop("Scenarios not found in data: ", paste(missing_scenarios, collapse = ", "))
  }
  
  if (verbose) {
    cat("=== SCENARIO COMPARISON ANALYSIS ===\n")
    cat("Number of scenarios:", length(scenarios), "\n")
    cat("Scenarios:", paste(scenarios, collapse = ", "), "\n")
    cat("Parameter sets:", nrow(parameter_df), "\n")
    cat("Using parallel processing:", use_parallel, "\n\n")
  }
  
  start_time <- Sys.time()
  
  # For single parameter set, run each scenario
  if (nrow(parameter_df) == 1) {
    start_time <- Sys.time()
    
    # Define single scenario run function
    run_single_scenario <- function(scenario_name) {
      tryCatch({
        result <- optimal_control_shooting(
          parameter_df = parameter_df,
          emissions_df = emissions_df,
          economic_df = economic_df,
          scenario = scenario_name,
          verbose = FALSE
        )
        
        return(list(
          success = TRUE,
          scenario = scenario_name,
          result = result
        ))
        
      }, error = function(e) {
        return(list(
          success = FALSE,
          scenario = scenario_name,
          error = e$message
        ))
      })
    }
    
    # Execute scenario comparisons
    if (use_parallel && length(scenarios) > 1) {
      # Parallel execution across scenarios
      if (verbose) cat("Running", length(scenarios), "scenarios in parallel...\n")
      
      cl <- parallel::makeCluster(max(1, parallel::detectCores() - 1))
      doParallel::registerDoParallel(cl)
      
      # Export necessary functions and variables
      parallel::clusterEvalQ(cl, {
        library(dplyr)
      })
      
      parallel::clusterExport(cl, c("optimal_control_shooting", "optimal_control_solve",
                                    "parameter_df", "emissions_df", "economic_df"),
                              envir = environment())
      
      scenario_results_list <- foreach::foreach(scenario = scenarios, .packages = "dplyr") %dopar% {
        run_single_scenario(scenario)
      }
      
      parallel::stopCluster(cl)
      
    } else {
      # Serial execution
      if (verbose) cat("Running", length(scenarios), "scenarios in serial...\n")
      
      scenario_results_list <- list()
      for (i in 1:length(scenarios)) {
        scenario <- scenarios[i]
        if (verbose) {
          elapsed <- difftime(Sys.time(), start_time, units = "mins")
          cat("Scenario", i, "of", length(scenarios), "(", scenario, ") - Elapsed:", 
              round(elapsed, 1), "minutes\n")
        }
        
        scenario_results_list[[i]] <- run_single_scenario(scenario)
      }
    }
    
    # Process results
    scenario_results <- list()
    failed_scenarios <- list()
    
    for (i in 1:length(scenario_results_list)) {
      result <- scenario_results_list[[i]]
      
      if (result$success) {
        scenario_results[[result$scenario]] <- result$result
      } else {
        failed_scenarios[[result$scenario]] <- result$error
      }
    }
    
    # Create comparison summary and extract additional metrics
    comparison_summary <- NULL
    year_first_1p5C <- numeric(0)
    year_peak_temp <- numeric(0)
    year_mitig_capped <- numeric(0)
    
    if (length(scenario_results) > 0) {
      summary_data <- list()
      
      for (i in 1:length(scenario_results)) {
        scenario_name <- names(scenario_results)[i]
        run <- scenario_results[[i]]
        
        # Extract new metrics
        # 1. Year of first time temperature reaches 1.5°C
        temp_1p5_indices <- which(run$temperature_anomaly >= 1.5)
        year_first_1p5 <- if (length(temp_1p5_indices) > 0) {
          run$years[temp_1p5_indices[1]]
        } else {
          NA
        }
        
        # 2. Year of peak temperature
        peak_temp_index <- which.max(run$temperature_anomaly)
        year_peak <- run$years[peak_temp_index]
        
        # 3. Year when mitigation is capped by emissions limit
        # Use epsilon from the solution (epsilon_used) or default value
        epsilon <- if ("epsilon_used" %in% names(run)) run$epsilon_used else 0.01
        
        # Check where mitigation is very close to the constraint (indicating constraint is active)
        # Use small tolerance to detect when qty_mitig ≈ baseline_emissions - epsilon
        tolerance <- 0.001
        max_allowed_mitig <- run$baseline_annual_emissions - epsilon
        capped_indices <- which(abs(run$qty_mitig - max_allowed_mitig) < tolerance)
        year_mitig_cap <- if (length(capped_indices) > 0) {
          run$years[capped_indices[1]]
        } else {
          NA
        }
        
        # Store for top-level list elements
        year_first_1p5C <- c(year_first_1p5C, year_first_1p5)
        year_peak_temp <- c(year_peak_temp, year_peak)
        year_mitig_capped <- c(year_mitig_capped, year_mitig_cap)
        
        # Create summary data frame row
        summary_data[[i]] <- data.frame(
          scenario = scenario_name,
          final_emissions = run$final_emissions,
          final_temperature = run$final_temperature,
          peak_temperature = max(run$temperature_anomaly),
          total_cost = run$total_cost,
          mitig_cost = run$mitig_cost,
          remov_cost = run$remov_cost,
          temp_cost = run$temp_cost,
          total_mitigation_units = sum(run$qty_mitig),
          total_cdr_units = sum(run$qty_remov),
          years_above_1p5 = sum(run$temperature_anomaly > 1.5),
          year_first_1p5C = year_first_1p5,
          year_peak_temp = year_peak,
          year_mitig_capped = year_mitig_cap,
          converged = run$converged,
          iterations = run$iterations,
          stringsAsFactors = FALSE
        )
      }
      
      comparison_summary <- do.call(rbind, summary_data)
      
      # Name the vectors for top-level list elements
      names(year_first_1p5C) <- names(scenario_results)
      names(year_peak_temp) <- names(scenario_results)
      names(year_mitig_capped) <- names(scenario_results)
    }
    
    # Calculate runtime
    total_time <- difftime(Sys.time(), start_time, units = "mins")
    
    if (verbose) {
      cat("\n=== SCENARIO COMPARISON COMPLETE ===\n")
      cat("Total time:", sprintf("%.1f", total_time), "minutes\n")
      cat("Successful scenarios:", length(scenario_results), "out of", length(scenarios), "\n")
      cat("Failed scenarios:", length(failed_scenarios), "\n")
      
      if (!is.null(comparison_summary)) {
        cat("\nScenario comparison:\n")
        cat("Peak temperature range:", sprintf("%.2f - %.2f°C",
                                               min(comparison_summary$peak_temperature),
                                               max(comparison_summary$peak_temperature)), "\n")
        cat("Total cost range:", sprintf("%.1f - %.1f trillion $",
                                         min(comparison_summary$total_cost),
                                         max(comparison_summary$total_cost)), "\n")
        
        # Report on new metrics
        valid_1p5_years <- year_first_1p5C[!is.na(year_first_1p5C)]
        if (length(valid_1p5_years) > 0) {
          cat("First 1.5°C crossing range:", sprintf("%d - %d",
                                                     min(valid_1p5_years),
                                                     max(valid_1p5_years)), "\n")
        } else {
          cat("No scenarios reach 1.5°C\n")
        }
        
        valid_cap_years <- year_mitig_capped[!is.na(year_mitig_capped)]
        if (length(valid_cap_years) > 0) {
          cat("Mitigation capping range:", sprintf("%d - %d",
                                                   min(valid_cap_years),
                                                   max(valid_cap_years)), "\n")
        } else {
          cat("No scenarios have mitigation capped by emissions limit\n")
        }
      }
    }
    
    return(list(
      scenario_results = scenario_results,
      comparison_summary = comparison_summary,
      year_first_1p5C = year_first_1p5C,
      year_peak_temp = year_peak_temp,
      year_mitig_capped = year_mitig_capped,
      failed_scenarios = failed_scenarios,
      run_info = list(
        scenarios = scenarios,
        n_scenarios = length(scenarios),
        n_successful = length(scenario_results),
        n_failed = length(failed_scenarios),
        parameter_sets = nrow(parameter_df),
        use_parallel = use_parallel,
        start_time = start_time,
        end_time = Sys.time(),
        total_time_minutes = as.numeric(total_time)
      )
    ))
  } # Close the if (nrow(parameter_df) == 1) block
} # Close the run_scenario_comparison function