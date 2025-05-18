#' @title Optimal Control Methods for Climate Temperature Overshoot
#' @description
#' This script contains functions for solving optimal control problems related to
#' climate temperature overshoot. It implements forward-backward sweep and shooting methods
#' to find optimal mitigation and carbon dioxide removal (CDR) strategies.
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
#'
#' @dependencies
#' Required packages: dplyr, tidyr, here
#' 

#' @title Initialize Log File for Optimal Control Run
#' @description
#' Creates a log file with timestamp for recording simulation progress and debugging information.
#' @param prefix Prefix for the log file name (default: "optimal_control")
#' @param log_dir Directory for storing log files (default: "logs")
#' @return Path to the created log file
#' @examples
#' log_file <- init_log_file()
#' write_log(log_file, "Starting simulation with parameters X, Y, Z")
#'
init_log_file <- function(prefix = "optimal_control", log_dir = "logs") {
  # Create logs directory if it doesn't exist
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Create log file with timestamp
  log_filename <- file.path(log_dir, 
                            paste0(prefix, "_log_", 
                                   format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                   ".txt"))
  
  # Initialize the file with a header
  cat(paste0("Optimal Control Simulation Log\n",
             "Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
             "-------------------------------------------\n\n"),
      file = log_filename)
  
  return(log_filename)
}

#' @title Write to Log File
#' @description
#' Appends a message to the specified log file with timestamp.
#' @param log_file Path to the log file
#' @param message Message to write to the log
#' @param include_timestamp Whether to include timestamp (default: FALSE)
#' @return None
#' @examples
#' write_log(log_file, "Starting forward sweep")
#'
write_log <- function(log_file, message, include_timestamp = FALSE) {
  if (include_timestamp) {
    message <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", message)
  }
  
  cat(paste0(message, "\n"), file = log_file, append = TRUE)
}

#' @title Create Vector List for Optimal Control
#' @description 
#' This function
#' creates and initialises all the vectors required for the optimal control solution 
#' function.
#' 
#' @param parameter_df Dataframe containing parameters from LHS (experimental_design.R) 
#' and the fixed model and algorithm parameters (model_parameters.R). 
#' @param emissions_df Data frame containing emissions scenario data by year
#' 

create_vector_list <- function(parameter_df,
                               emissions_df,
                               economic_df,
                               scenario){
  
  # Filter by scenario
  emissions_filtered <- emissions_df %>%
    filter(Scenario == scenario)
  
  economic_filtered <- economic_df %>%
    filter(Scenario == scenario)
  
  # Time setup
  years <- emissions_filtered %>% 
    arrange(Year) %>%           # Ensure years are in order
    pull(Year) %>%              # Extract just the years
    unique()                    # Get unique values
  
  years_rel <- years - min(years)
  n_years <- length(years)
  
  # SSP scenario inputs
  # numeric vector containing the yearly baseline CO2 emissions from IPCC scenarios
  baseline_annual_emissions <- emissions_filtered %>%
    arrange(Year) %>%           # Same ordering as years
    pull(Value)
  # numeric vector containing the yearly GWP from IPCC scenarios
  baseline_annual_gwp <- economic_filtered %>%
    arrange(Year) %>%           # Same ordering as years
    pull(Value)
  
  # Used parameter variables
  clim_temp_init <- parameter_df$clim_temp_init
  tcre <- parameter_df$tcre
  trans_cond <- parameter_df$trans_cond
  
  # State variables
  # numeric vector filled with NA values. updated during forward sweep.
  temperature_anomaly <- rep(NA, n_years)
  # numeric vector to track cumulative CO2 emissions (after mit and CDR). updated during forward sweep.
  cumulative_emissions <- rep(0, n_years)
  
  # Initial state
  cumulative_emissions[1] <- baseline_annual_emissions[1]
  temperature_anomaly[1] <- clim_temp_init + ((cumulative_emissions[1]/1000) * tcre)
  #temperature_anomaly[1] <- (((cumulative_emissions[1] + 2500)/1000) * tcre)
  
  # Control variables with bounds
  # numeric vector of mitigation control at each time step, initialised to 0
  qty_mitig <- rep(0, n_years)
  # numeric vector of CDR control at each time step, initialised to 0
  qty_remov <- rep(0, n_years)
  mitigation_max <- baseline_annual_emissions
  # note: all other min/max values are single parameters, stored in parameter_df
  
  # Adjoint variable
  # numeric vector of adjoint variable, initialised to 0
  adjoint_var <- rep(0, n_years)
  adjoint_var[n_years] <- trans_cond  # Set Terminal condition for final value of lambda
  
  # Cumulative Cost components, initialised to NA
  cost_mitig_cumul <- rep(NA, n_years)
  cost_remov_cumul <- rep(NA, n_years)
  cost_resid_cumul <- rep(NA, n_years)
  cost_total_cumul <- rep(NA, n_years)
  
  return(list(
    
    # Time variables
    years = years,
    years_rel = years_rel,
    n_years = n_years,
    
    # State variables
    baseline_annual_emissions = baseline_annual_emissions,
    temperature_anomaly = temperature_anomaly,
    cumulative_emissions = cumulative_emissions,
    
    # Control variables and bounds
    qty_mitig = qty_mitig,
    qty_remov = qty_remov,
    mitigation_max = mitigation_max,
    
    # Adjoint variable
    adjoint_var = adjoint_var,
    
    # Cost components
    baseline_annual_gwp = baseline_annual_gwp,
    cost_mitig_cumul = cost_mitig_cumul,
    cost_remov_cumul = cost_remov_cumul,
    cost_resid_cumul = cost_resid_cumul,
    cost_total_cumul = cost_total_cumul
  ))
}

#' @title Run Forward-Backward Sweep for Multiple Parameter Sets
#' @description
#' Applies the forward-backward sweep method across multiple parameter combinations.
#' This function iterates through each row of a parameter dataframe, running the
#' optimal control algorithm for each set of parameters and collecting the results.
#' 
#' @param parameter_df Data frame containing multiple sets of model parameters (one per row)
#' @param vector_list List of vectors required for optimal control calculation
#' @param save_intermediate Logical; whether to save intermediate results (default: FALSE)
#' @param verbose Logical; whether to print progress messages (default: FALSE)
#' @return List of results, with each element containing the complete optimal control solution
#'         for a specific parameter set. Each result is tagged with a unique run_id.
#'

run_multiple_sweeps <- function(parameter_df, 
                                emissions_df,
                                economic_df,
                                scenario,
                                save_intermediate = FALSE,
                                verbose = FALSE) {
  
  # Create a timestamp for this batch of runs
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Create log file if verbose
  log_file <- NULL
  if (verbose) {
    log_file <- init_log_file("sweep_batch", "logs")
    write_log(log_file, paste("Starting batch with", nrow(parameter_df), "parameter sets"))
  }
  
  # Use lapply to process each parameter set
  results_list <- lapply(seq_len(nrow(parameter_df)), function(i) {
    
    # Create a run ID for this iteration that includes the timestamp
    run_id <- paste0("run_", timestamp, "_", i)
    
    if (verbose) {
      write_log(log_file, paste("Starting run", run_id))
    }
    
    # Extract current parameter set
    current_params <- parameter_df[i, , drop = FALSE]
    
    # Create vector list for this specific parameter set
    vector_list <- create_vector_list(
      parameter_df = current_params,
      emissions_df = emissions_df,
      economic_df = economic_df,
      scenario = scenario
    )
    
    # Run either forward_backward_sweep or shooting_method
    # result <- forward_backward_sweep(current_params, vector_list)
    result <- shooting_method(current_params, vector_list, log_file)
    
    # Add run_id to result
    result$run_id <- run_id
    
    # Add parameters to result
    result$parameters <- current_params
    
    # Log success or failure
    if (verbose) {
      if (result$error) {
        write_log(log_file, paste("Run", run_id, "failed:", result$error_message))
      } else {
        write_log(log_file, paste("Run", run_id, "completed successfully"))
      }
    }
    
    # Optionally save intermediate result
    if(save_intermediate) {
      saveRDS(result, file = paste0("result_", run_id, ".rds"))
    }
    
    return(result)
  })
  
  # Name each element in results_list according to run_id
  names(results_list) <- paste0("run_", timestamp, "_", seq_len(nrow(parameter_df)))
  
  # Log summary
  if (verbose) {
    successful_runs <- sum(sapply(results_list, function(r) !r$error))
    write_log(log_file, paste("Batch completed:", successful_runs, "successful out of", 
                              length(results_list), "total runs"))
  }
  
  return(results_list)
}

#' @title Run Forward-Backward Sweep for Multiple Parameter Sets with Parallel Processing
#' @description
#' Applies the forward-backward sweep method across multiple parameter combinations using parallel processing.
#' This function iterates through each row of a parameter dataframe, running the
#' optimal control algorithm for each set of parameters and collecting the results.
#' 
#' @param parameter_df Data frame containing multiple sets of model parameters (one per row)
#' @param emissions_df Data frame containing emissions scenario data
#' @param economic_df Data frame containing economic scenario data
#' @param scenario String specifying which scenario to use
#' @param n_cores Number of cores to use for parallel processing (default: detectCores() - 1)
#' @param save_intermediate Logical; whether to save intermediate results (default: FALSE)
#' @param verbose Logical; whether to print progress messages (default: FALSE)
#' @return List of results, with each element containing the complete optimal control solution
#'         for a specific parameter set. Each result is tagged with a unique run_id.
#'
run_multiple_sweeps_parallel <- function(parameter_df, 
                                         emissions_df,
                                         economic_df,
                                         scenario,
                                         n_cores = NULL,
                                         save_intermediate = FALSE,
                                         verbose = FALSE) {
  
  # Load required packages for parallel processing
#  library(parallel)
#  library(foreach)
#  library(doParallel)
#  library(iterators)
  
  # Determine number of cores to use
  if (is.null(n_cores)) {
    n_cores <- max(1, detectCores() - 1)  # Use all cores except one
  } else {
    n_cores <- min(n_cores, detectCores())  # Ensure we don't request more cores than available
  }
  
  # Create a timestamp for this batch of runs
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Create log file if verbose
  log_file <- NULL
  if (verbose) {
    log_file <- init_log_file("sweep_batch_parallel", "logs")
    write_log(log_file, paste("Starting parallel batch with", nrow(parameter_df), 
                              "parameter sets using", n_cores, "cores"))
  }
  
  # Set up parallel backend
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  # Create a temporary log directory for parallel processes if needed
  temp_log_dir <- "logs/temp_parallel"
  if (verbose) {
    if (!dir.exists(temp_log_dir)) {
      dir.create(temp_log_dir, recursive = TRUE, showWarnings = FALSE)
    }
    write_log(log_file, paste("Temporary logs will be stored in", temp_log_dir))
  }
  
  # Export necessary variables and functions to the cluster
  clusterExport(cl, varlist = c("create_vector_list", "shooting_method_kkt", 
                                "forward_backward_sweep_kkt", "verify_kkt_conditions", 
                                "check_optimality_conditions", "init_log_file", "write_log",
                                "timestamp", "temp_log_dir", "verbose"), 
                envir = environment())
  
  # Also export packages needed by worker functions
  clusterEvalQ(cl, {
    library(dplyr)
    library(tidyr)
  })
  
  # Use foreach for parallel execution
  results_list <- foreach(i = seq_len(nrow(parameter_df)), 
                          .packages = c("dplyr", "tidyr"),
                          .export = c("create_vector_list", "shooting_method_kkt", 
                                      "forward_backward_sweep_kkt", "init_log_file", "write_log")) %dopar% {
                                        
                                        # Create a run ID for this iteration
                                        run_id <- paste0("run_", timestamp, "_", i)
                                        
                                        # Create per-process log file if verbose
                                        process_log_file <- NULL
                                        if (verbose) {
                                          process_log_file <- file.path(temp_log_dir, paste0("process_", run_id, ".log"))
                                          cat(paste("Starting run", run_id, "\n"), file = process_log_file)
                                        }
                                        
                                        # Extract current parameter set
                                        current_params <- parameter_df[i, , drop = FALSE]
                                        
                                        # Try-catch block to handle errors gracefully
                                        tryCatch({
                                          # Create vector list for this specific parameter set
                                          vector_list <- create_vector_list(
                                            parameter_df = current_params,
                                            emissions_df = emissions_df,
                                            economic_df = economic_df,
                                            scenario = scenario
                                          )
                                          
                                          # Run either forward_backward_sweep_kkt or shooting_method_kkt
                                          # result <- forward_backward_sweep_kkt(current_params, vector_list)
                                          result <- shooting_method_kkt(current_params, vector_list, process_log_file)
                                          
                                          # Add run_id and parameters to result
                                          result$run_id <- run_id
                                          result$parameters <- current_params
                                          result$scenario <- scenario
                                          
                                          # Log success
                                          if (verbose) {
                                            cat(paste("Run", run_id, "completed successfully", "\n"), 
                                                file = process_log_file, append = TRUE)
                                          }
                                          
                                          return(result)
                                          
                                        }, error = function(e) {
                                          # Create error result
                                          error_result <- list(
                                            run_id = run_id,
                                            parameters = current_params,
                                            scenario = scenario,
                                            error = TRUE,
                                            error_message = as.character(e),
                                            error_call = deparse(e$call)
                                          )
                                          
                                          # Log error
                                          if (verbose) {
                                            cat(paste("Run", run_id, "failed:", as.character(e), "\n"), 
                                                file = process_log_file, append = TRUE)
                                          }
                                          
                                          return(error_result)
                                        }) -> result
                                        
                                        # Optionally save intermediate result
                                        if(save_intermediate) {
                                          saveRDS(result, file = paste0("result_", run_id, ".rds"))
                                          if (verbose) {
                                            cat(paste("Saved intermediate result for run", run_id, "\n"), 
                                                file = process_log_file, append = TRUE)
                                          }
                                        }
                                        
                                        return(result)
                                      }
  
  # Stop the cluster
  stopCluster(cl)
  
  # Name each element in results_list according to run_id
  names(results_list) <- paste0("run_", timestamp, "_", seq_len(nrow(parameter_df)))
  
  # Collect and consolidate log files if verbose
  if (verbose) {
    # Count successful and failed runs
    successful_runs <- sum(sapply(results_list, function(r) !r$error))
    failed_runs <- sum(sapply(results_list, function(r) r$error))
    
    write_log(log_file, paste("Parallel batch completed:", successful_runs, 
                              "successful out of", length(results_list), "total runs"))
    
    # Log failed run IDs and reasons
    if (failed_runs > 0) {
      write_log(log_file, "Failed runs:")
      for (result in results_list) {
        if (result$error) {
          write_log(log_file, paste("  -", result$run_id, ":", result$error_message))
        }
      }
    }
    
    # Optionally consolidate all process logs into the main log
    write_log(log_file, "\nProcess logs consolidated below:\n" + 
                "---------------------------------\n")
    
    # Read and append each process log
    process_logs <- list.files(temp_log_dir, pattern = paste0("process_run_", timestamp), 
                               full.names = TRUE)
    for (process_log in process_logs) {
      process_id <- gsub(".*process_(run_[^.]+)\\.log", "\\1", process_log)
      write_log(log_file, paste("\nLog for", process_id, ":"))
      write_log(log_file, paste(readLines(process_log), collapse = "\n"))
    }
  }
  
  return(results_list)
}


#' Save solution with timestamped filename
#'
#' This function takes an existing solution (typically a list), creates a
#' timestamped filename, saves it to a fixed output directory, and
#' optionally assigns it to a timestamped variable in the global environment.
#'
#' @param solution The R object to save
#' @param prefix A prefix for the filename (e.g., "oc_solution")
#' @param scenario Scenario name to include in the filename
#' @param assign_global Logical; whether to assign the solution to a global variable with the timestamped name
#'
#' @return The full path to the saved file (invisibly)
#' @export
#'
#' @examples
#' # save_timestamped_solution(oc_solution, "oc_solution", "SSP3-Baseline")
#'

save_timestamped_solution <- function(solution, prefix, scenario, assign_global = FALSE) {
  # Set the output directory
  output_dir <- here::here("output")
  
  # Create timestamp for unique filename
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  solution_name <- paste0(prefix, "_", scenario, "_", timestamp)
  
  # Save the solution with the unique name
  output_file <- file.path(output_dir, paste0(solution_name, ".RData"))
  
  # Save the solution object with its original name
  original_name <- deparse(substitute(solution))
  temp_env <- new.env()
  temp_env[[original_name]] <- solution
  save(list = original_name, envir = temp_env, file = output_file)
  
  # Assign the solution to a variable with the unique name in the global environment if requested
  if (assign_global) {
    assign(solution_name, solution, envir = .GlobalEnv)
  }
  
  # Print confirmation message
  cat("Solution saved as:", output_file, "\n")
  
  # Return the file path
  invisible(output_file)
}

#' @title Verify KKT Conditions for Optimal Control
#' @description
#' Calculates violations of Karush-Kuhn-Tucker (KKT) conditions for the optimal control
#' solution. This function checks stationarity conditions for interior points and
#' complementary slackness conditions for controls at their bounds.
#' 
#' @param qty_mitig Vector of mitigation quantities
#' @param qty_remov Vector of removal quantities
#' @param adjoint_var Vector of adjoint variable values
#' @param parameter_df Data frame with model parameters
#' @param baseline_annual_emissions Vector of baseline emissions (upper bound for mitigation)
#' @param removal_max Maximum removal quantity (upper bound for removal)
#' 
#' @return List containing KKT violation vectors and summary statistics
#' 
#' @examples
#' kkt_results <- verify_kkt_conditions(result$qty_mitig, result$qty_remov, 
#'                                      result$adjoint_var, parameter_df,
#'                                      result$baseline_annual_emissions)

verify_kkt_conditions <- function(qty_mitig, qty_remov, adjoint_var, parameter_df,
                                  baseline_annual_emissions, removal_max = 200) {
  # Extract required parameters
  n_years <- length(qty_mitig)
  years_rel <- seq(0, n_years - 1)
  
  cost_mitig_unit <- parameter_df$cost_mitig_unit
  cost_remov_unit <- parameter_df$cost_remov_unit
  disc_rate <- parameter_df$disc_rate
  exp_mitig <- parameter_df$exp_mitig
  exp_remov <- parameter_df$exp_remov
  
  # Ensure exp terms are not exactly 1 (causes divide by zero)
  safe_exp_mitig <- ifelse(exp_mitig == 1, 1.001, exp_mitig)
  safe_exp_remov <- ifelse(exp_remov == 1, 1.001, exp_remov)
  
  # Initialize KKT violation vectors
  mitig_kkt_violations <- numeric(n_years)
  remov_kkt_violations <- numeric(n_years)
  
  # Tolerance for determining if control is at bound
  bound_tol <- 1e-10
  
  # Check mitigation KKT conditions
  for (i in 1:n_years) {
    # Calculate stationarity term for mitigation
    # Hamiltonian derivative w.r.t. mitigation
    if (qty_mitig[i] < bound_tol) {
      # Control at lower bound (0)
      # Derivative should be ≥ 0 (since we're maximizing)
      deriv_mitig <- exp_mitig * cost_mitig_unit * 
        (bound_tol)^(exp_mitig-1) * exp(-disc_rate*years_rel[i]) - adjoint_var[i]
      mitig_kkt_violations[i] <- max(0, -deriv_mitig)
    } else if (abs(qty_mitig[i] - baseline_annual_emissions[i]) < bound_tol) {
      # Control at upper bound (baseline emissions)
      # Derivative should be ≤ 0
      deriv_mitig <- exp_mitig * cost_mitig_unit * 
        qty_mitig[i]^(exp_mitig-1) * exp(-disc_rate*years_rel[i]) - adjoint_var[i]
      mitig_kkt_violations[i] <- max(0, deriv_mitig)
    } else {
      # Interior point - derivative should be 0
      deriv_mitig <- exp_mitig * cost_mitig_unit * 
        qty_mitig[i]^(exp_mitig-1) * exp(-disc_rate*years_rel[i]) - adjoint_var[i]
      mitig_kkt_violations[i] <- abs(deriv_mitig)
    }
  }
  
  # Check removal KKT conditions
  for (i in 1:n_years) {
    # Calculate stationarity term for removal
    if (qty_remov[i] < bound_tol) {
      # Control at lower bound (0)
      # Derivative should be ≥ 0
      deriv_remov <- exp_remov * cost_remov_unit * 
        (bound_tol)^(exp_remov-1) * exp(-disc_rate*years_rel[i]) - adjoint_var[i]
      remov_kkt_violations[i] <- max(0, -deriv_remov)
    } else if (abs(qty_remov[i] - removal_max) < bound_tol) {
      # Control at upper bound
      # Derivative should be ≤ 0
      deriv_remov <- exp_remov * cost_remov_unit * 
        qty_remov[i]^(exp_remov-1) * exp(-disc_rate*years_rel[i]) - adjoint_var[i]
      remov_kkt_violations[i] <- max(0, deriv_remov)
    } else {
      # Interior point - derivative should be 0
      deriv_remov <- exp_remov * cost_remov_unit * 
        qty_remov[i]^(exp_remov-1) * exp(-disc_rate*years_rel[i]) - adjoint_var[i]
      remov_kkt_violations[i] <- abs(deriv_remov)
    }
  }
  
  # Calculate combined KKT violations
  combined_violations <- pmax(mitig_kkt_violations, remov_kkt_violations)
  
  # Return KKT violations and summary statistics
  return(list(
    mitig_kkt_violations = mitig_kkt_violations,
    remov_kkt_violations = remov_kkt_violations,
    combined_violations = combined_violations,
    mean_violation = mean(combined_violations),
    max_violation = max(combined_violations),
    violation_quantiles = quantile(combined_violations, probs = c(0.5, 0.75, 0.9, 0.95, 0.99))
  ))
}

#' @title Calculate Optimality Score for Optimal Control Solution
#' @description
#' Calculates an overall optimality score based on terminal constraint satisfaction
#' and KKT condition violations. The score ranges from 0 (poor) to 1 (perfect).
#' 
#' @param result Result object from forward_backward_sweep
#' @param target_emissions Target terminal emissions
#' @param emission_tolerance Tolerance for terminal emissions (default: 0.1)
#' @param kkt_tolerance Tolerance for KKT violations (default: 0.001)
#' 
#' @return Numeric score between 0 and 1
#' 
#' @examples
#' score <- check_optimality_conditions(result, co2_target_2100)
check_optimality_conditions <- function(result, target_emissions, 
                                        emission_tolerance = 0.1,
                                        kkt_tolerance = 0.001) {
  # Calculate emission gap
  actual_emissions <- utils::tail(result$cumulative_emissions, 1)
  emission_gap <- actual_emissions - target_emissions
  
  # Calculate normalized emission gap score (0 = perfect, 1 = exceeds tolerance)
  emission_score <- min(1, abs(emission_gap) / emission_tolerance)
  
  # Calculate KKT violation score (0 = perfect, 1 = exceeds tolerance)
  if (!is.null(result$mean_kkt_violation)) {
    kkt_score <- min(1, result$mean_kkt_violation / kkt_tolerance)
  } else {
    # If mean_kkt_violation isn't available, try calculating it
    kkt_results <- verify_kkt_conditions(result$qty_mitig, result$qty_remov,
                                         result$adjoint_var, result$parameters,
                                         result$baseline_annual_emissions)
    kkt_score <- min(1, kkt_results$mean_violation / kkt_tolerance)
  }
  
  # Calculate weighted optimality score (higher is better)
  # Give more weight to emission constraint if it's an equality constraint
  optimality_score <- 1 - (0.6 * emission_score + 0.4 * kkt_score)
  
  return(optimality_score)
}

#' @title Generate KKT Condition Diagnostic Plot
#' @description
#' Creates a diagnostic plot showing KKT condition violations across the time horizon.
#' This helps identify specific time periods where the solution violates optimality.
#' 
#' @param result Result object from forward_backward_sweep or shooting_method
#' @param parameter_df Data frame with model parameters
#' 
#' @return ggplot object with KKT violation visualization
#' 
#' @examples
#' plot <- plot_kkt_diagnostics(result, parameter_df)
#' print(plot)
plot_kkt_diagnostics <- function(result, parameter_df) {
  # If KKT violations haven't been calculated, do it now
  if (is.null(result$kkt_violations)) {
    kkt_results <- verify_kkt_conditions(result$qty_mitig, result$qty_remov,
                                         result$adjoint_var, parameter_df,
                                         result$baseline_annual_emissions)
    mitig_violations <- kkt_results$mitig_kkt_violations
    remov_violations <- kkt_results$remov_kkt_violations
  } else {
    # Use pre-calculated values
    mitig_violations <- result$mitig_kkt_violations
    remov_violations <- result$remov_kkt_violations
  }
  
  # Create data frame for plotting
  plot_data <- data.frame(
    year = result$years,
    mitig_violations = mitig_violations,
    remov_violations = remov_violations
  )
  
  # Transform to long format for ggplot
  plot_data_long <- tidyr::pivot_longer(
    plot_data,
    cols = c(mitig_violations, remov_violations),
    names_to = "violation_type",
    values_to = "violation"
  )
  
  # Create the plot
  p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = year, y = violation, 
                                                    color = violation_type)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0.001, linetype = "dashed", color = "red") +
    ggplot2::scale_color_manual(values = c("mitig_violations" = "blue", 
                                           "remov_violations" = "green"),
                                labels = c("Mitigation", "Removal")) +
    ggplot2::labs(
      title = "KKT Condition Violations Over Time",
      subtitle = "Values below dashed line indicate good KKT condition satisfaction",
      x = "Year",
      y = "Violation Magnitude",
      color = "Control Variable"
    ) +
    ggplot2::theme_minimal()
  
  return(p)
}

