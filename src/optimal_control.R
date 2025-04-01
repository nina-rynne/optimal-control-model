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

# Load required packages
library(dplyr)    # For data manipulation
library(tidyr)    # For reshaping data
library(here)     # For file path management

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
  temperature_anomaly[1] <- clim_temp_init + tcre * cumulative_emissions[1]
  
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
  
  # Use lapply to process each parameter set
  results_list <- lapply(seq_len(nrow(parameter_df)), function(i) {
    
    # Create a run ID for this iteration that includes the timestamp
    run_id <- paste0("run_", timestamp, "_", i)
    
    # Extract current parameter set
    current_params <- parameter_df[i, , drop = FALSE]
    
    # Create vector list for this specific parameter set
    vector_list <- create_vector_list(
      parameter_df = current_params,
      emissions_df = emissions_df,
      economic_df = economic_df,
      scenario = scenario
    )
    
    # Run either forward_backward_sweep or shooting_method. Use only one.
    result <- forward_backward_sweep(current_params, vector_list)
    #result <- shooting_method(current_params, vector_list)
    
    # Add run_id to result
    result$run_id <- run_id
    
    # Add parameters to result
    result$parameters <- current_params
    
    # Optionally save intermediate result
    if(save_intermediate) {
      saveRDS(result, file = paste0("result_", run_id, ".rds"))
    }
    
    return(result)
  })
  
  # Name each element in results_list according to run_id
  names(results_list) <- paste0("run_", timestamp, "_", seq_len(nrow(parameter_df)))
  
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
  library(parallel)
  library(foreach)
  library(doParallel)
  library(iterators)
  
  # Determine number of cores to use
  if (is.null(n_cores)) {
    n_cores <- max(1, detectCores() - 1)  # Use all cores except one
  } else {
    n_cores <- min(n_cores, detectCores())  # Ensure we don't request more cores than available
  }
  
  if (verbose) {
    cat(sprintf("Running with %d cores\n", n_cores))
    cat(sprintf("Processing %d parameter sets\n", nrow(parameter_df)))
  }
  
  # Create a timestamp for this batch of runs
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Set up parallel backend
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  # Export necessary variables and functions to the cluster
  clusterExport(cl, varlist = c("create_vector_list", "shooting_method", 
                                "forward_backward_sweep"), 
                envir = environment())
  
  # Also export packages needed by worker functions
  clusterEvalQ(cl, {
    library(dplyr)
    library(tidyr)
  })
  
  # Use foreach for parallel execution
  results_list <- foreach(i = seq_len(nrow(parameter_df)), 
                          .packages = c("dplyr", "tidyr"),
                          .export = c("create_vector_list", "shooting_method", 
                                      "forward_backward_sweep")) %dopar% {
                                        
                                        # Create a run ID for this iteration
                                        run_id <- paste0("run_", timestamp, "_", i)
                                        
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
                                        #result <- forward_backward_sweep(current_params, vector_list)
                                        result <- shooting_method(current_params, vector_list)
                                        
                                        # Add run_id and parameters to result
                                        result$run_id <- run_id
                                        result$parameters <- current_params
                                        
                                        # Optionally save intermediate result
                                        if(save_intermediate) {
                                          saveRDS(result, file = paste0("result_", run_id, ".rds"))
                                        }
                                        
                                        return(result)
                                      }
  
  # Stop the cluster
  stopCluster(cl)
  
  # Name each element in results_list according to run_id
  names(results_list) <- paste0("run_", timestamp, "_", seq_len(nrow(parameter_df)))
  
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

