#' @title Shooting Method for Climate Temperature Overshoot
#' @description
#' This script implements the shooting method to find optimal mitigation and 
#'  carbon dioxide removal (CDR) strategies.
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

# Load required packages
library(dplyr)    # For data manipulation
library(tidyr)    # For reshaping data
library(here)     # For file path management


#' @title Shooting Method for Optimal Control
#' @description
#' Implements the shooting method for finding optimal control strategies for 
#' temperature overshoot management. The function finds optimal mitigation and 
#' carbon dioxide removal paths.
#' 
#' @param parameter_df Data frame containing model parameters
#' @param vector_list List of vectors required for optimal control calculation
#' @param log_file Optional log file path for detailed logging
#' @return List of vectors with optimal control solution and all state variables
#' @examples
#' # Run with standard parameter set
#' result <- shooting_method(parameter_df, vector_list)
#'

shooting_method <- function(parameter_df,
                            vector_list) {
  # Algorithm settings
  convergence_tolerance <- 0.001
  converged = FALSE
  max_iterations <- 1e4
  iteration <- 0
  
  # Extract required variables from parameter_df
  trans_low <- parameter_df$trans_low
  trans_high <- parameter_df$trans_high
  co2_target_2100 <- parameter_df$co2_target_2100
  
  # First evaluation at lower bound
  result_low <- forward_backward_sweep(parameter_df, vector_list, trans_low)
  
  # Calculate deviation from target
  emission_gap_low <- utils::tail(result_low$cumulative_emissions, 1) - co2_target_2100
  
  # First evaluation at upper bound
  result_high <- forward_backward_sweep(parameter_df, vector_list, trans_high)
  
  # Calculate deviation from target
  emission_gap_high <- utils::tail(result_high$cumulative_emissions, 1) - co2_target_2100
  
  # Main secant iteration loop
  while(!converged && iteration < max_iterations) {
    iteration <- iteration + 1
    
    # Ensure we're working with the better approximation
    if(abs(emission_gap_low) > abs(emission_gap_high)) {
      # Swap values to keep better approximation
      temp_trans <- trans_low
      trans_low <- trans_high
      trans_high <- temp_trans
      
      temp_gap <- emission_gap_low
      emission_gap_low <- emission_gap_high
      emission_gap_high <- temp_gap
    }
    
    # Calculate secant method step
    adj_step <- emission_gap_low * (trans_high - trans_low) / (emission_gap_high - emission_gap_low)
    
    # Update values for next iteration
    trans_high <- trans_low
    emission_gap_high <- emission_gap_low
    trans_low <- trans_low - adj_step
    
    # Evaluate at new point
    result <- forward_backward_sweep(parameter_df, vector_list, trans_low)
    emission_gap_low <- tail(result$cumulative_emissions, 1) - co2_target_2100
    
    # Check convergence
    if(abs(emission_gap_low) <= convergence_tolerance || iteration >= max_iterations) {
      converged <- TRUE
    }
  }
  
  # Return the final result
  return(result)
}


#' @title Shooting Method for Optimal Control
#' @description
#' Implements the shooting method for finding optimal control strategies for 
#' temperature overshoot management. The function finds optimal mitigation and 
#' carbon dioxide removal paths.
#' 
#' @param parameter_df Data frame containing model parameters
#' @param vector_list List of vectors required for optimal control calculation
#' @param log_file Optional log file path for detailed logging
#' @return List of vectors with optimal control solution and all state variables
#' @examples
#' # Run with standard parameter set
#' result <- shooting_method(parameter_df, vector_list)
#'

nacheck_shooting_method <- function(parameter_df,
                            vector_list) {
  # Algorithm settings
  convergence_tolerance <- 0.001
  converged = FALSE
  max_iterations <- 1e4
  iteration <- 0
  
  # Extract required variables from parameter_df
  trans_low <- parameter_df$trans_low
  trans_high <- parameter_df$trans_high
  co2_target_2100 <- parameter_df$co2_target_2100
  
  # First evaluation at lower bound
  result_low <- forward_backward_sweep(parameter_df, vector_list, trans_low)
  
  # Calculate deviation from target
  emission_gap_low <- utils::tail(result_low$cumulative_emissions, 1) - co2_target_2100
  
  # First evaluation at upper bound
  result_high <- forward_backward_sweep(parameter_df, vector_list, trans_high)
  
  # Calculate deviation from target
  emission_gap_high <- utils::tail(result_high$cumulative_emissions, 1) - co2_target_2100
  
  # Main secant iteration loop
  # Add protection against NA values in the loop condition
  while ((!is.na(converged) && !converged) && 
         (!is.na(iteration) && iteration < max_iterations)) {
    
    # Add NA check at the beginning of each iteration
    if (any(is.na(c(converged, iteration)))) {
      warning("NA values detected in convergence check. Stopping iteration.")
      break
    }
    
    iteration <- iteration + 1
    
    # Ensure we're working with the better approximation
    if(abs(emission_gap_low) > abs(emission_gap_high)) {
      # Swap values to keep better approximation
      temp_trans <- trans_low
      trans_low <- trans_high
      trans_high <- temp_trans
      
      temp_gap <- emission_gap_low
      emission_gap_low <- emission_gap_high
      emission_gap_high <- temp_gap
    }
    
    # Calculate secant method step
    adj_step <- emission_gap_low * (trans_high - trans_low) / (emission_gap_high - emission_gap_low)
    
    # Update values for next iteration
    trans_high <- trans_low
    emission_gap_high <- emission_gap_low
    trans_low <- trans_low - adj_step
    
    # Evaluate at new point
    result <- forward_backward_sweep(parameter_df, vector_list, trans_low)
    emission_gap_low <- tail(result$cumulative_emissions, 1) - co2_target_2100
    
    # Check convergence
    if(abs(emission_gap_low) <= convergence_tolerance || iteration >= max_iterations) {
      converged <- TRUE
    }
  }
  
  # Return the final result
  return(result)
}



#' @title Shooting Method for Optimal Control
#' @description
#' Implements the shooting method for finding optimal control strategies for 
#' temperature overshoot management. The function finds optimal mitigation and 
#' carbon dioxide removal paths.
#' 
#' @param parameter_df Data frame containing model parameters
#' @param vector_list List of vectors required for optimal control calculation
#' @param log_file Optional log file path for detailed logging
#' @return List of vectors with optimal control solution and all state variables
#' @examples
#' # Run with standard parameter set
#' result <- shooting_method(parameter_df, vector_list)
#'

extra_test_shooting_method <- function(params, vector_list) {
  # Start with verbose logging
  cat("Starting shooting_method with parameter set:\n")
  print(head(params))
  
  # Initialize variables
  convergence_tolerance <- 0.001
  iteration <- 0
  converged <- FALSE
  max_iterations <- 1000  # Adjust as needed
  
  # Extract required variables from parameter_df
  trans_low <- parameter_df$trans_low
  trans_high <- parameter_df$trans_high
  co2_target_2100 <- parameter_df$co2_target_2100
  
  # First evaluation at lower bound
  result_low <- forward_backward_sweep(parameter_df, vector_list, trans_low)
  
  # Calculate deviation from target
  emission_gap_low <- utils::tail(result_low$cumulative_emissions, 1) - co2_target_2100
  
  # First evaluation at upper bound
  result_high <- forward_backward_sweep(parameter_df, vector_list, trans_high)
  
  # Calculate deviation from target
  emission_gap_high <- utils::tail(result_high$cumulative_emissions, 1) - co2_target_2100
  
  # Debug - before loop
  cat("Before loop - converged:", converged, 
      "iteration:", iteration, 
      "max_iterations:", max_iterations, "\n")
  
  # Make sure loop variables are initialized properly
  if (is.na(converged)) {
    warning("converged is NA before entering loop")
    converged <- FALSE  # Set a default value
  }
  
  # Loop with additional safety checks
  while (!is.na(converged) && !converged && 
         !is.na(iteration) && iteration < max_iterations) {
    
    # Debug - loop start
    cat("Iteration", iteration, "starting\n")
    
    # Ensure we're working with the better approximation
    if(abs(emission_gap_low) > abs(emission_gap_high)) {
      # Swap values to keep better approximation
      temp_trans <- trans_low
      trans_low <- trans_high
      trans_high <- temp_trans
      
      temp_gap <- emission_gap_low
      emission_gap_low <- emission_gap_high
      emission_gap_high <- temp_gap
    }
    
    # Calculate secant method step
    adj_step <- emission_gap_low * (trans_high - trans_low) / (emission_gap_high - emission_gap_low)
    
    # Update values for next iteration
    trans_high <- trans_low
    emission_gap_high <- emission_gap_low
    trans_low <- trans_low - adj_step
    
    # Evaluate at new point
    result <- forward_backward_sweep(parameter_df, vector_list, trans_low)
    emission_gap_low <- tail(result$cumulative_emissions, 1) - co2_target_2100
    
    # Check convergence
    if(abs(emission_gap_low) <= convergence_tolerance || iteration >= max_iterations) {
      converged <- TRUE
    }
    
    # Check for NAs in key variables
    # List all critical variables that might become NA
    critical_vars <- c(converged, iteration)
    critical_var_names <- c("converged", "iteration")
    
    na_indices <- which(is.na(critical_vars))
    if (length(na_indices) > 0) {
      warning("NA detected in: ", 
              paste(critical_var_names[na_indices], collapse=", "))
      break
    }
    
    # Debug - convergence update
    cat("Iteration", iteration, "- converged:", converged, "\n")
    
    # Increment iteration counter
    iteration <- iteration + 1
  }
  
  # Debug - after loop
  cat("After loop - converged:", converged, 
      "iterations completed:", iteration, "\n")
  
  # Return the final result
  return(result)
}