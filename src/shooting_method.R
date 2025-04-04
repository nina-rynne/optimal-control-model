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
#' @dependencies
#' Required packages: dplyr, tidyr, here
#' 

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
  temperature_target <- 1.5
  
  # Extract required variables from parameter_df
  tcre <- parameter_df$tcre
  clim_temp_init <- parameter_df$clim_temp_init
  trans_low <- parameter_df$trans_low
  trans_high <- parameter_df$trans_high
  co2_target_2100 <- parameter_df$co2_target_2100
  #co2_target_2100 <- ((temperature_target - clim_temp_init) / tcre) * 1000
  
  # First evaluation at lower bound
  result_low <- forward_backward_sweep(parameter_df, vector_list, trans_low)
  
  # Calculate deviation from target
  emission_gap_low <- utils::tail(result_low$cumulative_emissions, 1) - co2_target_2100
  
  # First evaluation at upper bound
  result_high <- forward_backward_sweep(parameter_df, vector_list, trans_high)
  
  # Calculate deviation from target
  emission_gap_high <- utils::tail(result_high$cumulative_emissions, 1) - co2_target_2100
  
  # Main secant iteration loop
  while (!is.na(converged) && !converged && 
         !is.na(iteration) && iteration < max_iterations) {
    iteration <- iteration + 1
    
    # Also add this inside the loop to check for NAs:
    if (any(is.na(c(converged, iteration)))) {
      warning("NA values detected in convergence check. Stopping iteration.")
      break
    }
    
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

