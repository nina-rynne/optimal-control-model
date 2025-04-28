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

old_shooting_method <- function(parameter_df,
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

###----- ERROR HANDLING VERSION -----###

#' @title Shooting Method for Optimal Control with Error Handling
#' @description
#' Implements the shooting method for finding optimal control strategies for 
#' temperature overshoot management. The function finds optimal mitigation and 
#' carbon dioxide removal paths with robust error handling.
#' 
#' @param parameter_df Data frame containing model parameters
#' @param vector_list List of vectors required for optimal control calculation
#' @param log_file Optional log file path for detailed logging
#' @return List of vectors with optimal control solution and all state variables
#'         or an error message if calculation failed
#' @examples
#' # Run with standard parameter set
#' result <- shooting_method(parameter_df, vector_list)
#'

shooting_method <- function(parameter_df,
                            vector_list,
                            log_file = NULL) {
  
  # Create result structure with an error field
  result <- list(
    error = FALSE,
    error_message = NULL
  )
  
  # Validation checks
  if (is.null(parameter_df) || !is.data.frame(parameter_df)) {
    result$error <- TRUE
    result$error_message <- "Invalid parameter data frame provided"
    return(result)
  }
  
  if (is.null(vector_list) || !is.list(vector_list)) {
    result$error <- TRUE
    result$error_message <- "Invalid vector list provided"
    return(result)
  }
  
  # Check required parameters
  required_params <- c("tcre", "clim_temp_init", "trans_low", "trans_high", "co2_target_2100")
  missing_params <- required_params[!required_params %in% names(parameter_df)]
  if (length(missing_params) > 0) {
    result$error <- TRUE
    result$error_message <- paste("Missing required parameters:", 
                                  paste(missing_params, collapse = ", "))
    return(result)
  }
  
  # Wrap the calculation in a try-catch to handle unexpected errors
  tryCatch({
    # Algorithm settings
    convergence_tolerance <- 0.001
    converged <- FALSE
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
    
    # Log iteration start if log file provided
    if (!is.null(log_file)) {
      write_log(log_file, "Starting shooting method")
    }
    
    # First evaluation at lower bound
    result_low <- forward_backward_sweep(parameter_df, vector_list, trans_low)
    
    # Check if forward_backward_sweep returned an error
    if (result_low$error) {
      result$error <- TRUE
      result$error_message <- paste("Lower bound evaluation failed:", result_low$error_message)
      return(result)
    }
    
    # Calculate deviation from target
    emission_gap_low <- utils::tail(result_low$cumulative_emissions, 1) - co2_target_2100
    
    # First evaluation at upper bound
    result_high <- forward_backward_sweep(parameter_df, vector_list, trans_high)
    
    # Check if forward_backward_sweep returned an error
    if (result_high$error) {
      result$error <- TRUE
      result$error_message <- paste("Upper bound evaluation failed:", result_high$error_message)
      return(result)
    }
    
    # Calculate deviation from target
    emission_gap_high <- utils::tail(result_high$cumulative_emissions, 1) - co2_target_2100
    
    # Check if bounds are suitable
    if (sign(emission_gap_low) == sign(emission_gap_high)) {
      result$error <- TRUE
      result$error_message <- paste(
        "Solution may not exist in specified bounds. Low gap:", emission_gap_low,
        "High gap:", emission_gap_high
      )
      return(result)
    }
    
    # Main secant iteration loop
    while (!is.na(converged) && !converged && !is.na(iteration) && iteration < max_iterations) {
      iteration <- iteration + 1
      
      # Also add this inside the loop to check for NAs:
      if (any(is.na(c(converged, iteration)))) {
        result$error <- TRUE
        result$error_message <- "NA values detected in convergence check"
        return(result)
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
      # Avoid division by zero
      if (abs(emission_gap_high - emission_gap_low) < 1e-10) {
        result$error <- TRUE
        result$error_message <- "Division by zero in secant method"
        return(result)
      }
      
      adj_step <- emission_gap_low * (trans_high - trans_low) / (emission_gap_high - emission_gap_low)
      
      # Update values for next iteration
      trans_high <- trans_low
      emission_gap_high <- emission_gap_low
      trans_low <- trans_low - adj_step
      
      # Evaluate at new point
      result_current <- forward_backward_sweep(parameter_df, vector_list, trans_low)
      
      # Check if forward_backward_sweep returned an error
      if (result_current$error) {
        result$error <- TRUE
        result$error_message <- paste("Iteration", iteration, "failed:", result_current$error_message)
        return(result)
      }
      
      emission_gap_low <- utils::tail(result_current$cumulative_emissions, 1) - co2_target_2100
      
      # Log progress if log file provided
      if (!is.null(log_file)) {
        write_log(log_file, sprintf("Iteration %d: trans_low = %.6f, gap = %.6f", 
                                    iteration, trans_low, emission_gap_low))
      }
      
      # Check convergence
      if(abs(emission_gap_low) <= convergence_tolerance) {
        converged <- TRUE
      }
    }
    
    # Check if we hit max iterations without converging
    if (iteration >= max_iterations && !converged) {
      result$error <- TRUE
      result$error_message <- paste("Failed to converge after", max_iterations, 
                                    "iterations. Best emission gap:", emission_gap_low)
      return(result)
    }
    
    # If we've reached here, we have a successful result
    # Build the complete result structure from result_current
    result <- result_current
    
    # Add shooting method specific information
    result$shooting_iterations <- iteration
    result$shooting_converged <- converged
    result$emission_gap <- emission_gap_low
    result$trans_value <- trans_low
    
    return(result)
    
  }, error = function(e) {
    # Catch any other errors that we didn't explicitly handle
    return(list(
      error = TRUE,
      error_message = paste("Shooting method error:", e$message)
    ))
  })
}

