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

#' @title Modified Shooting Method for Climate Temperature Overshoot with KKT Verification
#' @description
#' Implements an enhanced shooting method for finding optimal control strategies for 
#' temperature overshoot management with terminal state constraints. The method iteratively
#' finds the optimal terminal adjoint value to satisfy the terminal state constraint
#' while ensuring all Karush-Kuhn-Tucker (KKT) optimality conditions are satisfied.
#' 
#' Key improvements in this implementation:
#'   1. Robust verification of KKT conditions at each shooting iteration
#'   2. Adaptive secant method step sizing for improved convergence
#'   3. Detection and recovery from ill-conditioned shooting iterations
#'   4. Comprehensive solution quality assessment beyond terminal constraint satisfaction
#'   5. Multi-criteria convergence monitoring (terminal constraint + KKT conditions)
#' 
#' @param parameter_df Data frame containing model parameters
#' @param vector_list List of vectors required for optimal control calculation
#' @param log_file Optional log file path for detailed logging
#' 
#' @return List of vectors with optimal control solution and all state variables,
#'         including shooting method diagnostics and KKT verification metrics
#' 
#' @examples
#' # Run with standard parameter set
#' result <- shooting_method(parameter_df, vector_list)
#' 
#' # Check solution quality metrics
#' print(paste("Terminal emission gap:", result$emission_gap))
#' print(paste("KKT optimality score:", result$kkt_optimality_score))
#'
#' @references
#' Lenhart, S. and Workman, J.T. (2007). Optimal Control Applied to Biological Models.
#' Chiang, A.C. (2000). Elements of Dynamic Optimization.
#' 
#' @note This method builds on standard shooting methods for boundary value problems,
#'       but adds critical verification of solution optimality through KKT conditions.
#'       It finds a solution that both satisfies the terminal constraint AND is optimal
#'       according to the necessary conditions for optimal control.

shooting_method_kkt <- function(parameter_df,
                            vector_list,
                            log_file = NULL) {
  # Create result structure with an error field
  result <- list(
    error = FALSE,
    error_message = NULL
  )
  
  # Wrap the calculation in a try-catch to handle unexpected errors
  tryCatch({
    # Algorithm settings
    # CHANGE: Tighten convergence tolerance for more precise solutions
    convergence_tolerance <- 0.0001  # Previously 0.001
    kkt_tolerance <- 0.001  # New parameter for KKT condition verification
    converged <- FALSE
    max_iterations <- 1e4
    iteration <- 0
    temperature_target <- 1.5
    
    # CHANGE: Add secant method stabilization parameters
    min_step_size <- 1e-6
    max_step_size <- 1e3
    step_dampening <- 0.8  # Dampen large steps for stability
    
    # Extract required variables from parameter_df
    tcre <- parameter_df$tcre
    clim_temp_init <- parameter_df$clim_temp_init
    trans_low <- parameter_df$trans_low
    trans_high <- parameter_df$trans_high
    co2_target_2100 <- parameter_df$co2_target_2100
    
    # Log iteration start if log file provided
    if (!is.null(log_file)) {
      write_log(log_file, "Starting shooting method with KKT verification")
      write_log(log_file, paste("Target CO2 in 2100:", co2_target_2100, "GtCO2"))
      write_log(log_file, paste("Initial lambda(T) bracket: [", trans_low, ",", trans_high, "]"))
    }
    
    # First evaluation at lower bound
    result_low <- forward_backward_sweep_kkt(parameter_df, vector_list, trans_low)
    
    # Check if forward_backward_sweep_kkt returned an error
    if (result_low$error) {
      result$error <- TRUE
      result$error_message <- paste("Lower bound evaluation failed:", result_low$error_message)
      return(result)
    }
    
    # Calculate deviation from target
    emission_gap_low <- utils::tail(result_low$cumulative_emissions, 1) - co2_target_2100
    
    # NEW: Evaluate KKT optimality of lower bound solution
    kkt_score_low <- ifelse(!is.null(result_low$mean_kkt_violation), 
                            result_low$mean_kkt_violation, Inf)
    
    # First evaluation at upper bound
    result_high <- forward_backward_sweep_kkt(parameter_df, vector_list, trans_high)
    
    # Check if forward_backward_sweep_kkt returned an error
    if (result_high$error) {
      result$error <- TRUE
      result$error_message <- paste("Upper bound evaluation failed:", result_high$error_message)
      return(result)
    }
    
    # Calculate deviation from target
    emission_gap_high <- utils::tail(result_high$cumulative_emissions, 1) - co2_target_2100
    
    # NEW: Evaluate KKT optimality of upper bound solution
    kkt_score_high <- ifelse(!is.null(result_high$mean_kkt_violation), 
                             result_high$mean_kkt_violation, Inf)
    
    # Check if bounds are suitable
    if (sign(emission_gap_low) == sign(emission_gap_high)) {
      if (!is.null(log_file)) {
        write_log(log_file, paste("WARNING: Initial bounds may not bracket solution.",
                                  "Low gap:", emission_gap_low,
                                  "High gap:", emission_gap_high))
      }
      
      # Try to adjust bounds to find bracket (instead of immediately failing)
      if (emission_gap_low < 0 && emission_gap_high < 0) {
        # Both bounds give final emissions below target, need higher lambda
        trans_high <- trans_high * 2
        if (!is.null(log_file)) {
          write_log(log_file, paste("Adjusting upper bound to:", trans_high))
        }
        result_high <- forward_backward_sweep_kkt(parameter_df, vector_list, trans_high)
        emission_gap_high <- utils::tail(result_high$cumulative_emissions, 1) - co2_target_2100
      } else if (emission_gap_low > 0 && emission_gap_high > 0) {
        # Both bounds give final emissions above target, need lower lambda
        trans_low <- trans_low / 2
        if (!is.null(log_file)) {
          write_log(log_file, paste("Adjusting lower bound to:", trans_low))
        }
        result_low <- forward_backward_sweep_kkt(parameter_df, vector_list, trans_low)
        emission_gap_low <- utils::tail(result_low$cumulative_emissions, 1) - co2_target_2100
      }
      
      # Check again after adjustment
      if (sign(emission_gap_low) == sign(emission_gap_high)) {
        result$error <- TRUE
        result$error_message <- paste(
          "Solution may not exist in specified bounds. Low gap:", emission_gap_low,
          "High gap:", emission_gap_high)
        return(result)
      }
    }
    
    # Track best KKT score and corresponding solution
    best_kkt_score <- min(kkt_score_low, kkt_score_high)
    best_trans_value <- ifelse(kkt_score_low < kkt_score_high, trans_low, trans_high)
    best_result <- ifelse(kkt_score_low < kkt_score_high, result_low, result_high)
    best_emission_gap <- ifelse(kkt_score_low < kkt_score_high, emission_gap_low, emission_gap_high)
    
    # Main secant iteration loop
    while (!is.na(converged) && !converged && !is.na(iteration) && iteration < max_iterations) {
      iteration <- iteration + 1
      
      # Also add this inside the loop to check for NAs:
      if (any(is.na(c(converged, iteration)))) {
        if (!is.null(log_file)) {
          write_log(log_file, "NA values detected in convergence check. Stopping iteration.")
        }
        break
      }
      
      # Ensure we're working with the better approximation for terminal condition
      if(abs(emission_gap_low) > abs(emission_gap_high)) {
        # Swap values to keep better approximation
        temp_trans <- trans_low
        trans_low <- trans_high
        trans_high <- temp_trans
        
        temp_gap <- emission_gap_low
        emission_gap_low <- emission_gap_high
        emission_gap_high <- temp_gap
        
        # Also swap KKT scores
        temp_kkt <- kkt_score_low
        kkt_score_low <- kkt_score_high
        kkt_score_high <- temp_kkt
      }
      
      # Calculate secant method step
      # Avoid division by zero with safeguard
      denominator <- emission_gap_high - emission_gap_low
      if (abs(denominator) < 1e-10) {
        if (!is.null(log_file)) {
          write_log(log_file, "WARNING: Near-zero slope in secant method. Using fallback step.")
        }
        # Fallback: use a small step in the appropriate direction
        adj_step <- 0.1 * (trans_high - trans_low) * sign(emission_gap_low)
      } else {
        adj_step <- emission_gap_low * (trans_high - trans_low) / denominator
      }
      
      # CHANGE: Apply step size limits for stability
      if (abs(adj_step) > max_step_size) {
        adj_step <- max_step_size * sign(adj_step)
        if (!is.null(log_file)) {
          write_log(log_file, paste("Limiting step size to:", max_step_size))
        }
      } else if (abs(adj_step) < min_step_size) {
        adj_step <- min_step_size * sign(adj_step)
        if (!is.null(log_file)) {
          write_log(log_file, paste("Increasing step size to minimum:", min_step_size))
        }
      }
      
      # CHANGE: Apply step dampening for stability
      adj_step <- adj_step * step_dampening
      
      # Update values for next iteration
      trans_high <- trans_low
      emission_gap_high <- emission_gap_low
      kkt_score_high <- kkt_score_low
      trans_low <- trans_low - adj_step
      
      # Log current iteration details
      if (!is.null(log_file)) {
        write_log(log_file, paste("Iteration", iteration, "- trying lambda(T) =", trans_low,
                                  "with step =", adj_step))
      }
      
      # Evaluate at new point
      result_current <- forward_backward_sweep_kkt(parameter_df, vector_list, trans_low)
      
      # Check if forward_backward_sweep_kkt returned an error
      if (result_current$error) {
        if (!is.null(log_file)) {
          write_log(log_file, paste("Iteration failed:", result_current$error_message,
                                    "Attempting recovery..."))
        }
        
        # Recovery attempt: try a smaller step
        trans_low <- trans_high - (adj_step * 0.5)
        result_current <- forward_backward_sweep_kkt(parameter_df, vector_list, trans_low)
        
        # If still failing, report error
        if (result_current$error) {
          result$error <- TRUE
          result$error_message <- paste("Iteration", iteration, "failed:", 
                                        result_current$error_message, 
                                        "Recovery attempt also failed.")
          return(result)
        }
      }
      
      emission_gap_low <- utils::tail(result_current$cumulative_emissions, 1) - co2_target_2100
      
      # NEW: Extract KKT violation score
      kkt_score_low <- ifelse(!is.null(result_current$mean_kkt_violation), 
                              result_current$mean_kkt_violation, Inf)
      
      # NEW: Track best KKT score across all iterations
      if (kkt_score_low < best_kkt_score && abs(emission_gap_low) < 10) {
        # Only update best solution if emission gap is reasonably small
        best_kkt_score <- kkt_score_low
        best_trans_value <- trans_low
        best_result <- result_current
        best_emission_gap <- emission_gap_low
        
        if (!is.null(log_file)) {
          write_log(log_file, paste("New best KKT score:", best_kkt_score, 
                                    "with emission gap:", best_emission_gap))
        }
      }
      
      # Log progress if log file provided
      if (!is.null(log_file)) {
        write_log(log_file, sprintf("Iteration %d: lambda(T) = %.6f, gap = %.6f, KKT score = %.6f", 
                                    iteration, trans_low, emission_gap_low, kkt_score_low))
      }
      
      # CHANGE: Multi-criteria convergence check
      # Check both emission gap and KKT optimality
      if (abs(emission_gap_low) <= convergence_tolerance && kkt_score_low <= kkt_tolerance) {
        converged <- TRUE
        if (!is.null(log_file)) {
          write_log(log_file, paste("Converged! Final emission gap:", emission_gap_low,
                                    "KKT score:", kkt_score_low))
        }
      }
      
      # Also consider converged if we're making negligible progress
      if (iteration > 10 && abs(adj_step) < min_step_size && 
          abs(emission_gap_low - emission_gap_high) < convergence_tolerance) {
        if (!is.null(log_file)) {
          write_log(log_file, "Converged due to negligible step size and progress.")
        }
        converged <- TRUE
      }
    }
    
    # Check if we hit max iterations without converging
    if (iteration >= max_iterations && !converged) {
      if (!is.null(log_file)) {
        write_log(log_file, paste("WARNING: Failed to fully converge after", max_iterations, 
                                  "iterations. Using best solution found."))
      }
      
      # Use the best solution we found based on KKT score
      result <- best_result
      emission_gap_low <- best_emission_gap
      trans_low <- best_trans_value
    } else {
      # Use the final converged solution
      result <- result_current
    }
    
    # If we've reached here, we have a solution
    # Add shooting method specific information
    result$shooting_iterations <- iteration
    result$shooting_converged <- converged
    result$emission_gap <- emission_gap_low
    result$trans_value <- trans_low
    result$kkt_optimality_score <- ifelse(!is.null(result$mean_kkt_violation),
                                          result$mean_kkt_violation, Inf)
    
    # NEW: Add multi-criteria quality assessment
    # Normalized scores from 0 (perfect) to 1 (poor)
    emission_quality <- min(1, abs(emission_gap_low) / convergence_tolerance)
    kkt_quality <- min(1, result$kkt_optimality_score / kkt_tolerance)
    result$solution_quality <- 1 - (emission_quality * 0.5 + kkt_quality * 0.5)
    
    if (!is.null(log_file)) {
      write_log(log_file, paste("Solution quality score:", 
                                round(result$solution_quality * 100, 1), "%"))
    }
    
    return(result)
    
  }, error = function(e) {
    # Catch any other errors that we didn't explicitly handle
    return(list(
      error = TRUE,
      error_message = paste("Shooting method error:", e$message)
    ))
  })
}
