#' @title Forward Backward Sweep for Climate Temperature Overshoot
#' @description
#' This script implements forward-backward sweep and shooting methods
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

#' @title Forward-Backward Sweep for Optimal Control NO ERROR HANDLING
#' @description
#' Implements the forward-backward sweep method for finding optimal control
#' strategies for temperature overshoot management. The function finds optimal
#' mitigation and carbon dioxide removal paths.
#' 
#' @param parameter_df Data frame containing model parameters
#' @param vector_list List of vectors required for optimal control calculation
#' @param trans_cond Transversality condition value. Default = 0
#' @param log_file Optional log file path for detailed logging
#' @return List of vectors with optimal control solution and all state variables
#' @examples
#' # Run with standard parameter set
#' result <- forward_backward_sweep(parameter_df, vector_list)
#'

old_forward_backward_sweep <- function(parameter_df,
                                   vector_list,
                                   trans_cond = 0) {
  # Algorithm settings
  time_step <- 1
  convergence_tolerance <- 0.001
  min_convergence <- -1
  max_iterations <- 1e4
  iteration <- 0
  upd_weight <- 0.01
  
  # Control variable limits
  # TO BE MOVED TO model_parameters.R or parameter_details.yml
  mitigation_min <- 0
  removal_min <- 0
  removal_max <- 100  # Could be parameterized based on technical limits
  
  # Extract required variables from parameter_df
  clim_temp_init <- parameter_df$clim_temp_init
  tcre <- parameter_df$tcre
  cost_mitig_unit <- parameter_df$cost_mitig_unit
  cost_remov_unit <- parameter_df$cost_remov_unit
  econ_dam_pct <- parameter_df$econ_dam_pct
  disc_rate <- parameter_df$disc_rate
  exp_mitig <- parameter_df$exp_mitig
  exp_remov <- parameter_df$exp_remov
  exp_temp_anom <- parameter_df$exp_temp_anom
  
  # Extract required vectors from vector_list
  # Time variables
  years <- vector_list$years
  years_rel <- vector_list$years_rel
  n_years <- vector_list$n_years
  
  # State variables
  baseline_annual_emissions <- vector_list$baseline_annual_emissions
  temperature_anomaly <- vector_list$temperature_anomaly
  cumulative_emissions <- vector_list$cumulative_emissions
  
  # Control variables and bounds
  qty_mitig <- vector_list$qty_mitig
  qty_remov <- vector_list$qty_remov
  
  # Adjoint variable
  adjoint_var <- vector_list$adjoint_var
  adjoint_var[n_years] <- trans_cond
  
  # Cost components
  baseline_annual_gwp <- vector_list$baseline_annual_gwp
  cost_mitig_cumul <- vector_list$cost_mitig_cumul
  cost_remov_cumul <- vector_list$cost_remov_cumul
  cost_resid_cumul <- vector_list$cost_resid_cumul
  cost_total_cumul <- vector_list$cost_total_cumul
  
  ### ---------- BEGIN ITERATION LOOP ------------ ###
  
  while((is.na(min_convergence) || min_convergence < 0) && iteration < max_iterations){
    
    # Save control variables, cumulative emissions and adjoint variable for
    # convergence testing in last step
    previous_qty_mitig <- qty_mitig
    previous_qty_remov <- qty_remov
    previous_cumulative_emissions <- cumulative_emissions
    previous_adjoint_var <- adjoint_var

    ### ---------- FORWARD SWEEP ------------ ###
    for (i in 2:n_years) {
      # Calculate net emissions rate (could be negative with CDR)
      annual_net_emissions <- baseline_annual_emissions[i-1] - qty_mitig[i-1] - qty_remov[i-1]
    
      # Update cumulative emissions
      cumulative_emissions[i] <- cumulative_emissions[i-1] + annual_net_emissions
      
    } ### ---------- END FORWARD SWEEP ------------ ###
    
    # Calculate temperature based on cumulative emissions
    temperature_anomaly <- clim_temp_init + ((cumulative_emissions/1000) * tcre)
    
    ### ---------- BACKWARD SWEEP ------------ ###
    for (i in 1:(n_years-1)) {
      # Calculate backward index (moving from end toward beginning)
      j <- n_years - i
      
      adjoint_derivative <- -(exp_temp_anom * baseline_annual_gwp[j] * econ_dam_pct 
                              * tcre * exp(-disc_rate * (years_rel[j] - years_rel[1]))
                              * (temp_for_calc)^(exp_temp_anom - 1)
      )
      
      # Update adjoint variable (backward Euler step)
      adjoint_var[j] <- adjoint_var[j+1] - adjoint_derivative
        
    } ### ---------- END BACKWARD SWEEP ------------ ###
    
    ### ---------- UPDATE CONTROL VARIABLES ------------ ###
  
    # Update mitigation control based on optimality condition
    # This comes from solving ∂H/∂u_m = 0 for u_m
    new_qty_mitig <- (adjoint_var/(exp_mitig*cost_mitig_unit) * exp(disc_rate*years_rel))^(1/(exp_mitig-1))
    
    # Apply bounds to mitigation (can't mitigate more than baseline emissions)
    new_qty_mitig <- pmin(new_qty_mitig, baseline_annual_emissions)  # Upper bound
    new_qty_mitig <- pmax(new_qty_mitig, 0)
    
    # Update removal control based on optimality condition
    # This comes from solving ∂H/∂u_r = 0 for u_r
    new_qty_remov <- (adjoint_var/(exp_remov*cost_remov_unit) * exp(disc_rate*years_rel))^(1/(exp_remov-1))
    
    # Apply bounds to removal
    new_qty_remov <- pmin(new_qty_remov, removal_max)  # Upper bound (can't be negative)
    new_qty_remov <- pmax(new_qty_remov, removal_min)  # Lower bound (technological limit)
    
    # Use convex combination to smooth updates (helps convergence)
    qty_mitig <- upd_weight * new_qty_mitig + (1 - upd_weight) * previous_qty_mitig
    qty_remov <- upd_weight * new_qty_remov + (1 - upd_weight) * previous_qty_remov
    
    ### ---------- UPDATE COST VECTORS ------------ ###
    
    cost_mitig_cumul <- cumsum((cost_mitig_unit * qty_mitig^exp_mitig) * exp(disc_rate*years_rel))
    cost_remov_cumul <- cumsum((cost_remov_unit * qty_remov^exp_remov) * exp(disc_rate*years_rel))
    cost_resid_cumul <- cumsum((baseline_annual_gwp * econ_dam_pct * ((temperature_anomaly)^(exp_temp_anom)) 
                                * exp(disc_rate*years_rel)))
    cost_total_cumul <- cost_mitig_cumul + cost_remov_cumul + cost_resid_cumul
    
    
    ### ---------- TEST CONVERGENCE ------------ ###
    
    # Calculate absolute differences for key variables
    mitigation_diff <- sum(abs(qty_mitig - previous_qty_mitig), na.rm = TRUE)
    removal_diff <- sum(abs(qty_remov - previous_qty_remov), na.rm = TRUE)
    emissions_diff <- sum(abs(cumulative_emissions - previous_cumulative_emissions), na.rm = TRUE)
    adjoint_diff <- sum(abs(adjoint_var - previous_adjoint_var), na.rm = TRUE)
    
    # Calculate convergence metrics with safeguards against NAs
    sum_qty_mitig <- sum(abs(qty_mitig), na.rm = TRUE)
    sum_qty_remov <- sum(abs(qty_remov), na.rm = TRUE) 
    sum_emissions <- sum(abs(cumulative_emissions), na.rm = TRUE)
    sum_adjoint <- sum(abs(adjoint_var), na.rm = TRUE)
    
    # Avoid division by zero with a small epsilon
    epsilon <- 1e-10
    
    # Calculate convergence metrics (scaled by variable magnitudes)
    # These should be non-negative when converged
    mitigation_conv <- convergence_tolerance * max(sum_qty_mitig, epsilon) - mitigation_diff
    removal_conv <- convergence_tolerance * max(sum_qty_remov, epsilon) - removal_diff
    emissions_conv <- convergence_tolerance * max(sum_emissions, epsilon) - emissions_diff
    adjoint_conv <- convergence_tolerance * max(sum_adjoint, epsilon) - adjoint_diff
    
    # Take minimum of all metrics - all must be positive for convergence
    # Protect against NA with na.rm = TRUE
    min_convergence <- min(mitigation_conv, removal_conv, emissions_conv, adjoint_conv, na.rm = TRUE)
    
    # If all values were NA, min() with na.rm=TRUE returns Inf, so check for that:
    if(is.infinite(min_convergence)) {
      min_convergence <- -1  # Force another iteration unless we hit max_iterations
    }
    
    iteration <- iteration + 1
    
    ### ---------- END ITERATION LOOP ------------ ###
  }
  
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
    
    # Adjoint variable
    adjoint_var = adjoint_var,
    
    # Cost components
    baseline_annual_gwp = baseline_annual_gwp,
    cost_mitig_cumul = cost_mitig_cumul,
    cost_remov_cumul = cost_remov_cumul,
    cost_resid_cumul = cost_resid_cumul,
    cost_total_cumul = cost_total_cumul,
    
    # Additional diagnostic information
    iterations = iteration,
    converged = !is.na(min_convergence) && min_convergence >= 0,
    min_convergence = min_convergence)) 
}



###----- ERROR HANDLING VERSION -----###

#' @title Forward-Backward Sweep for Optimal Control with Error Handling
#' @description
#' Implements the forward-backward sweep method for finding optimal control
#' strategies for temperature overshoot management. The function finds optimal
#' mitigation and carbon dioxide removal paths. Includes robust error handling.
#' 
#' @param parameter_df Data frame containing model parameters
#' @param vector_list List of vectors required for optimal control calculation
#' @param trans_cond Transversality condition value. Default = 0
#' @param log_file Optional log file path for detailed logging
#' @return List of vectors with optimal control solution and all state variables, 
#'         or an error message if calculation failed
#' @examples
#' # Run with standard parameter set
#' result <- forward_backward_sweep(parameter_df, vector_list)
#'

forward_backward_sweep <- function(parameter_df,
                                   vector_list,
                                   trans_cond = 0) {
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
  
  # Check required parameters exist
  required_params <- c("clim_temp_init", "tcre", "cost_mitig_unit", 
                       "cost_remov_unit", "econ_dam_pct", "disc_rate", 
                       "exp_mitig", "exp_remov", "exp_temp_anom")
  
  missing_params <- required_params[!required_params %in% names(parameter_df)]
  if (length(missing_params) > 0) {
    result$error <- TRUE
    result$error_message <- paste("Missing required parameters:", 
                                  paste(missing_params, collapse = ", "))
    return(result)
  }
  
  # Check required vectors exist
  required_vectors <- c("years", "years_rel", "n_years", 
                        "baseline_annual_emissions", "temperature_anomaly",
                        "cumulative_emissions", "qty_mitig", "qty_remov",
                        "adjoint_var", "baseline_annual_gwp", "cost_mitig_cumul",
                        "cost_remov_cumul", "cost_resid_cumul", "cost_total_cumul")
  
  missing_vectors <- required_vectors[!required_vectors %in% names(vector_list)]
  if (length(missing_vectors) > 0) {
    result$error <- TRUE
    result$error_message <- paste("Missing required vectors:", 
                                  paste(missing_vectors, collapse = ", "))
    return(result)
  }
  
  # Wrap the entire calculation in a try-catch to handle unexpected errors
  tryCatch({
    # Algorithm settings
    time_step <- 1
    convergence_tolerance <- 0.001
    min_convergence <- -1
    max_iterations <- 1e4
    iteration <- 0
    upd_weight <- 0.01
    
    # Control variable limits
    mitigation_min <- 0
    removal_min <- 0
    removal_max <- 200  # Could be parameterized based on technical limits
    
    # Extract required variables from parameter_df
    clim_temp_init <- parameter_df$clim_temp_init
    tcre <- parameter_df$tcre
    cost_mitig_unit <- parameter_df$cost_mitig_unit
    cost_remov_unit <- parameter_df$cost_remov_unit
    econ_dam_pct <- parameter_df$econ_dam_pct
    disc_rate <- parameter_df$disc_rate
    exp_mitig <- parameter_df$exp_mitig
    exp_remov <- parameter_df$exp_remov
    exp_temp_anom <- parameter_df$exp_temp_anom
    
    # Extract required vectors from vector_list
    # Time variables
    years <- vector_list$years
    years_rel <- vector_list$years_rel
    n_years <- vector_list$n_years
    
    # State variables
    baseline_annual_emissions <- vector_list$baseline_annual_emissions
    temperature_anomaly <- vector_list$temperature_anomaly
    cumulative_emissions <- vector_list$cumulative_emissions
    
    # Control variables and bounds
    qty_mitig <- vector_list$qty_mitig
    qty_remov <- vector_list$qty_remov
    
    # Adjoint variable
    adjoint_var <- vector_list$adjoint_var
    adjoint_var[n_years] <- trans_cond
    
    # Cost components
    baseline_annual_gwp <- vector_list$baseline_annual_gwp
    cost_mitig_cumul <- vector_list$cost_mitig_cumul
    cost_remov_cumul <- vector_list$cost_remov_cumul
    cost_resid_cumul <- vector_list$cost_resid_cumul
    cost_total_cumul <- vector_list$cost_total_cumul
    
    ### ---------- BEGIN ITERATION LOOP ------------ ###
    
    while((is.na(min_convergence) || min_convergence < 0) && iteration < max_iterations){
      
      # Save control variables, cumulative emissions and adjoint variable for
      # convergence testing in last step
      previous_qty_mitig <- qty_mitig
      previous_qty_remov <- qty_remov
      previous_cumulative_emissions <- cumulative_emissions
      previous_adjoint_var <- adjoint_var
      
      ### ---------- FORWARD SWEEP ------------ ###
      for (i in 2:n_years) {
        # Calculate net emissions rate (could be negative with CDR)
        annual_net_emissions <- baseline_annual_emissions[i-1] - qty_mitig[i-1] - qty_remov[i-1]
        
        # Update cumulative emissions
        cumulative_emissions[i] <- cumulative_emissions[i-1] + annual_net_emissions
        
      } ### ---------- END FORWARD SWEEP ------------ ###
      
      # Calculate temperature based on cumulative emissions
      temperature_anomaly <- clim_temp_init + ((cumulative_emissions/1000) * tcre)
      #temperature_anomaly <- (((cumulative_emissions + 2500)/1000) * tcre)
      
      # Check for invalid temperature values
      if (any(is.na(temperature_anomaly)) || any(is.infinite(temperature_anomaly))) {
        result$error <- TRUE
        result$error_message <- "Invalid temperature values detected"
        return(result)
      }
      
      ### ---------- BACKWARD SWEEP ------------ ###
      for (i in 1:(n_years-1)) {
        # Calculate backward index (moving from end toward beginning)
        j <- n_years - i
        
        # Safeguard against potential numerical issues
        temp_for_calc <- max(temperature_anomaly[j], 1e-10)  # Avoid zero/negative issues
        
        # Calculate adjoint derivative with safety checks
        adjoint_derivative <- -(exp_temp_anom * baseline_annual_gwp[j] * econ_dam_pct 
                                * tcre * exp(-disc_rate * (years_rel[j] - years_rel[1]))
                                * (temp_for_calc)^(exp_temp_anom - 1)
        )
        
        # Check for numerical issues in adjoint derivative
        if (is.na(adjoint_derivative) || is.infinite(adjoint_derivative)) {
          result$error <- TRUE
          result$error_message <- paste("Invalid adjoint derivative at year", years[j])
          return(result)
        }
        
        # Update adjoint variable (backward Euler step)
        adjoint_var[j] <- adjoint_var[j+1] - adjoint_derivative
        
      } ### ---------- END BACKWARD SWEEP ------------ ###
      
      # Check for invalid adjoint values
      if (any(is.na(adjoint_var)) || any(is.infinite(adjoint_var))) {
        result$error <- TRUE
        result$error_message <- "Invalid adjoint values detected"
        return(result)
      }
      
      ### ---------- UPDATE CONTROL VARIABLES ------------ ###
      
      # Create temporary variables for calculations
      # This prevents NaN or Inf from propagating when parameters are problematic
      
      # Safely calculate control updates - prevent division by zero errors
      safe_exp_mitig <- ifelse(exp_mitig == 1, 1.001, exp_mitig)  # Avoid division by zero
      safe_exp_remov <- ifelse(exp_remov == 1, 1.001, exp_remov)  # Avoid division by zero
      
      # Update mitigation control based on optimality condition with safeguards
      # This comes from solving ∂H/∂u_m = 0 for u_m
      temp_for_mitig <- adjoint_var/(safe_exp_mitig*cost_mitig_unit) * exp(disc_rate*years_rel)
      temp_for_mitig <- pmax(temp_for_mitig, 0)  # Ensure values are positive before raising to power
      
      new_qty_mitig <- (temp_for_mitig)^(1/(safe_exp_mitig-1))
      
      # Ensure we don't produce NaN or Inf values
      new_qty_mitig <- ifelse(is.na(new_qty_mitig) | is.infinite(new_qty_mitig), 
                              previous_qty_mitig, new_qty_mitig)
      
      # Apply bounds to mitigation (can't mitigate more than baseline emissions)
      new_qty_mitig <- pmin(new_qty_mitig, baseline_annual_emissions)  # Upper bound
      #new_qty_mitig <- pmin(new_qty_mitig, mitigation_max)  # Upper bound
      new_qty_mitig <- pmax(new_qty_mitig, 0)  # Lower bound
      
      # Update removal control based on optimality condition
      # This comes from solving ∂H/∂u_r = 0 for u_r
      temp_for_remov <- adjoint_var/(safe_exp_remov*cost_remov_unit) * exp(disc_rate*years_rel)
      temp_for_remov <- pmax(temp_for_remov, 0)  # Ensure values are positive before raising to power
      
      new_qty_remov <- (temp_for_remov)^(1/(safe_exp_remov-1))
      
      # Ensure we don't produce NaN or Inf values
      new_qty_remov <- ifelse(is.na(new_qty_remov) | is.infinite(new_qty_remov), 
                              previous_qty_remov, new_qty_remov)
      
      # Apply bounds to removal
      new_qty_remov <- pmin(new_qty_remov, removal_max)  # Upper bound
      new_qty_remov <- pmax(new_qty_remov, removal_min)  # Lower bound
      
      # Use convex combination to smooth updates (helps convergence)
      qty_mitig <- upd_weight * new_qty_mitig + (1 - upd_weight) * previous_qty_mitig
      qty_remov <- upd_weight * new_qty_remov + (1 - upd_weight) * previous_qty_remov
      
      ### ---------- UPDATE COST VECTORS ------------ ###
      
      # Guard against potential numerical issues in cost calculations
      safe_qty_mitig <- pmax(qty_mitig, 0)  # Ensure non-negative values before raising to power
      safe_qty_remov <- pmax(qty_remov, 0)  # Ensure non-negative values before raising to power
      safe_temp <- pmax(temperature_anomaly, 0)  # Ensure non-negative values before raising to power
      
      cost_mitig_cumul <- cumsum(((cost_mitig_unit * safe_qty_mitig^exp_mitig) * 
                                   exp(-disc_rate*years_rel)))
      cost_remov_cumul <- cumsum(((cost_remov_unit * safe_qty_remov^exp_remov) * 
                                   exp(-disc_rate*years_rel)))
      cost_resid_cumul <- cumsum((baseline_annual_gwp * econ_dam_pct * 
                                    ((safe_temp)^(exp_temp_anom)) * 
                                    exp(-disc_rate*years_rel)))
      cost_total_cumul <- cost_mitig_cumul + cost_remov_cumul + cost_resid_cumul
      
      # Check for invalid cost values
      if (any(is.na(cost_total_cumul)) || any(is.infinite(cost_total_cumul))) {
        result$error <- TRUE
        result$error_message <- "Invalid cost values detected"
        return(result)
      }
      
      ### ---------- TEST CONVERGENCE ------------ ###
      
      # Calculate absolute differences for key variables
      mitigation_diff <- sum(abs(qty_mitig - previous_qty_mitig), na.rm = TRUE)
      removal_diff <- sum(abs(qty_remov - previous_qty_remov), na.rm = TRUE)
      emissions_diff <- sum(abs(cumulative_emissions - previous_cumulative_emissions), 
                            na.rm = TRUE)
      adjoint_diff <- sum(abs(adjoint_var - previous_adjoint_var), na.rm = TRUE)
      
      # Calculate convergence metrics with safeguards against NAs
      sum_qty_mitig <- sum(abs(qty_mitig), na.rm = TRUE)
      sum_qty_remov <- sum(abs(qty_remov), na.rm = TRUE) 
      sum_emissions <- sum(abs(cumulative_emissions), na.rm = TRUE)
      sum_adjoint <- sum(abs(adjoint_var), na.rm = TRUE)
      
      # Avoid division by zero with a small epsilon
      epsilon <- 1e-10
      
      # Calculate convergence metrics (scaled by variable magnitudes)
      # These should be non-negative when converged
      mitigation_conv <- convergence_tolerance * max(sum_qty_mitig, epsilon) - mitigation_diff
      removal_conv <- convergence_tolerance * max(sum_qty_remov, epsilon) - removal_diff
      emissions_conv <- convergence_tolerance * max(sum_emissions, epsilon) - emissions_diff
      adjoint_conv <- convergence_tolerance * max(sum_adjoint, epsilon) - adjoint_diff
      
      # Take minimum of all metrics - all must be positive for convergence
      # Protect against NA with na.rm = TRUE
      min_convergence <- min(mitigation_conv, removal_conv, emissions_conv, adjoint_conv, 
                             na.rm = TRUE)
      
      # If all values were NA, min() with na.rm=TRUE returns Inf, so check for that:
      if(is.infinite(min_convergence)) {
        min_convergence <- -1  # Force another iteration unless we hit max_iterations
      }
      
      # Check for divergence (large negative convergence metric)
      # if (!is.na(min_convergence) && min_convergence < -1e6) {
      #   result$error <- TRUE
      #   result$error_message <- "Solution appears to be diverging"
      #   return(result)
      # }
      
      iteration <- iteration + 1
      
      ### ---------- END ITERATION LOOP ------------ ###
    }
    
    # Build the full result structure
    result <- c(result, list(
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
      
      # Adjoint variable
      adjoint_var = adjoint_var,
      
      # Cost components
      baseline_annual_gwp = baseline_annual_gwp,
      cost_mitig_cumul = cost_mitig_cumul,
      cost_remov_cumul = cost_remov_cumul,
      cost_resid_cumul = cost_resid_cumul,
      cost_total_cumul = cost_total_cumul,
      
      # Additional diagnostic information
      iterations = iteration,
      converged = !is.na(min_convergence) && min_convergence >= 0,
      min_convergence = min_convergence))
    
    # Check if we hit max iterations without converging
    if (iteration >= max_iterations && (is.na(min_convergence) || min_convergence < 0)) {
      result$error <- TRUE
      result$error_message <- paste("Failed to converge after", max_iterations, 
                                    "iterations. Best convergence metric:", min_convergence)
    }
    
    return(result)
    
  }, error = function(e) {
    # Catch any other errors that we didn't explicitly handle
    return(list(
      error = TRUE,
      error_message = paste("Calculation error:", e$message)
    ))
  })
}


#' @title Forward-Backward Sweep for Optimal Control with KKT Verification
#' @description
#' Implements the forward-backward sweep method for finding optimal control
#' strategies for temperature overshoot management. The function finds optimal
#' mitigation and carbon dioxide removal paths while ensuring KKT optimality
#' conditions are satisfied.
#' 
#' This implementation includes:
#'   1. Adaptive update weights for faster, more stable convergence
#'   2. Explicit verification of Karush-Kuhn-Tucker (KKT) conditions
#'   3. Monitoring of complementary slackness conditions at control bounds
#'   4. Advanced convergence criteria including KKT violation metrics
#' 
#' The KKT conditions ensure that:
#'   - For interior control values: ∂H/∂u = 0 (stationarity condition)
#'   - For controls at lower bound: ∂H/∂u ≤ 0 (dual feasibility)
#'   - For controls at upper bound: ∂H/∂u ≥ 0 (dual feasibility)
#'   - For all bounds: μ·(bound - control) = 0 (complementary slackness)
#' 
#' @param parameter_df Data frame containing model parameters
#' @param vector_list List of vectors required for optimal control calculation
#' @param trans_cond Transversality condition value (terminal adjoint value). Default = 0
#' @param log_file Optional log file path for detailed logging
#' 
#' @return List of vectors with optimal control solution and all state variables,
#'         including KKT violation metrics for solution quality assessment:
#'         - kkt_violations: Vector of KKT condition violations at each time step
#'         - mean_kkt_violation: Average KKT violation across all time steps
#'         - max_kkt_violation: Maximum KKT violation at any time step
#'         
#' @examples
#' # Run with standard parameter set
#' result <- forward_backward_sweep(parameter_df, vector_list)
#' 
#' # Check solution quality
#' if (result$mean_kkt_violation < 0.001) {
#'   print("Solution satisfies KKT conditions")
#' }
#'
#' @references
#' Karush, W. (1939). Minima of Functions of Several Variables with Inequalities as Side Constraints.
#' Kuhn, H.W. and Tucker, A.W. (1951). Nonlinear programming. Proceedings of 2nd Berkeley Symposium.
#' Lenhart, S. and Workman, J.T. (2007). Optimal Control Applied to Biological Models.
#'
#' @note This implementation addresses scenarios with terminal state constraints
#'       by allowing a non-zero terminal adjoint value via the trans_cond parameter.
#'       For problems with fixed terminal states, this should be used in conjunction
#'       with a shooting method that finds the appropriate trans_cond value.

forward_backward_sweep_kkt <- function(parameter_df,
                                   vector_list,
                                   trans_cond = 0) {
  # Create result structure with an error field
  result <- list(
    error = FALSE,
    error_message = NULL
  )
  
  # Validation checks (keep your existing checks)
  if (is.null(parameter_df) || !is.data.frame(parameter_df)) {
    result$error <- TRUE
    result$error_message <- "Invalid parameter data frame provided"
    return(result)
  }
  
  # Other validation checks remain the same...
  
  # Wrap the entire calculation in a try-catch to handle unexpected errors
  tryCatch({
    # Algorithm settings
    time_step <- 1
    convergence_tolerance <- 0.001
    min_convergence <- -1
    max_iterations <- 1e4
    iteration <- 0
    
    # CHANGE: Increase update weight significantly for better convergence
    upd_weight <- 0.5  # Previously 0.01
    
    # Add adaptive update weight parameters
    min_upd_weight <- 0.2
    max_upd_weight <- 0.8
    upd_weight_adjust <- 0.05
    
    # Control variable limits
    mitigation_min <- 0
    removal_min <- 0
    removal_max <- 200
    
    # Extract required variables from parameter_df (keep existing extractions)
    clim_temp_init <- parameter_df$clim_temp_init
    tcre <- parameter_df$tcre
    cost_mitig_unit <- parameter_df$cost_mitig_unit
    cost_remov_unit <- parameter_df$cost_remov_unit
    econ_dam_pct <- parameter_df$econ_dam_pct
    disc_rate <- parameter_df$disc_rate
    exp_mitig <- parameter_df$exp_mitig
    exp_remov <- parameter_df$exp_remov
    exp_temp_anom <- parameter_df$exp_temp_anom
    
    # Extract required vectors from vector_list (keep existing extractions)
    years <- vector_list$years
    years_rel <- vector_list$years_rel
    n_years <- vector_list$n_years
    
    # State variables
    baseline_annual_emissions <- vector_list$baseline_annual_emissions
    temperature_anomaly <- vector_list$temperature_anomaly
    cumulative_emissions <- vector_list$cumulative_emissions
    
    # Control variables and bounds
    qty_mitig <- vector_list$qty_mitig
    qty_remov <- vector_list$qty_remov
    
    # Adjoint variable
    adjoint_var <- vector_list$adjoint_var
    adjoint_var[n_years] <- trans_cond
    
    # Cost components (keep existing extractions)
    baseline_annual_gwp <- vector_list$baseline_annual_gwp
    cost_mitig_cumul <- vector_list$cost_mitig_cumul
    cost_remov_cumul <- vector_list$cost_remov_cumul
    cost_resid_cumul <- vector_list$cost_resid_cumul
    cost_total_cumul <- vector_list$cost_total_cumul
    
    # Add variables to track KKT violations
    kkt_violations <- rep(Inf, n_years)
    
    ### ---------- BEGIN ITERATION LOOP ------------ ###
    
    while((is.na(min_convergence) || min_convergence < 0) && iteration < max_iterations){
      
      # Save control variables, cumulative emissions and adjoint variable for
      # convergence testing in last step
      previous_qty_mitig <- qty_mitig
      previous_qty_remov <- qty_remov
      previous_cumulative_emissions <- cumulative_emissions
      previous_adjoint_var <- adjoint_var
      
      ### ---------- FORWARD SWEEP ------------ ###
      for (i in 2:n_years) {
        # Calculate net emissions rate (could be negative with CDR)
        annual_net_emissions <- baseline_annual_emissions[i-1] - qty_mitig[i-1] - qty_remov[i-1]
        
        # Update cumulative emissions
        cumulative_emissions[i] <- cumulative_emissions[i-1] + annual_net_emissions
        
      } ### ---------- END FORWARD SWEEP ------------ ###
      
      # Calculate temperature based on cumulative emissions
      temperature_anomaly <- clim_temp_init + ((cumulative_emissions/1000) * tcre)
      
      # Check for invalid temperature values
      if (any(is.na(temperature_anomaly)) || any(is.infinite(temperature_anomaly))) {
        result$error <- TRUE
        result$error_message <- "Invalid temperature values detected"
        return(result)
      }
      
      ### ---------- BACKWARD SWEEP ------------ ###
      for (i in 1:(n_years-1)) {
        # Calculate backward index (moving from end toward beginning)
        j <- n_years - i
        
        # Safeguard against potential numerical issues
        temp_for_calc <- max(temperature_anomaly[j], 1e-10)
        
        # Calculate adjoint derivative with safety checks
        adjoint_derivative <- -(exp_temp_anom * baseline_annual_gwp[j] * econ_dam_pct 
                                * tcre * exp(-disc_rate * (years_rel[j] - years_rel[1]))
                                * (temp_for_calc)^(exp_temp_anom - 1)
        )
        
        # Check for numerical issues in adjoint derivative
        if (is.na(adjoint_derivative) || is.infinite(adjoint_derivative)) {
          result$error <- TRUE
          result$error_message <- paste("Invalid adjoint derivative at year", years[j])
          return(result)
        }
        
        # Update adjoint variable (backward Euler step)
        adjoint_var[j] <- adjoint_var[j+1] - adjoint_derivative
        
      } ### ---------- END BACKWARD SWEEP ------------ ###
      
      # Check for invalid adjoint values
      if (any(is.na(adjoint_var)) || any(is.infinite(adjoint_var))) {
        result$error <- TRUE
        result$error_message <- "Invalid adjoint values detected"
        return(result)
      }
      
      ### ---------- UPDATE CONTROL VARIABLES ------------ ###
      
      # Create temporary variables for calculations (keep your existing safeguards)
      safe_exp_mitig <- ifelse(exp_mitig == 1, 1.001, exp_mitig)
      safe_exp_remov <- ifelse(exp_remov == 1, 1.001, exp_remov)
      
      # NEW: Calculate optimal control values directly from KKT conditions
      
      # Update mitigation control based on optimality condition with safeguards
      temp_for_mitig <- adjoint_var/(safe_exp_mitig*cost_mitig_unit) * exp(disc_rate*years_rel)
      temp_for_mitig <- pmax(temp_for_mitig, 0)
      
      new_qty_mitig <- (temp_for_mitig)^(1/(safe_exp_mitig-1))
      
      # Ensure we don't produce NaN or Inf values
      new_qty_mitig <- ifelse(is.na(new_qty_mitig) | is.infinite(new_qty_mitig), 
                              previous_qty_mitig, new_qty_mitig)
      
      # Apply bounds to mitigation (can't mitigate more than baseline emissions)
      new_qty_mitig <- pmin(new_qty_mitig, baseline_annual_emissions)
      new_qty_mitig <- pmax(new_qty_mitig, 0)
      
      # Update removal control based on optimality condition
      temp_for_remov <- adjoint_var/(safe_exp_remov*cost_remov_unit) * exp(disc_rate*years_rel)
      temp_for_remov <- pmax(temp_for_remov, 0)
      
      new_qty_remov <- (temp_for_remov)^(1/(safe_exp_remov-1))
      
      # Ensure we don't produce NaN or Inf values
      new_qty_remov <- ifelse(is.na(new_qty_remov) | is.infinite(new_qty_remov), 
                              previous_qty_remov, new_qty_remov)
      
      # Apply bounds to removal
      new_qty_remov <- pmin(new_qty_remov, removal_max)
      new_qty_remov <- pmax(new_qty_remov, removal_min)
      
      # CHANGE: Implement adaptive update weight
      # If we're making good progress, increase update weight
      if (iteration > 1) {
        last_diff_mitig <- sum(abs(previous_qty_mitig - qty_mitig))
        last_diff_remov <- sum(abs(previous_qty_remov - qty_remov))
        
        new_diff_mitig <- sum(abs(new_qty_mitig - qty_mitig))
        new_diff_remov <- sum(abs(new_qty_remov - qty_remov))
        
        # If changes are getting smaller, increase update weight for faster convergence
        if ((new_diff_mitig < last_diff_mitig) && (new_diff_remov < last_diff_remov)) {
          upd_weight <- min(max_upd_weight, upd_weight + upd_weight_adjust)
        } else {
          # If changes are getting larger, decrease update weight for stability
          upd_weight <- max(min_upd_weight, upd_weight - upd_weight_adjust)
        }
      }
      
      # Use convex combination to smooth updates (helps convergence)
      qty_mitig <- upd_weight * new_qty_mitig + (1 - upd_weight) * previous_qty_mitig
      qty_remov <- upd_weight * new_qty_remov + (1 - upd_weight) * previous_qty_remov
      
      ### ---------- UPDATE COST VECTORS ------------ ###
      
      # Guard against potential numerical issues in cost calculations
      safe_qty_mitig <- pmax(qty_mitig, 0)
      safe_qty_remov <- pmax(qty_remov, 0)
      safe_temp <- pmax(temperature_anomaly, 0)
      
      cost_mitig_cumul <- cumsum(((cost_mitig_unit * safe_qty_mitig^exp_mitig) * 
                                    exp(-disc_rate*years_rel)))
      cost_remov_cumul <- cumsum(((cost_remov_unit * safe_qty_remov^exp_remov) * 
                                    exp(-disc_rate*years_rel)))
      cost_resid_cumul <- cumsum((baseline_annual_gwp * econ_dam_pct * 
                                    ((safe_temp)^(exp_temp_anom)) * 
                                    exp(-disc_rate*years_rel)))
      cost_total_cumul <- cost_mitig_cumul + cost_remov_cumul + cost_resid_cumul
      
      # Check for invalid cost values
      if (any(is.na(cost_total_cumul)) || any(is.infinite(cost_total_cumul))) {
        result$error <- TRUE
        result$error_message <- "Invalid cost values detected"
        return(result)
      }
      
      # NEW: Calculate KKT condition violations
      # For interior points, the stationarity condition should be satisfied
      # For points at bounds, check complementary slackness
      
      # Mitigation KKT violations
      mitig_kkt_violations <- rep(0, n_years)
      for (i in 1:n_years) {
        stationarity_mitig <- exp_mitig * cost_mitig_unit * qty_mitig[i]^(exp_mitig-1) * 
          exp(-disc_rate*years_rel[i]) - adjoint_var[i]
        
        if (abs(qty_mitig[i]) < 1e-10) {
          # At lower bound - dual feasibility check
          mitig_kkt_violations[i] <- min(0, stationarity_mitig)
        } else if (abs(qty_mitig[i] - baseline_annual_emissions[i]) < 1e-10) {
          # At upper bound - dual feasibility check
          mitig_kkt_violations[i] <- max(0, stationarity_mitig)
        } else {
          # Interior point - stationarity check
          mitig_kkt_violations[i] <- abs(stationarity_mitig)
        }
      }
      
      # Removal KKT violations
      remov_kkt_violations <- rep(0, n_years)
      for (i in 1:n_years) {
        stationarity_remov <- exp_remov * cost_remov_unit * qty_remov[i]^(exp_remov-1) * 
          exp(-disc_rate*years_rel[i]) - adjoint_var[i]
        
        if (abs(qty_remov[i]) < 1e-10) {
          # At lower bound - dual feasibility check
          remov_kkt_violations[i] <- min(0, stationarity_remov)
        } else if (abs(qty_remov[i] - removal_max) < 1e-10) {
          # At upper bound - dual feasibility check
          remov_kkt_violations[i] <- max(0, stationarity_remov)
        } else {
          # Interior point - stationarity check
          remov_kkt_violations[i] <- abs(stationarity_remov)
        }
      }
      
      # Combined KKT violations
      kkt_violations <- pmax(mitig_kkt_violations, remov_kkt_violations)
      
      ### ---------- TEST CONVERGENCE ------------ ###
      
      # Keep your existing convergence checks, but add KKT violation check
      mitigation_diff <- sum(abs(qty_mitig - previous_qty_mitig), na.rm = TRUE)
      removal_diff <- sum(abs(qty_remov - previous_qty_remov), na.rm = TRUE)
      emissions_diff <- sum(abs(cumulative_emissions - previous_cumulative_emissions), 
                            na.rm = TRUE)
      adjoint_diff <- sum(abs(adjoint_var - previous_adjoint_var), na.rm = TRUE)
      
      # Calculate convergence metrics with safeguards against NAs
      sum_qty_mitig <- sum(abs(qty_mitig), na.rm = TRUE)
      sum_qty_remov <- sum(abs(qty_remov), na.rm = TRUE) 
      sum_emissions <- sum(abs(cumulative_emissions), na.rm = TRUE)
      sum_adjoint <- sum(abs(adjoint_var), na.rm = TRUE)
      
      # Avoid division by zero with a small epsilon
      epsilon <- 1e-10
      
      # Calculate convergence metrics (scaled by variable magnitudes)
      mitigation_conv <- convergence_tolerance * max(sum_qty_mitig, epsilon) - mitigation_diff
      removal_conv <- convergence_tolerance * max(sum_qty_remov, epsilon) - removal_diff
      emissions_conv <- convergence_tolerance * max(sum_emissions, epsilon) - emissions_diff
      adjoint_conv <- convergence_tolerance * max(sum_adjoint, epsilon) - adjoint_diff
      
      # NEW: Add KKT violation convergence check
      kkt_conv <- convergence_tolerance - mean(kkt_violations)
      
      # Take minimum of all metrics - all must be positive for convergence
      min_convergence <- min(mitigation_conv, removal_conv, emissions_conv, adjoint_conv, 
                             kkt_conv, na.rm = TRUE)
      
      # If all values were NA, min() with na.rm=TRUE returns Inf, so check for that:
      if(is.infinite(min_convergence)) {
        min_convergence <- -1
      }
      
      iteration <- iteration + 1
      
    } ### ---------- END ITERATION LOOP ------------ ###
    
    # Build the full result structure
    result <- c(result, list(
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
      
      # Adjoint variable
      adjoint_var = adjoint_var,
      
      # Cost components
      baseline_annual_gwp = baseline_annual_gwp,
      cost_mitig_cumul = cost_mitig_cumul,
      cost_remov_cumul = cost_remov_cumul,
      cost_resid_cumul = cost_resid_cumul,
      cost_total_cumul = cost_total_cumul,
      
      # NEW: Add KKT violations to output
      kkt_violations = kkt_violations,
      mean_kkt_violation = mean(kkt_violations),
      max_kkt_violation = max(kkt_violations),
      
      # Additional diagnostic information
      iterations = iteration,
      converged = !is.na(min_convergence) && min_convergence >= 0,
      min_convergence = min_convergence,
      final_upd_weight = upd_weight))
    
    # Check if we hit max iterations without converging
    if (iteration >= max_iterations && (is.na(min_convergence) || min_convergence < 0)) {
      result$error <- TRUE
      result$error_message <- paste("Failed to converge after", max_iterations, 
                                    "iterations. Best convergence metric:", min_convergence)
    }
    
    return(result)
    
  }, error = function(e) {
    # Catch any other errors that we didn't explicitly handle
    return(list(
      error = TRUE,
      error_message = paste("Calculation error:", e$message)
    ))
  })
}
  