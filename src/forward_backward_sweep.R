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

# Load required packages
library(dplyr)    # For data manipulation
library(tidyr)    # For reshaping data
library(here)     # For file path management


#' @title Forward-Backward Sweep for Optimal Control
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

forward_backward_sweep <- function(parameter_df,
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
    temperature_anomaly <- clim_temp_init + (tcre * cumulative_emissions)
    
    ### ---------- BACKWARD SWEEP ------------ ###
    for (i in 1:(n_years-1)) {
      # Calculate backward index (moving from end toward beginning)
      j <- n_years - i
      
      adjoint_derivative <- -(exp_temp_anom * baseline_annual_gwp[j] * econ_dam_pct 
                              * tcre * exp(-disc_rate * (years_rel[j] - years_rel[1]))
                              * (temperature_anomaly[j])^(exp_temp_anom - 1)
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

  