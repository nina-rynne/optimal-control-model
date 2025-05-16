#' @title Non-Linear Programming Solution for Climate Optimal Control (Fixed Version)
#' @description
#' Implements non-linear programming to solve the climate optimal control problem
#' with proper dynamics and result handling.
#' 
#' @param parameter_df Data frame containing model parameters
#' @param emissions_df Data frame containing emissions scenario data
#' @param economic_df Data frame containing economic data
#' @param scenario Scenario string identifier
#' @param max_iterations Maximum number of iterations (default: 500)
#' @param verbose Logical; whether to print progress (default: TRUE)
#' 
#' @return List containing optimal control paths and all state variables

climate_nonlinear_programming <- function(parameter_df, 
                                          emissions_df, 
                                          economic_df, 
                                          scenario,
                                          max_iterations = 500, 
                                          verbose = TRUE) {
  # Load required packages
  library(dplyr)
  library(tidyr)
  library(nloptr)  # For non-linear optimization
  
  # Initialize result variables at the global function level
  # This ensures they're accessible even if errors occur
  optimal_emissions <- NULL
  optimal_temperature <- NULL
  optimal_mitig <- NULL
  optimal_remov <- NULL
  optimal_cost_mitig_cumul <- NULL
  optimal_cost_remov_cumul <- NULL
  optimal_cost_resid_cumul <- NULL
  optimal_cost_total_cumul <- NULL
  annual_net_emissions <- NULL
  converged <- FALSE
  error_message <- NULL
  
  # Set default error status
  has_error <- FALSE
  
  # Error handling
  tryCatch({
    # Step 1: Create necessary vectors using your existing function
    vector_list <- create_vector_list(
      parameter_df = parameter_df,
      emissions_df = emissions_df,
      economic_df = economic_df,
      scenario = scenario
    )
    
    # Extract key variables and parameters
    years <- vector_list$years
    years_rel <- vector_list$years_rel
    n_years <- vector_list$n_years
    baseline_annual_emissions <- vector_list$baseline_annual_emissions
    baseline_annual_gwp <- vector_list$baseline_annual_gwp
    initial_cumulative_emissions <- vector_list$cumulative_emissions[1]
    
    # Extract cost parameters
    clim_temp_init <- parameter_df$clim_temp_init
    tcre <- parameter_df$tcre
    cost_mitig_unit <- parameter_df$cost_mitig_unit
    cost_remov_unit <- parameter_df$cost_remov_unit
    econ_dam_pct <- parameter_df$econ_dam_pct
    disc_rate <- parameter_df$disc_rate
    exp_mitig <- parameter_df$exp_mitig
    exp_remov <- parameter_df$exp_remov
    exp_temp_anom <- parameter_df$exp_temp_anom
    
    # Terminal state target (if applicable)
    co2_target_2100 <- ifelse(!is.null(parameter_df$co2_target_2100), 
                              parameter_df$co2_target_2100, 
                              Inf)  # Use Inf if no target specified
    
    if (verbose) {
      cat("Setting up NLP problem...\n")
      cat("Initial cumulative emissions:", initial_cumulative_emissions, "GtC\n")
      cat("Initial temperature anomaly:", clim_temp_init, "°C\n")
      cat("Baseline emissions range:", min(baseline_annual_emissions), "to", 
          max(baseline_annual_emissions), "GtC/yr\n")
      cat("Total simulation years:", n_years, "\n")
    }
    
    # Number of control points (much smaller than n_years)
    n_control_points <- min(20, ceiling(n_years/5))
    
    # Create mapping from control points to full time series
    control_years <- seq(1, n_years, length.out = n_control_points)
    
    # Function to expand control points to full time series
    expand_controls <- function(control_values) {
      # Linear interpolation
      approx(control_years, control_values, 1:n_years, rule = 2)$y
    }
    
    # Step 2: Define the objective function for optimization
    objective_function <- function(x) {
      # Split and expand controls
      mitig_control_pts <- x[1:n_control_points]
      remov_control_pts <- x[(n_control_points+1):(2*n_control_points)]
      
      # Expand to full time series
      mitig_quantities <- expand_controls(mitig_control_pts)
      remov_quantities <- expand_controls(remov_control_pts)
      
      # Cap mitigation at baseline emissions
      mitig_quantities <- pmin(mitig_quantities, baseline_annual_emissions)
      
      # Initialize state trajectories
      cumul_emissions <- numeric(n_years)
      temp_anomaly <- numeric(n_years)
      
      # Set initial state with correct values
      cumul_emissions[1] <- initial_cumulative_emissions
      temp_anomaly[1] <- clim_temp_init + ((cumul_emissions[1]/1000) * tcre)
      
      # Calculate state trajectory and costs
      total_cost <- 0
      
      for (t in 1:n_years) {
        # Calculate costs for current period
        mitig_cost <- cost_mitig_unit * (mitig_quantities[t]^exp_mitig) * 
          exp(-disc_rate * years_rel[t])
        remov_cost <- cost_remov_unit * (remov_quantities[t]^exp_remov) * 
          exp(-disc_rate * years_rel[t])
        damage_cost <- baseline_annual_gwp[t] * econ_dam_pct * 
          (temp_anomaly[t]^exp_temp_anom) * exp(-disc_rate * years_rel[t])
        
        # Add to total cost
        total_cost <- total_cost + mitig_cost + remov_cost + damage_cost
        
        # Update state for next period (if not the last period)
        if (t < n_years) {
          # Calculate net emissions - ensure it actually updates based on baseline emissions
          net_emissions <- baseline_annual_emissions[t] - mitig_quantities[t] - remov_quantities[t]
          # Update cumulative emissions
          cumul_emissions[t+1] <- cumul_emissions[t] + net_emissions
          # Update temperature
          temp_anomaly[t+1] <- clim_temp_init + ((cumul_emissions[t+1]/1000) * tcre)
        }
      }
      
      # Add terminal penalty if target exists
      if (is.finite(co2_target_2100)) {
        terminal_penalty <- 1e6 * (cumul_emissions[n_years] - co2_target_2100)^2
        total_cost <- total_cost + terminal_penalty
      }
      
      return(total_cost)
    }
    
    # Step 4: Set up initial guess and bounds
    if (verbose) cat("Setting up optimization parameters...\n")
    
    # Initial guess: start with meaningful mitigation and removal trajectories
    initial_mitig <- baseline_annual_emissions[1] * 
      seq(0.2, 0.7, length.out = n_control_points)
    initial_remov <- baseline_annual_emissions[1] * 
      seq(0.0, 0.3, length.out = n_control_points)
    initial_guess <- c(initial_mitig, initial_remov)
    
    # Bounds for variables
    # Lower bounds: all zeros
    lb <- rep(0, 2 * n_control_points)
    
    # Upper bounds: max baseline emissions for mitigation, some max for removal
    max_cdr <- 50  # Maximum technical potential for CDR
    ub_mitig <- rep(max(baseline_annual_emissions) * 1.2, n_control_points)
    ub_remov <- rep(max_cdr, n_control_points)
    ub <- c(ub_mitig, ub_remov)
    
    # Step 5: Run the optimization
    if (verbose) cat("Starting NLP optimization with", n_control_points, "control points...\n")
    
    # Use a more efficient algorithm for this problem
    opts <- list(
      "algorithm" = "NLOPT_LN_BOBYQA",  # Derivative-free algorithm for bound-constrained optimization
      "xtol_rel" = 1.0e-4,              # Relaxed tolerance
      "ftol_rel" = 1.0e-4,              # Relaxed tolerance 
      "maxeval" = max_iterations,
      "print_level" = ifelse(verbose, 1, 0)
    )
    
    # Run optimization
    optimization_result <- nloptr(
      x0 = initial_guess,
      eval_f = objective_function,
      lb = lb,
      ub = ub,
      opts = opts
    )
    
    # Check convergence
    converged <- optimization_result$status > 0
    if (verbose) {
      if (converged) {
        cat("NLP optimization converged successfully.\n")
      } else {
        cat("Warning: NLP optimization did not converge. Results may be suboptimal.\n")
      }
      cat("Objective function value:", optimization_result$objective, "\n")
      cat("Number of iterations:", optimization_result$iterations, "\n")
    }
    
    # Step 6: Extract solution and simulate trajectory
    if (verbose) cat("Extracting solution and simulating trajectory...\n")
    
    # Extract optimal control points
    mitig_control_pts <- optimization_result$solution[1:n_control_points]
    remov_control_pts <- optimization_result$solution[(n_control_points+1):(2*n_control_points)]
    
    # Expand to full time series
    optimal_mitig <- expand_controls(mitig_control_pts)
    optimal_remov <- expand_controls(remov_control_pts)
    
    # Cap mitigation at baseline emissions
    optimal_mitig <- pmin(optimal_mitig, baseline_annual_emissions)
    
    # Simulate trajectory with optimal controls
    optimal_emissions <- numeric(n_years)
    optimal_temperature <- numeric(n_years)
    optimal_cost_mitig <- numeric(n_years)
    optimal_cost_remov <- numeric(n_years)
    optimal_cost_resid <- numeric(n_years)
    optimal_cost_total <- numeric(n_years)
    annual_net_emissions <- numeric(n_years)
    
    # Initialize with starting values
    optimal_emissions[1] <- initial_cumulative_emissions  # Use the actual initial value
    optimal_temperature[1] <- clim_temp_init + ((optimal_emissions[1]/1000) * tcre)
    
    # Simulate forward with full dynamics
    for (t in 1:n_years) {
      # Calculate costs
      optimal_cost_mitig[t] <- cost_mitig_unit * (optimal_mitig[t]^exp_mitig) * 
        exp(-disc_rate * years_rel[t])
      optimal_cost_remov[t] <- cost_remov_unit * (optimal_remov[t]^exp_remov) * 
        exp(-disc_rate * years_rel[t])
      optimal_cost_resid[t] <- baseline_annual_gwp[t] * econ_dam_pct * 
        (optimal_temperature[t]^exp_temp_anom) * exp(-disc_rate * years_rel[t])
      optimal_cost_total[t] <- optimal_cost_mitig[t] + optimal_cost_remov[t] + optimal_cost_resid[t]
      
      # Calculate net emissions for the current period 
      annual_net_emissions[t] <- baseline_annual_emissions[t] - optimal_mitig[t] - optimal_remov[t]
      
      # Update state for next period
      if (t < n_years) {
        # Ensure cumulative emissions are properly updated with net emissions
        optimal_emissions[t+1] <- optimal_emissions[t] + annual_net_emissions[t]
        optimal_temperature[t+1] <- clim_temp_init + ((optimal_emissions[t+1]/1000) * tcre)
      }
    }
    
    # Calculate cumulative costs
    optimal_cost_mitig_cumul <- cumsum(optimal_cost_mitig)
    optimal_cost_remov_cumul <- cumsum(optimal_cost_remov)
    optimal_cost_resid_cumul <- cumsum(optimal_cost_resid)
    optimal_cost_total_cumul <- cumsum(optimal_cost_total)
    
    # Print a summary of the optimization
    if (verbose) {
      cat("\nOptimization summary:\n")
      cat("  Initial cumulative emissions:", optimal_emissions[1], "GtC\n")
      cat("  Final cumulative emissions:  ", optimal_emissions[n_years], "GtC\n")
      cat("  Change in emissions:         ", optimal_emissions[n_years] - optimal_emissions[1], "GtC\n")
      cat("  Initial temperature anomaly: ", optimal_temperature[1], "°C\n")
      cat("  Final temperature anomaly:   ", optimal_temperature[n_years], "°C\n")
      cat("  Change in temperature:       ", optimal_temperature[n_years] - optimal_temperature[1], "°C\n")
      cat("  Average annual mitigation:   ", mean(optimal_mitig), "GtC/yr\n")
      cat("  Average annual removal:      ", mean(optimal_remov), "GtC/yr\n")
      cat("  Total optimal cost:          ", tail(optimal_cost_total_cumul, 1), "$ Trillion\n")
    }
    
    # Verify that the simulation actually changed over time
    if (all(diff(optimal_emissions) == 0)) {
      warning("The cumulative emissions did not change over time. This suggests a problem with the dynamics.")
    }
    
  }, error = function(e) {
    # If an error occurs, capture it and continue
    has_error <- TRUE
    error_message <- e$message
    message("Error in NLP optimization: ", error_message)
  })
  
  # Step 7: Build result list outside the tryCatch to ensure we always return something
  # This is the key change - we're moving this step outside the error handling
  
  # Build dummy data for error cases
  if (has_error || is.null(optimal_emissions)) {
    years <- vector_list$years
    years_rel <- vector_list$years_rel
    n_years <- vector_list$n_years
    initial_cumulative_emissions <- vector_list$cumulative_emissions[1]
    baseline_annual_emissions <- vector_list$baseline_annual_emissions
    baseline_annual_gwp <- vector_list$baseline_annual_gwp
    clim_temp_init <- parameter_df$clim_temp_init
    tcre <- parameter_df$tcre
    
    # Create a dummy trajectory that changes over time
    optimal_emissions <- numeric(n_years)
    optimal_emissions[1] <- initial_cumulative_emissions
    for (t in 2:n_years) {
      optimal_emissions[t] <- optimal_emissions[t-1] + baseline_annual_emissions[t-1]
    }
    optimal_temperature <- clim_temp_init + ((optimal_emissions/1000) * tcre)
    
    # Set control variables to zero
    optimal_mitig <- rep(0, n_years)
    optimal_remov <- rep(0, n_years)
    annual_net_emissions <- baseline_annual_emissions
    
    # Set cost variables to zero
    optimal_cost_mitig_cumul <- rep(0, n_years)
    optimal_cost_remov_cumul <- rep(0, n_years)
    optimal_cost_resid_cumul <- rep(0, n_years)
    optimal_cost_total_cumul <- rep(0, n_years)
    
    if (is.null(error_message)) {
      error_message <- "Optimization failed for unknown reasons"
    }
  }
  
  # Now build the result with all variables, whether from successful optimization or error handling
  result <- list(
    # Time variables
    years = vector_list$years,
    years_rel = vector_list$years_rel,
    n_years = vector_list$n_years,
    
    # State variables
    baseline_annual_emissions = vector_list$baseline_annual_emissions,
    temperature_anomaly = optimal_temperature,
    cumulative_emissions = optimal_emissions,
    
    # Control variables
    qty_mitig = optimal_mitig,
    qty_remov = optimal_remov,
    
    # Adjoint variable (shadow price)
    adjoint_var = rep(NA, vector_list$n_years),
    
    # Additional state information
    annual_net_emissions = annual_net_emissions,
    
    # Cost components
    baseline_annual_gwp = vector_list$baseline_annual_gwp,
    cost_mitig_cumul = optimal_cost_mitig_cumul,
    cost_remov_cumul = optimal_cost_remov_cumul,
    cost_resid_cumul = optimal_cost_resid_cumul,
    cost_total_cumul = optimal_cost_total_cumul,
    
    # Additional diagnostic information
    nlp_objective_value = if(exists("optimization_result")) optimization_result$objective else NA,
    nlp_iterations = if(exists("optimization_result")) optimization_result$iterations else NA,
    nlp_status = if(exists("optimization_result")) optimization_result$status else NA,
    nlp_message = if(exists("optimization_result")) optimization_result$message else NA,
    nlp_control_points = if(exists("n_control_points")) n_control_points else NA,
    converged = converged,
    error = has_error,
    error_message = error_message
  )
  
  if (verbose) cat("Non-linear programming solution complete.\n")
  
  return(result)
}


#' @title SLSQP Non-Linear Programming Solution for Climate Optimal Control
#' @description
#' Implements non-linear programming using SLSQP algorithm to solve the climate
#' optimal control problem. This approach is similar to MATLAB's fmincon.
#' 
#' @param parameter_df Data frame containing model parameters
#' @param emissions_df Data frame containing emissions scenario data
#' @param economic_df Data frame containing economic data
#' @param scenario Scenario string identifier
#' @param max_iterations Maximum number of iterations (default: 500)
#' @param verbose Logical; whether to print progress (default: TRUE)
#' 
#' @return List containing optimal control paths and all state variables

climate_slsqp_programming <- function(parameter_df, 
                                      emissions_df, 
                                      economic_df, 
                                      scenario,
                                      max_iterations = 500, 
                                      verbose = TRUE) {
  # Load required packages
  library(dplyr)
  library(tidyr)
  library(nloptr)  # For non-linear optimization
  
  # Initialize result variables at the global function level
  # This ensures they're accessible even if errors occur
  optimal_emissions <- NULL
  optimal_temperature <- NULL
  optimal_mitig <- NULL
  optimal_remov <- NULL
  optimal_cost_mitig_cumul <- NULL
  optimal_cost_remov_cumul <- NULL
  optimal_cost_resid_cumul <- NULL
  optimal_cost_total_cumul <- NULL
  annual_net_emissions <- NULL
  converged <- FALSE
  error_message <- NULL
  
  # Set default error status
  has_error <- FALSE
  
  # Error handling
  tryCatch({
    # Step 1: Create necessary vectors using your existing function
    vector_list <- create_vector_list(
      parameter_df = parameter_df,
      emissions_df = emissions_df,
      economic_df = economic_df,
      scenario = scenario
    )
    
    # Extract key variables and parameters
    years <- vector_list$years
    years_rel <- vector_list$years_rel
    n_years <- vector_list$n_years
    baseline_annual_emissions <- vector_list$baseline_annual_emissions
    baseline_annual_gwp <- vector_list$baseline_annual_gwp
    initial_cumulative_emissions <- vector_list$cumulative_emissions[1]
    
    # Extract cost parameters
    clim_temp_init <- parameter_df$clim_temp_init
    tcre <- parameter_df$tcre
    cost_mitig_unit <- parameter_df$cost_mitig_unit
    cost_remov_unit <- parameter_df$cost_remov_unit
    econ_dam_pct <- parameter_df$econ_dam_pct
    disc_rate <- parameter_df$disc_rate
    exp_mitig <- parameter_df$exp_mitig
    exp_remov <- parameter_df$exp_remov
    exp_temp_anom <- parameter_df$exp_temp_anom
    
    # Terminal state target (if applicable)
    co2_target_2100 <- ifelse(!is.null(parameter_df$co2_target_2100), 
                              parameter_df$co2_target_2100, 
                              Inf)  # Use Inf if no target specified
    
    if (verbose) {
      cat("Setting up SLSQP problem...\n")
      cat("Initial cumulative emissions:", initial_cumulative_emissions, "GtC\n")
      cat("Initial temperature anomaly:", clim_temp_init, "°C\n")
      cat("Baseline emissions range:", min(baseline_annual_emissions), "to", 
          max(baseline_annual_emissions), "GtC/yr\n")
      cat("Total simulation years:", n_years, "\n")
    }
    
    # Number of control points (much smaller than n_years)
    n_control_points <- min(20, ceiling(n_years/5))
    
    # Create mapping from control points to full time series
    control_years <- seq(1, n_years, length.out = n_control_points)
    
    # Function to expand control points to full time series
    expand_controls <- function(control_values) {
      # Linear interpolation
      approx(control_years, control_values, 1:n_years, rule = 2)$y
    }
    
    # Step 2: Define the objective function for optimization
    objective_function <- function(x) {
      # Split and expand controls
      mitig_control_pts <- x[1:n_control_points]
      remov_control_pts <- x[(n_control_points+1):(2*n_control_points)]
      
      # Expand to full time series
      mitig_quantities <- expand_controls(mitig_control_pts)
      remov_quantities <- expand_controls(remov_control_pts)
      
      # Cap mitigation at baseline emissions
      mitig_quantities <- pmin(mitig_quantities, baseline_annual_emissions)
      
      # Initialize state trajectories
      cumul_emissions <- numeric(n_years)
      temp_anomaly <- numeric(n_years)
      
      # Set initial state with correct values
      cumul_emissions[1] <- initial_cumulative_emissions
      temp_anomaly[1] <- clim_temp_init + ((cumul_emissions[1]/1000) * tcre)
      
      # Calculate state trajectory and costs
      total_cost <- 0
      
      for (t in 1:n_years) {
        # Calculate costs for current period
        mitig_cost <- cost_mitig_unit * (mitig_quantities[t]^exp_mitig) * 
          exp(-disc_rate * years_rel[t])
        remov_cost <- cost_remov_unit * (remov_quantities[t]^exp_remov) * 
          exp(-disc_rate * years_rel[t])
        damage_cost <- baseline_annual_gwp[t] * econ_dam_pct * 
          (temp_anomaly[t]^exp_temp_anom) * exp(-disc_rate * years_rel[t])
        
        # Add to total cost
        total_cost <- total_cost + mitig_cost + remov_cost + damage_cost
        
        # Update state for next period (if not the last period)
        if (t < n_years) {
          # Calculate net emissions - ensure it actually updates based on baseline emissions
          net_emissions <- baseline_annual_emissions[t] - mitig_quantities[t] - remov_quantities[t]
          # Update cumulative emissions
          cumul_emissions[t+1] <- cumul_emissions[t] + net_emissions
          # Update temperature
          temp_anomaly[t+1] <- clim_temp_init + ((cumul_emissions[t+1]/1000) * tcre)
        }
      }
      
      # Add terminal penalty if target exists
      if (is.finite(co2_target_2100)) {
        terminal_penalty <- 1e6 * (cumul_emissions[n_years] - co2_target_2100)^2
        total_cost <- total_cost + terminal_penalty
      }
      
      return(total_cost)
    }
    
    # Step 3: Define the gradient of the objective function (for SLSQP)
    # SLSQP works best with analytical gradients, but we'll use numerical approximation
    # This is one key difference from the BOBYQA method which didn't need gradients
    gradient_function <- function(x) {
      # Use finite differences to estimate gradient
      delta <- 1e-6  # Small delta for finite difference
      grad <- numeric(length(x))
      f0 <- objective_function(x)
      
      for (i in 1:length(x)) {
        x_perturbed <- x
        x_perturbed[i] <- x_perturbed[i] + delta
        f1 <- objective_function(x_perturbed)
        grad[i] <- (f1 - f0) / delta
      }
      
      return(grad)
    }
    
    # Step 4: Define constraint functions
    # Unlike BOBYQA, SLSQP can handle nonlinear inequality and equality constraints
    # We'll define an inequality constraint that mitigation <= baseline_emissions
    
    # This inequality constraint function should return values >= 0 when constraints are satisfied
    constraint_function <- function(x) {
      # Split and expand controls
      mitig_control_pts <- x[1:n_control_points]
      
      # Expand to full time series
      mitig_quantities <- expand_controls(mitig_control_pts)
      
      # For each time period, mitigation must be <= baseline emissions
      # We represent this as baseline_emissions - mitigation >= 0
      constraints <- baseline_annual_emissions - mitig_quantities
      
      # Return the minimum constraint value
      # If this is negative, it means at least one constraint is violated
      return(min(constraints))
    }
    
    # Gradient of constraint function (optional but improves performance)
    constraint_gradient <- function(x) {
      # Use finite differences to estimate gradient of constraint function
      delta <- 1e-6
      grad <- numeric(length(x))
      f0 <- constraint_function(x)
      
      for (i in 1:length(x)) {
        x_perturbed <- x
        x_perturbed[i] <- x_perturbed[i] + delta
        f1 <- constraint_function(x_perturbed)
        grad[i] <- (f1 - f0) / delta
      }
      
      return(grad)
    }
    
    # Step 5: Set up initial guess and bounds
    if (verbose) cat("Setting up optimization parameters...\n")
    
    # Initial guess: start with meaningful mitigation and removal trajectories
    initial_mitig <- baseline_annual_emissions[1] * 
      seq(0.2, 0.7, length.out = n_control_points)
    initial_remov <- baseline_annual_emissions[1] * 
      seq(0.0, 0.3, length.out = n_control_points)
    initial_guess <- c(initial_mitig, initial_remov)
    
    # Bounds for variables
    # Lower bounds: all zeros
    lb <- rep(0, 2 * n_control_points)
    
    # Upper bounds: max baseline emissions for mitigation, some max for removal
    max_cdr <- 50  # Maximum technical potential for CDR
    ub_mitig <- rep(max(baseline_annual_emissions) * 1.2, n_control_points)
    ub_remov <- rep(max_cdr, n_control_points)
    ub <- c(ub_mitig, ub_remov)
    
    # Step 6: Run the optimization with SLSQP
    if (verbose) cat("Starting SLSQP optimization with", n_control_points, "control points...\n")
    
    # SLSQP options - note the different algorithm
    opts <- list(
      "algorithm" = "NLOPT_LD_SLSQP",  # Sequential Least-Squares Programming 
      "xtol_rel" = 1.0e-4,             # Relative tolerance on optimization parameters
      "ftol_rel" = 1.0e-4,             # Relative tolerance on function value
      "maxeval" = max_iterations,      # Maximum number of function evaluations
      "print_level" = ifelse(verbose, 1, 0)
    )
    
    # Run optimization with SLSQP
    optimization_result <- nloptr(
      x0 = initial_guess,
      eval_f = objective_function,
      eval_grad_f = gradient_function,  # Add gradient for SLSQP
      lb = lb,
      ub = ub,
      eval_g_ineq = constraint_function,  # Add inequality constraint
      eval_jac_g_ineq = constraint_gradient,  # Add constraint gradient
      opts = opts
    )
    
    # Check convergence
    converged <- optimization_result$status > 0
    if (verbose) {
      if (converged) {
        cat("SLSQP optimization converged successfully.\n")
      } else {
        cat("Warning: SLSQP optimization did not converge. Results may be suboptimal.\n")
      }
      cat("Objective function value:", optimization_result$objective, "\n")
      cat("Number of iterations:", optimization_result$iterations, "\n")
    }
    
    # Step 7: Extract solution and simulate trajectory
    if (verbose) cat("Extracting solution and simulating trajectory...\n")
    
    # Extract optimal control points
    mitig_control_pts <- optimization_result$solution[1:n_control_points]
    remov_control_pts <- optimization_result$solution[(n_control_points+1):(2*n_control_points)]
    
    # Expand to full time series
    optimal_mitig <- expand_controls(mitig_control_pts)
    optimal_remov <- expand_controls(remov_control_pts)
    
    # Cap mitigation at baseline emissions
    optimal_mitig <- pmin(optimal_mitig, baseline_annual_emissions)
    
    # Simulate trajectory with optimal controls
    optimal_emissions <- numeric(n_years)
    optimal_temperature <- numeric(n_years)
    optimal_cost_mitig <- numeric(n_years)
    optimal_cost_remov <- numeric(n_years)
    optimal_cost_resid <- numeric(n_years)
    optimal_cost_total <- numeric(n_years)
    annual_net_emissions <- numeric(n_years)
    
    # Initialize with starting values
    optimal_emissions[1] <- initial_cumulative_emissions  # Use the actual initial value
    optimal_temperature[1] <- clim_temp_init + ((optimal_emissions[1]/1000) * tcre)
    
    # Simulate forward with full dynamics
    for (t in 1:n_years) {
      # Calculate costs
      optimal_cost_mitig[t] <- cost_mitig_unit * (optimal_mitig[t]^exp_mitig) * 
        exp(-disc_rate * years_rel[t])
      optimal_cost_remov[t] <- cost_remov_unit * (optimal_remov[t]^exp_remov) * 
        exp(-disc_rate * years_rel[t])
      optimal_cost_resid[t] <- baseline_annual_gwp[t] * econ_dam_pct * 
        (optimal_temperature[t]^exp_temp_anom) * exp(-disc_rate * years_rel[t])
      optimal_cost_total[t] <- optimal_cost_mitig[t] + optimal_cost_remov[t] + optimal_cost_resid[t]
      
      # Calculate net emissions for the current period 
      annual_net_emissions[t] <- baseline_annual_emissions[t] - optimal_mitig[t] - optimal_remov[t]
      
      # Update state for next period
      if (t < n_years) {
        # Ensure cumulative emissions are properly updated with net emissions
        optimal_emissions[t+1] <- optimal_emissions[t] + annual_net_emissions[t]
        optimal_temperature[t+1] <- clim_temp_init + ((optimal_emissions[t+1]/1000) * tcre)
      }
    }
    
    # Calculate cumulative costs
    optimal_cost_mitig_cumul <- cumsum(optimal_cost_mitig)
    optimal_cost_remov_cumul <- cumsum(optimal_cost_remov)
    optimal_cost_resid_cumul <- cumsum(optimal_cost_resid)
    optimal_cost_total_cumul <- cumsum(optimal_cost_total)
    
    # Print a summary of the optimization
    if (verbose) {
      cat("\nOptimization summary:\n")
      cat("  Initial cumulative emissions:", optimal_emissions[1], "GtC\n")
      cat("  Final cumulative emissions:  ", optimal_emissions[n_years], "GtC\n")
      cat("  Change in emissions:         ", optimal_emissions[n_years] - optimal_emissions[1], "GtC\n")
      cat("  Initial temperature anomaly: ", optimal_temperature[1], "°C\n")
      cat("  Final temperature anomaly:   ", optimal_temperature[n_years], "°C\n")
      cat("  Change in temperature:       ", optimal_temperature[n_years] - optimal_temperature[1], "°C\n")
      cat("  Average annual mitigation:   ", mean(optimal_mitig), "GtC/yr\n")
      cat("  Average annual removal:      ", mean(optimal_remov), "GtC/yr\n")
      cat("  Total optimal cost:          ", tail(optimal_cost_total_cumul, 1), "$ Trillion\n")
    }
    
    # Verify that the simulation actually changed over time
    if (all(diff(optimal_emissions) == 0)) {
      warning("The cumulative emissions did not change over time. This suggests a problem with the dynamics.")
    }
    
  }, error = function(e) {
    # If an error occurs, capture it and continue
    has_error <- TRUE
    error_message <- e$message
    message("Error in SLSQP optimization: ", error_message)
  })
  
  # Build dummy data for error cases
  if (has_error || is.null(optimal_emissions)) {
    years <- vector_list$years
    years_rel <- vector_list$years_rel
    n_years <- vector_list$n_years
    initial_cumulative_emissions <- vector_list$cumulative_emissions[1]
    baseline_annual_emissions <- vector_list$baseline_annual_emissions
    baseline_annual_gwp <- vector_list$baseline_annual_gwp
    clim_temp_init <- parameter_df$clim_temp_init
    tcre <- parameter_df$tcre
    
    # Create a dummy trajectory that changes over time
    optimal_emissions <- numeric(n_years)
    optimal_emissions[1] <- initial_cumulative_emissions
    for (t in 2:n_years) {
      optimal_emissions[t] <- optimal_emissions[t-1] + baseline_annual_emissions[t-1]
    }
    optimal_temperature <- clim_temp_init + ((optimal_emissions/1000) * tcre)
    
    # Set control variables to zero
    optimal_mitig <- rep(0, n_years)
    optimal_remov <- rep(0, n_years)
    annual_net_emissions <- baseline_annual_emissions
    
    # Set cost variables to zero
    optimal_cost_mitig_cumul <- rep(0, n_years)
    optimal_cost_remov_cumul <- rep(0, n_years)
    optimal_cost_resid_cumul <- rep(0, n_years)
    optimal_cost_total_cumul <- rep(0, n_years)
    
    if (is.null(error_message)) {
      error_message <- "Optimization failed for unknown reasons"
    }
  }
  
  # Now build the result with all variables, whether from successful optimization or error handling
  result <- list(
    # Time variables
    years = vector_list$years,
    years_rel = vector_list$years_rel,
    n_years = vector_list$n_years,
    
    # State variables
    baseline_annual_emissions = vector_list$baseline_annual_emissions,
    temperature_anomaly = optimal_temperature,
    cumulative_emissions = optimal_emissions,
    
    # Control variables
    qty_mitig = optimal_mitig,
    qty_remov = optimal_remov,
    
    # Adjoint variable (shadow price)
    adjoint_var = rep(NA, vector_list$n_years),
    
    # Additional state information
    annual_net_emissions = annual_net_emissions,
    
    # Cost components
    baseline_annual_gwp = vector_list$baseline_annual_gwp,
    cost_mitig_cumul = optimal_cost_mitig_cumul,
    cost_remov_cumul = optimal_cost_remov_cumul,
    cost_resid_cumul = optimal_cost_resid_cumul,
    cost_total_cumul = optimal_cost_total_cumul,
    
    # Additional diagnostic information
    nlp_objective_value = if(exists("optimization_result")) optimization_result$objective else NA,
    nlp_iterations = if(exists("optimization_result")) optimization_result$iterations else NA,
    nlp_status = if(exists("optimization_result")) optimization_result$status else NA,
    nlp_message = if(exists("optimization_result")) optimization_result$message else NA,
    nlp_control_points = if(exists("n_control_points")) n_control_points else NA,
    converged = converged,
    error = has_error,
    error_message = error_message
  )
  
  if (verbose) cat("SLSQP optimization complete.\n")
  
  return(result)
}



#' @title Create Dashboard for Non-Linear Programming Solution (Simplified)
#' @description
#' Creates a dashboard visualizing the results of the non-linear programming solution,
#' matching exactly the style of your existing dashboard functions.
#' 
#' @param nlp_result The non-linear programming result list
#' @param save_plot Logical indicating whether to save the plot (default: TRUE)
#' @param show_legend Logical indicating whether to display a legend (default: FALSE)
#' @param width Plot width in mm (default: 190)
#' @param height Plot height in mm (default: 260)
#' @param filename Custom filename (default: generated based on current time)
#'                   
#' @return A ggplot object with layout matching create_combined_dashboard

create_nlp_dashboard <- function(nlp_result, 
                                 save_plot = TRUE,
                                 show_legend = FALSE,
                                 width = 190,
                                 height = 260,
                                 filename = NULL) {
  
  # Required packages
  library(ggplot2)
  library(patchwork)
  
  # Define theme for consistent styling
  my_theme <- theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # 1. Emissions plot (top-left)
  p1 <- ggplot() +
    geom_line(data = data.frame(x = nlp_result$years, y = nlp_result$cumulative_emissions),
              aes(x = x, y = y), color = "firebrick", linewidth = 1) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Cumulative Emissions",
         x = "Year",
         y = "Cumulative Emissions (GtC)")
  
  # 2. Temperature plot (top-right)
  p2 <- ggplot() +
    geom_line(data = data.frame(x = nlp_result$years, y = nlp_result$temperature_anomaly),
              aes(x = x, y = y), color = "steelblue", linewidth = 1) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Temperature Anomaly",
         x = "Year",
         y = "Temperature Anomaly (°C)")
  
  # 3. Mitigation plot (middle-left)
  p3 <- ggplot() +
    geom_line(data = data.frame(x = nlp_result$years, y = nlp_result$qty_mitig),
              aes(x = x, y = y), color = "forestgreen", linewidth = 1) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Mitigation Quantity",
         x = "Year",
         y = "Mitigation Quantity (GtC/yr)")
  
  # 4. Carbon Dioxide Removal plot (middle-right)
  p4 <- ggplot() +
    geom_line(data = data.frame(x = nlp_result$years, y = nlp_result$qty_remov),
              aes(x = x, y = y), color = "purple4", linewidth = 1) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Carbon Dioxide Removal",
         x = "Year",
         y = "Carbon Dioxide Removal (GtC/yr)")
  
  # 5. Adjoint Variable plot (bottom-left)
  p5 <- ggplot() +
    geom_line(data = data.frame(x = nlp_result$years, y = nlp_result$adjoint_var),
              aes(x = x, y = y), color = "darkorange", linewidth = 1) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Adjoint Variable",
         x = "Year",
         y = "Adjoint Variable Value")
  
  # 6. Create optimization information plot for the bottom-right area
  nlp_info <- data.frame(
    info = c("Objective Value:", "Converged:", "Error:"),
    value = c(
      ifelse(is.null(nlp_result$nlp_objective_value), "NA", 
             format(round(nlp_result$nlp_objective_value, 2), big.mark=",")),
      ifelse(is.null(nlp_result$converged), "Unknown", 
             ifelse(nlp_result$converged, "Yes", "No")),
      ifelse(is.null(nlp_result$error) || !nlp_result$error, "None", 
             ifelse(is.null(nlp_result$error_message), "Unknown error", nlp_result$error_message))
    )
  )
  
  p6 <- ggplot(nlp_info, aes(x = 1, y = rev(seq_along(info)))) +
    geom_text(aes(label = paste(info, value)), hjust = 0, size = 3) +
    xlim(1, 10) +
    theme_void() +
    labs(title = "NLP Solver Info")
  
  # Combine all plots using patchwork
  combined_plot <- (p1 + p2) /
    (p3 + p4) /
    (p5 + p6)
  
  # Add title with total cost
  total_cost <- if(!is.null(nlp_result$cost_total_cumul) && length(nlp_result$cost_total_cumul) > 0) {
    tail(nlp_result$cost_total_cumul, 1) 
  } else {
    NA
  }
  
  cost_text <- if(is.na(total_cost)) {
    "Total cost: Not available"
  } else {
    paste("Total cost at end of period:", 
          format(round(total_cost, 2), big.mark=","), "$ Trillion")
  }
  
  final_plot <- combined_plot + 
    patchwork::plot_annotation(
      title = "Non-Linear Programming Solution",
      subtitle = cost_text,
      theme = theme(
        plot.margin = margin(10, 0, 10, 0),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5)
      )
    )
  
  # Save the plot if requested
  if (save_plot) {
    # Generate filename if not provided
    if (is.null(filename)) {
      filename <- paste0("nlp_solution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    }
    
    # Full path to save the file
    filepath <- here::here("figs", filename)
    
    # Save using ggsave
    ggsave(
      filename = filepath,
      plot = final_plot,
      width = width,
      height = height,
      units = "mm"
    )
    
    message(paste("Plot saved as", filepath))
  }
  
  return(final_plot)
}