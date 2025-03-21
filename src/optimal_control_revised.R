library(dplyr)

initialize_optimal_control <- function(emissions_data,economic_data, params, scenario) {
  # Algorithm settings
  convergence_tolerance <- 0.001
  max_iterations <- 1e4
  
  # Extract parameter values for readability
  temp_init <- params$temp_init
  tcre <- params$tcre
  cost_mitig_unit <- params$cost_mitig_unit
  cost_remov_unit <- params$cost_remov_unit
  econ_dam_pct <- params$econ_dam_pct
  disc_rate <- params$disc_rate
  exp_mitig <- params$exp_mitig
  exp_remov <- params$exp_remov
  exp_dam <- params$exp_dam
  upd_weight <- params$upd_weight
  trans_cond <- params$trans_cond
  
  # Time setup
  years <- unique(emissions_data$Year) # to avoid getting duplicate years
  years_rel <- years - min(years)
  n_years <- length(years)
  
  # State variables
  # numeric vector containing the yearly baseline CO2 emissions from IPCC scenarios
  baseline_annual_emissions <- emissions_data %>%
    filter(Scenario == scenario) %>%
    pull(Value) # annual emissions for chosen scenario
  # numeric vector filled with NA values. updated during forward sweep.
  temperature_anomaly <- rep(NA, n_years)
  # numeric vector to track cumulative CO2 emissions (after mit and CDR). updated during forward sweep.
  cumulative_emissions <- rep(0, n_years)
  
  # Initial state
  cumulative_emissions[1] <- baseline_annual_emissions[1]
  temperature_anomaly[1] <- temp_init + tcre * cumulative_emissions[1]
  
  # Control variables with bounds
  # numeric vector of mitigation control at each time step, initialised to 0
  mitigation_amount <- rep(0, n_years)
  # numeric vector of CDR control at each time step, initialised to 0
  removal_amount <- rep(0, n_years)
  
  mitigation_min <- 0
  mitigation_max <- baseline_annual_emissions
  removal_min <- 0
  removal_max <- 100  # Could be parameterized based on technical limits
  
  # Adjoint variable
  adjoint_var <- rep(0, n_years)
  adjoint_var[n_years] <- trans_cond  # Set Terminal condition for final value of lambda
  
  # Cost components
  mitigation_cost <- rep(NA, n_years)
  removal_cost <- rep(NA, n_years)
  residual_damage <- rep(NA, n_years)
  total_cost <- rep(NA, n_years)
  
  # Initial cost calculation
  mitigation_cost[1] <- cost_mitig_unit * mitigation_amount[1]^exp_mitig * exp(-disc_rate * years_rel[1])
  removal_cost[1] <- cost_remov_unit * removal_amount[1]^exp_remov * exp(-disc_rate * years_rel[1])
  ##### FIX NEEDED HERE. ADD GWP
  residual_damage[1] <- econ_dam_pct * temperature_anomaly[1]^exp_dam * exp(-disc_rate * years_rel[1])
  total_cost[1] <- mitigation_cost[1] + removal_cost[1] + residual_damage[1]
  
  return(list(
    # Algorithm settings
    convergence_tolerance = convergence_tolerance,
    max_iterations = max_iterations,
    iteration = 0,
    converged = FALSE,
    
    # Time variables
    years = years,
    years_rel = years_rel,
    n_years = n_years,
    
    # State variables
    baseline_annual_emissions = baseline_annual_emissions,
    temperature_anomaly = temperature_anomaly,
    cumulative_emissions = cumulative_emissions,
    
    # Control variables and bounds
    mitigation_amount = mitigation_amount,
    removal_amount = removal_amount,
    mitigation_min = mitigation_min,
    mitigation_max = mitigation_max,
    removal_min = removal_min,
    removal_max = removal_max,
    
    # Adjoint variable
    adjoint_var = adjoint_var,
    
    # Cost components
    mitigation_cost = mitigation_cost,
    removal_cost = removal_cost,
    climate_damage = climate_damage,
    total_cost = total_cost,
    
    # Model parameters
    temp_init = temp_init,
    tcre = tcre,
    cost_mitig_unit = cost_mitig_unit,
    cost_remov = cost_remov,
    dam_const = dam_const,
    disc_rate = disc_rate,
    exp_mitig = exp_mitig,
    exp_remov = exp_remov,
    exp_dam = exp_dam,
    upd_weight = upd_weight
  ))
}




##### FORWARD SWEEP

forward_sweep <- function(model) {
  # Extract variables for readability
  n_years <- model$n_years
  baseline_annual_emissions <- model$baseline_annual_emissions
  mitigation_amount <- model$mitigation_amount
  removal_amount <- model$removal_amount
  cumulative_emissions <- model$cumulative_emissions
  temp_init <- model$temp_init
  tcre <- model$tcre
  
  # Forward integration of emissions and temperature
  for (i in 2:n_years) {
    # Calculate net emissions rate (could be negative with CDR)
    annual_net_emissions <- baseline_annual_emissions[i-1] - mitigation_amount[i-1] - removal_amount[i-1]
    
    # Update cumulative emissions
    cumulative_emissions[i] <- cumulative_emissions[i-1] + annual_net_emissions
    
    # Document what's happening for clarity
    if (i <= 5) {  # Only log first few steps to avoid excessive output
      cat(sprintf("Time step %d: Baseline=%f, Mitigation=%f, Removal=%f, Net=%f, Cumulative=%f\n",
                  i-1, baseline_annual_emissions[i-1], mitigation_amount[i-1], 
                  removal_amount[i-1], annual_net_emissions, cumulative_emissions[i]))
    }
  }
  
  # Calculate temperature based on cumulative emissions
  temperature_anomaly <- temp_init + tcre * cumulative_emissions
  
  # Update the model
  model$cumulative_emissions <- cumulative_emissions
  model$temperature_anomaly <- temperature_anomaly
  
  return(model)
}


###### BACKWARD SWEEP

backward_sweep <- function(model) {
  # Extract variables for readability
  n_years <- model$n_years
  years_rel <- model$years_rel
  adjoint_var <- model$adjoint_var
  cumulative_emissions <- model$cumulative_emissions
  temperature_anomaly <- model$temperature_anomaly
  
  # Extract parameters
  temp_init <- model$temp_init
  tcre <- model$tcre
  econ_dam_pct <- model$econ_dam_pct
  exp_dam <- model$exp_dam
  disc_rate <- model$disc_rate
  
  # Backward integration of adjoint equation
  for (i in 1:(n_years-1)) {
    # Calculate backward index (moving from end toward beginning)
    j <- n_years - i
    
    # Calculate derivative of adjoint variable
    # This represents how marginal changes in emissions affect the objective function
    # Get GDP for the current year
    current_gdp <- economic_data$Value[j]
    
    # Calculate derivative of adjoint variable
    adjoint_derivative <- -(current_gdp * econ_dam_pct * exp_dam * tcre) * 
      exp(-disc_rate * (years_rel[j] - years_rel[1])) * 
      (temperature_anomaly[j])^(exp_dam-1)
    
    # Update adjoint variable (backward Euler step)
    adjoint_var[j] <- adjoint_var[j+1] - adjoint_derivative
    
    # Document what's happening for clarity
    if (i <= 5) {  # Only log first few steps to avoid excessive output
      cat(sprintf("Backward step %d (year %d): Adjoint derivative=%f, Adjoint=%f\n",
                  i, j, adjoint_derivative, adjoint_var[j]))
    }
  }
  
  # Update the model with new adjoint values
  model$adjoint_var <- adjoint_var
  
  return(model)
}


update_controls <- function(model) {
  # Extract variables for readability
  n_years <- model$n_years
  years_rel <- model$years_rel
  adjoint_var <- model$adjoint_var
  baseline_annual_emissions <- model$baseline_annual_emissions
  mitigation_amount <- model$mitigation_amount
  removal_amount <- model$removal_amount
  disc_rate <- model$disc_rate
  
  # Extract cost parameters and exponents
  cost_mitig <- model$cost_mitig
  cost_remov <- model$cost_remov
  exp_mitig <- model$exp_mitig
  exp_remov <- model$exp_remov
  upd_weight <- model$upd_weight
  
  # Store previous control values for convex combination
  previous_mitigation <- mitigation_amount
  previous_removal <- removal_amount
  
  # Update mitigation control based on optimality condition
  # This comes from solving ∂H/∂u_m = 0 for u_m
  new_mitigation <- (adjoint_var/(exp_mitig*cost_mitig) * exp(disc_rate*years_rel))^(1/(exp_mitig-1))
  
  # Apply bounds to mitigation (can't mitigate more than baseline emissions)
  new_mitigation <- pmin(new_mitigation, baseline_annual_emissions)  # Upper bound
  new_mitigation <- pmax(new_mitigation, 0)                          # Lower bound
  
  # Update removal control based on optimality condition
  # This comes from solving ∂H/∂u_r = 0 for u_r
  new_removal <- (adjoint_var/(exp_remov*cost_remov) * exp(disc_rate*years_rel))^(1/(exp_remov-1))
  
  # Apply bounds to removal
  new_removal[new_removal < 0] <- 0                # Lower bound (can't be negative)
  new_removal[new_removal > 100] <- 100            # Upper bound (technological limit)
  
  # Use convex combination to smooth updates (helps convergence)
  mitigation_amount <- upd_weight * new_mitigation + (1 - upd_weight) * previous_mitigation
  removal_amount <- upd_weight * new_removal + (1 - upd_weight) * previous_removal
  
  # Document a few updates for clarity
  cat("Control updates example (first 3 years):\n")
  for (i in 1:min(3, n_years)) {
    cat(sprintf("Year %d: Mitigation=%f → %f, Removal=%f → %f\n", 
                i, previous_mitigation[i], mitigation_amount[i],
                previous_removal[i], removal_amount[i]))
  }
  
  # Update the model
  model$mitigation_amount <- mitigation_amount
  model$removal_amount <- removal_amount
  
  return(model)
}

test_convergence <- function(model, previous_model) {
  # Extract convergence tolerance
  tol <- model$convergence_tolerance
  
  # Calculate absolute differences for key variables
  mitigation_diff <- sum(abs(model$mitigation_amount - previous_model$mitigation_amount))
  removal_diff <- sum(abs(model$removal_amount - previous_model$removal_amount))
  emissions_diff <- sum(abs(model$cumulative_emissions - previous_model$cumulative_emissions))
  adjoint_diff <- sum(abs(model$adjoint_var - previous_model$adjoint_var))
  
  # Calculate convergence metrics (scaled by variable magnitudes)
  # These should be non-negative when converged
  mitigation_conv <- tol * sum(abs(model$mitigation_amount)) - mitigation_diff
  removal_conv <- tol * sum(abs(model$removal_amount)) - removal_diff
  emissions_conv <- tol * sum(abs(model$cumulative_emissions)) - emissions_diff
  adjoint_conv <- tol * sum(abs(model$adjoint_var)) - adjoint_diff
  
  # Take minimum of all metrics - all must be positive for convergence
  min_convergence <- min(mitigation_conv, removal_conv, emissions_conv, adjoint_conv)
  
  # Report on convergence progress
  cat(sprintf("Convergence test results:\n"))
  cat(sprintf("Mitigation: %.6f\n", mitigation_conv))
  cat(sprintf("Removal: %.6f\n", removal_conv))
  cat(sprintf("Emissions: %.6f\n", emissions_conv))
  cat(sprintf("Adjoint: %.6f\n", adjoint_conv))
  cat(sprintf("Min value: %.6f\n", min_convergence))
  
  # Update convergence status
  has_converged <- min_convergence >= 0
  
  # Return updated model with convergence status
  model$converged <- has_converged
  return(list(
    model = model,
    converged = has_converged,
    convergence_value = min_convergence
  ))
}

test_convergence <- function(model, previous_mitigation, previous_removal, 
                             previous_emissions, previous_adjoint) {
  # Extract convergence tolerance
  tol <- model$convergence_tolerance
  
  # Calculate convergence metrics for each variable
  mitigation_conv <- tol * sum(abs(model$mitigation_amount)) - 
    sum(abs(previous_mitigation - model$mitigation_amount))
  
  removal_conv <- tol * sum(abs(model$removal_amount)) - 
    sum(abs(previous_removal - model$removal_amount))
  
  emissions_conv <- tol * sum(abs(model$cumulative_emissions)) - 
    sum(abs(previous_emissions - model$cumulative_emissions))
  
  adjoint_conv <- tol * sum(abs(model$adjoint_var)) - 
    sum(abs(previous_adjoint - model$adjoint_var))
  
  # Take minimum of all metrics
  min_convergence <- min(mitigation_conv, removal_conv, emissions_conv, adjoint_conv)
  
  # Return convergence status and value
  has_converged <- min_convergence >= 0
  
  # Log convergence information
  cat(sprintf("Convergence metrics - Mitigation: %.6f, Removal: %.6f, Emissions: %.6f, Adjoint: %.6f\n", 
              mitigation_conv, removal_conv, emissions_conv, adjoint_conv))
  cat(sprintf("Convergence status: %s (value: %.6f)\n", 
              ifelse(has_converged, "CONVERGED", "not converged"), min_convergence))
  
  return(list(
    converged = has_converged,
    value = min_convergence
  ))
}