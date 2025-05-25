#' Step 6.1: Real IPCC Data with Proper Shooting Method
#' Combines the working Step 6 algorithm with shooting method to enforce terminal constraint

library(deSolve)
library(ggplot2)
library(dplyr)
library(here)
library(yaml)
library(tidyr)

# Load your existing functions
source(here::here("src", "model_parameters.R"))
source(here::here("src", "data_preparation.R"))

#' Forward-Backward Sweep Function (for shooting method)
#' This is the core algorithm that will be called by the shooting method
forward_backward_sweep_shooting <- function(parameter_df, emissions_df, economic_df, scenario, 
                                            target_emissions, terminal_adjoint) {
  
  # Filter data by scenario
  emissions_scenario <- emissions_df %>%
    filter(Scenario == scenario) %>%
    arrange(Year)
  
  economic_scenario <- economic_df %>%
    filter(Scenario == scenario) %>%
    arrange(Year)
  
  # Extract parameters
  cost_mitig_unit <- parameter_df$cost_mitig_unit
  cost_remov_unit <- parameter_df$cost_remov_unit
  exp_mitig <- parameter_df$exp_mitig
  exp_remov <- parameter_df$exp_remov
  exp_temp_anom <- parameter_df$exp_temp_anom
  clim_temp_init <- parameter_df$clim_temp_init
  tcre <- parameter_df$tcre
  econ_dam_pct <- parameter_df$econ_dam_pct
  disc_rate <- parameter_df$disc_rate
  
  # Time and data setup
  years <- emissions_scenario$Year
  years_rel <- years - min(years)
  n_years <- length(years)
  dt <- 1
  baseline_emissions <- emissions_scenario$Value
  baseline_gwp <- economic_scenario$Value
  
  # Initialize variables
  cumulative_emissions <- rep(0, n_years)
  temperature_anomaly <- rep(clim_temp_init, n_years)
  qty_mitig <- rep(1, n_years)    
  qty_remov <- rep(0.1, n_years)  
  adjoint_var <- rep(0, n_years)
  
  # Set terminal condition from shooting method
  adjoint_var[n_years] <- terminal_adjoint
  
  # Algorithm parameters
  max_iterations <- 200  # Fewer iterations for shooting method calls
  tolerance <- 0.01
  update_weight <- 0.1   # More aggressive for faster convergence in shooting
  
  # Forward-Backward Sweep Loop
  for (iter in 1:max_iterations) {
    
    # Store previous values
    prev_mitig <- qty_mitig
    prev_remov <- qty_remov
    prev_cumulative <- cumulative_emissions
    prev_adjoint <- adjoint_var
    
    # === FORWARD SWEEP ===
    cumulative_emissions[1] <- 0
    for (i in 2:n_years) {
      annual_net <- baseline_emissions[i-1] - qty_mitig[i-1] - qty_remov[i-1]
      cumulative_emissions[i] <- cumulative_emissions[i-1] + annual_net * dt
    }
    
    # Calculate temperature anomaly
    temperature_anomaly <- clim_temp_init + (cumulative_emissions / 1000) * tcre
    temperature_anomaly <- pmax(temperature_anomaly, 0.1)
    
    # === BACKWARD SWEEP ===
    # Keep terminal condition fixed (set by shooting method)
    # Integrate backward from terminal condition
    for (i in (n_years-1):1) {
      j <- i  
      
      temp_base <- temperature_anomaly[j]
      discount_factor <- exp(-disc_rate * years_rel[j])
      gwp_value <- baseline_gwp[j]
      
      # Adjoint derivative
      adjoint_derivative <- -(exp_temp_anom * gwp_value * econ_dam_pct * 
                                (tcre / 1000) * discount_factor * 
                                (temp_base^(exp_temp_anom - 1)))
      
      if (!is.finite(adjoint_derivative)) {
        adjoint_derivative <- 0
      }
      
      # Backward integration
      adjoint_var[j] <- adjoint_var[j+1] - adjoint_derivative * dt
    }
    
    # === UPDATE CONTROLS ===
    new_mitig <- rep(0, n_years)
    new_remov <- rep(0, n_years)
    
    for (i in 1:n_years) {
      
      discount_factor <- exp(-disc_rate * years_rel[i])
      
      # Mitigation control
      mitig_denominator <- 2 * cost_mitig_unit * discount_factor
      if (mitig_denominator > 1e-10) {
        um_unconstrained <- adjoint_var[i] / mitig_denominator
      } else {
        um_unconstrained <- 0
      }
      
      # Apply mitigation bounds
      if (um_unconstrained <= 0) {
        new_mitig[i] <- 0
      } else if (um_unconstrained >= baseline_emissions[i]) {
        new_mitig[i] <- baseline_emissions[i]
      } else {
        new_mitig[i] <- um_unconstrained
      }
      
      # CDR control  
      remov_denominator <- 2 * cost_remov_unit * discount_factor
      if (remov_denominator > 1e-10) {
        ur_unconstrained <- adjoint_var[i] / remov_denominator
      } else {
        ur_unconstrained <- 0
      }
      
      # Apply CDR bounds (only non-negativity)
      if (ur_unconstrained <= 0) {
        new_remov[i] <- 0
      } else {
        new_remov[i] <- ur_unconstrained
      }
    }
    
    # Update with smoothing
    qty_mitig <- update_weight * new_mitig + (1 - update_weight) * prev_mitig
    qty_remov <- update_weight * new_remov + (1 - update_weight) * prev_remov
    
    # Check convergence
    mitig_change <- sum(abs(qty_mitig - prev_mitig))
    remov_change <- sum(abs(qty_remov - prev_remov))
    cumulative_change <- sum(abs(cumulative_emissions - prev_cumulative))
    adjoint_change <- sum(abs(adjoint_var - prev_adjoint))
    
    total_change <- mitig_change + remov_change + cumulative_change + adjoint_change
    
    if (total_change < tolerance) {
      break
    }
  }
  
  # Return final cumulative emissions for shooting method
  final_emissions <- cumulative_emissions[n_years]
  
  # Calculate comprehensive cost breakdown
  final_temperature <- temperature_anomaly[n_years]
  discount_factors <- exp(-disc_rate * years_rel)
  
  # Annual costs
  mitig_costs_annual <- cost_mitig_unit * qty_mitig^exp_mitig * discount_factors
  remov_costs_annual <- cost_remov_unit * qty_remov^exp_remov * discount_factors
  temp_costs_annual <- baseline_gwp * econ_dam_pct * (temperature_anomaly^exp_temp_anom) * discount_factors
  
  # Total costs
  total_mitig_cost <- sum(mitig_costs_annual)
  total_remov_cost <- sum(remov_costs_annual)
  total_temp_cost <- sum(temp_costs_annual)
  total_cost <- total_mitig_cost + total_remov_cost + total_temp_cost
  
  return(list(
    final_emissions = final_emissions,
    emission_gap = final_emissions - target_emissions,
    converged = (total_change < tolerance),
    iterations = iter,
    
    # Full solution vectors
    years = years,
    years_rel = years_rel,
    n_years = n_years,
    baseline_annual_emissions = baseline_emissions,
    baseline_annual_gwp = baseline_gwp,
    cumulative_emissions = cumulative_emissions,
    temperature_anomaly = temperature_anomaly,
    qty_mitig = qty_mitig,
    qty_remov = qty_remov,
    adjoint_var = adjoint_var,
    
    # Cost breakdown - NEW
    total_cost = total_cost,
    mitig_cost = total_mitig_cost,
    remov_cost = total_remov_cost,
    temp_cost = total_temp_cost,
    final_temperature = final_temperature
  ))
}

#' Shooting Method with Real IPCC Data
real_data_shooting_method <- function(parameter_df, emissions_df, economic_df, scenario, target_emissions = 600) {
  
  cat("=== Step 6.1: Real IPCC Data with Shooting Method ===\n")
  cat("Scenario:", scenario, "\n")
  cat("Target emissions:", target_emissions, "GtCO2\n\n")
  
  # Shooting method parameters
  max_shooting_iterations <- 200  # Increased from 50
  shooting_tolerance <- 1.0  # 1 GtCO2 tolerance for terminal constraint
  
  # Initial bounds for terminal adjoint value
  # Start with wider bounds since we don't know the scale
  lambda_low <- -1000
  lambda_high <- 1000
  
  cat("Starting shooting method...\n")
  cat("Initial lambda bounds: [", lambda_low, ",", lambda_high, "]\n")
  
  # Test lower bound
  result_low <- forward_backward_sweep_shooting(parameter_df, emissions_df, economic_df, 
                                                scenario, target_emissions, lambda_low)
  
  if (result_low$converged) {
    emission_gap_low <- result_low$emission_gap
    cat("Lower bound result: final emissions =", round(result_low$final_emissions, 1), 
        ", gap =", round(emission_gap_low, 1), "\n")
  } else {
    cat("WARNING: Lower bound did not converge\n")
    emission_gap_low <- Inf
  }
  
  # Test upper bound
  result_high <- forward_backward_sweep_shooting(parameter_df, emissions_df, economic_df, 
                                                 scenario, target_emissions, lambda_high)
  
  if (result_high$converged) {
    emission_gap_high <- result_high$emission_gap
    cat("Upper bound result: final emissions =", round(result_high$final_emissions, 1), 
        ", gap =", round(emission_gap_high, 1), "\n")
  } else {
    cat("WARNING: Upper bound did not converge\n")
    emission_gap_high <- -Inf
  }
  
  # Check if we have a valid bracket
  if (sign(emission_gap_low) == sign(emission_gap_high)) {
    cat("WARNING: Initial bounds may not bracket the solution\n")
    cat("Adjusting bounds...\n")
    
    # Try to find a better bracket
    if (emission_gap_low > 0 && emission_gap_high > 0) {
      # Both give emissions above target, need lower lambda
      lambda_low <- lambda_low * 2
      lambda_high <- lambda_low + 100
    } else if (emission_gap_low < 0 && emission_gap_high < 0) {
      # Both give emissions below target, need higher lambda  
      lambda_high <- lambda_high * 2
      lambda_low <- lambda_high - 100
    }
    
    cat("Adjusted bounds: [", lambda_low, ",", lambda_high, "]\n")
  }
  
  # Main shooting method loop (secant method)
  best_result <- NULL
  best_gap <- Inf
  
  for (iteration in 1:max_shooting_iterations) {
    
    # Progress reporting every 10 iterations
    if (iteration %% 10 == 1) {
      cat("\n=== Shooting Progress Report (Iteration", iteration, ") ===\n")
    } else {
      cat("\nShooting iteration", iteration, ":\n")
    }
    
    # Secant method update
    if (abs(emission_gap_high - emission_gap_low) < 1e-10) {
      cat("WARNING: Numerical issues in secant method\n")
      break
    }
    
    # Calculate new lambda using secant method
    lambda_new <- lambda_low - emission_gap_low * (lambda_high - lambda_low) / 
      (emission_gap_high - emission_gap_low)
    
    cat("Trying lambda =", round(lambda_new, 4), "\n")
    
    # Evaluate at new point
    result_new <- forward_backward_sweep_shooting(parameter_df, emissions_df, economic_df, 
                                                  scenario, target_emissions, lambda_new)
    
    if (!result_new$converged) {
      cat("Inner loop did not converge, trying smaller step\n")
      lambda_new <- (lambda_low + lambda_high) / 2
      result_new <- forward_backward_sweep_shooting(parameter_df, emissions_df, economic_df, 
                                                    scenario, target_emissions, lambda_new)
    }
    
    emission_gap_new <- result_new$emission_gap
    
    cat("Result: final emissions =", round(result_new$final_emissions, 1), 
        ", gap =", round(emission_gap_new, 1), "\n")
    
    # Track best result
    if (abs(emission_gap_new) < abs(best_gap)) {
      best_result <- result_new
      best_gap <- emission_gap_new
    }
    
    # Check convergence
    if (abs(emission_gap_new) <= shooting_tolerance) {
      cat("Shooting method converged!\n")
      return(result_new)
    }
    
    # Update bounds for next iteration
    if (sign(emission_gap_new) == sign(emission_gap_low)) {
      lambda_low <- lambda_new
      emission_gap_low <- emission_gap_new
    } else {
      lambda_high <- lambda_new  
      emission_gap_high <- emission_gap_new
    }
  }
  
  cat("\nShooting method reached max iterations\n")
  cat("Best result found: gap =", round(best_gap, 1), "GtCO2\n")
  
  return(best_result)
}

#' Test Step 6.1
test_step6_shooting <- function() {
  
  # Load parameters and data
  parameter_df <- create_params_dataframe()
  
  cat("Loading IPCC data...\n")
  emissions_imported <- import_ssp_emissions("emissions.csv")
  economic_imported <- import_ssp_economic("gwp.csv")
  
  emissions_df <- interpolate_ssp_emissions(
    emissions_df = emissions_imported,
    dt = 1,
    start_year = 2020,
    end_year = 2100
  )
  
  economic_df <- interpolate_ssp_economic(
    economic_df = economic_imported,
    dt = 1,
    start_year = 2020,
    end_year = 2100
  )
  
  scenario <- "SSP3-Baseline"
  target <- 600
  
  # Run shooting method
  result <- real_data_shooting_method(parameter_df, emissions_df, economic_df, scenario, target)
  
  if (!is.null(result)) {
    cat("\n=== FINAL RESULTS ===\n")
    cat("Scenario:", scenario, "\n")
    cat("Target:", target, "GtCO2\n")
    cat("Final emissions:", round(result$final_emissions, 1), "GtCO2\n")
    cat("Emission gap:", round(result$emission_gap, 1), "GtCO2\n")
    cat("Final temperature:", round(result$final_temperature, 2), "°C\n\n")
    
    # Calculate totals for verification
    total_baseline <- sum(result$baseline_annual_emissions)
    total_mitigation <- sum(result$qty_mitig)
    total_cdr <- sum(result$qty_remov)
    total_control <- total_mitigation + total_cdr
    
    cat("Verification:\n")
    cat("Total baseline emissions:", round(total_baseline, 1), "GtCO2\n")
    cat("Total mitigation:", round(total_mitigation, 1), "GtCO2\n")
    cat("Total CDR:", round(total_cdr, 1), "GtCO2\n")
    cat("Total control needed:", round(total_baseline - target, 1), "GtCO2\n")
    cat("Total control applied:", round(total_control, 1), "GtCO2\n")
    cat("Control efficiency:", round(100 * (total_baseline - target) / total_control, 1), "%\n\n")
    
    # Cost breakdown
    cat("Cost Breakdown (discounted):\n")
    cat("Total cost:", round(result$total_cost, 1), "trillion $\n")
    cat("Mitigation cost:", round(result$mitig_cost, 1), "trillion $ (", 
        round(100*result$mitig_cost/result$total_cost, 1), "%)\n")
    cat("CDR cost:", round(result$remov_cost, 1), "trillion $ (", 
        round(100*result$remov_cost/result$total_cost, 1), "%)\n")
    cat("Temperature damage cost:", round(result$temp_cost, 1), "trillion $ (", 
        round(100*result$temp_cost/result$total_cost, 1), "%)\n")
  }
  
  return(result)
}

# Run the test
if (TRUE) {
  result_step6_shooting <- test_step6_shooting()
}