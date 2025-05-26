#' Step 6: Test Real IPCC Data with Working Algorithm
#' Uses the proven Step 5 algorithm but with your actual IPCC emissions and economic data
#' This will isolate whether data or code was causing the original problems

library(deSolve)
library(ggplot2)
library(dplyr)
library(here)
library(yaml)
library(tidyr)

# Load your existing functions
source(here::here("src", "model_parameters.R"))
source(here::here("src", "data_preparation.R"))

#' Step 6: Full Model with Real IPCC Data
#' Uses the working Step 5 algorithm but feeds it real data
real_data_full_model <- function(parameter_df, emissions_df, economic_df, scenario, target_emissions = 600) {
  
  cat("=== Step 6: Testing Real IPCC Data ===\n")
  cat("Scenario:", scenario, "\n")
  
  # Filter data by scenario
  emissions_scenario <- emissions_df %>%
    filter(Scenario == scenario) %>%
    arrange(Year)
  
  economic_scenario <- economic_df %>%
    filter(Scenario == scenario) %>%
    arrange(Year)
  
  # Check data availability
  if (nrow(emissions_scenario) == 0) {
    stop("No emissions data found for scenario: ", scenario)
  }
  
  if (nrow(economic_scenario) == 0) {
    stop("No economic data found for scenario: ", scenario) 
  }
  
  cat("Emissions data points:", nrow(emissions_scenario), "\n")
  cat("Economic data points:", nrow(economic_scenario), "\n")
  
  # Extract parameter values
  cost_mitig_unit <- parameter_df$cost_mitig_unit
  cost_remov_unit <- parameter_df$cost_remov_unit
  exp_mitig <- parameter_df$exp_mitig
  exp_remov <- parameter_df$exp_remov
  exp_temp_anom <- parameter_df$exp_temp_anom
  
  # Climate parameters
  clim_temp_init <- parameter_df$clim_temp_init
  tcre <- parameter_df$tcre
  
  # Economic parameters  
  econ_dam_pct <- parameter_df$econ_dam_pct
  disc_rate <- parameter_df$disc_rate
  
  # Time setup from data
  years <- emissions_scenario$Year
  years_rel <- years - min(years)
  n_years <- length(years)
  dt <- 1
  
  cat("Time period:", min(years), "to", max(years), "(", n_years, "years)\n")
  
  # Extract real data values
  baseline_emissions <- emissions_scenario$Value
  baseline_gwp <- economic_scenario$Value
  
  # Data diagnostics
  cat("\nData Diagnostics:\n")
  cat("Emissions range:", round(min(baseline_emissions), 1), "to", round(max(baseline_emissions), 1), "GtCO2/year\n")
  cat("Economic range:", round(min(baseline_gwp), 1), "to", round(max(baseline_gwp), 1), "trillion USD/year\n")
  cat("Total baseline emissions:", round(sum(baseline_emissions), 1), "GtCO2\n")
  
  # Check for any problematic data values
  if (any(is.na(baseline_emissions)) || any(is.na(baseline_gwp))) {
    warning("NA values detected in data!")
  }
  
  if (any(baseline_emissions <= 0)) {
    warning("Non-positive emissions detected!")
  }
  
  if (any(baseline_gwp <= 0)) {
    warning("Non-positive GWP detected!")
  }
  
  # Initialize variables
  cumulative_emissions <- rep(0, n_years)
  temperature_anomaly <- rep(clim_temp_init, n_years)
  qty_mitig <- rep(1, n_years)    
  qty_remov <- rep(0.1, n_years)  
  adjoint_var <- rep(0, n_years)
  
  # Algorithm parameters - start conservative with real data
  max_iterations <- 500
  tolerance <- 0.01
  update_weight <- 0.05  # Even more conservative with real data
  
  cat("\nStarting optimization with real IPCC data...\n")
  cat("Update weight:", update_weight, "\n")
  cat("Tolerance:", tolerance, "\n\n")
  
  # Forward-Backward Sweep Loop
  for (iter in 1:max_iterations) {
    
    # Store previous values
    prev_mitig <- qty_mitig
    prev_remov <- qty_remov
    prev_cumulative <- cumulative_emissions
    prev_adjoint <- adjoint_var
    
    # === FORWARD SWEEP ===
    for (i in 1:n_years) {
      if (i == 1) {
        # First year: start from zero and add first year's net emissions
        annual_net <- baseline_emissions[i] - qty_mitig[i] - qty_remov[i]
        cumulative_emissions[i] <- 0 + annual_net * dt
      } else {
        # Subsequent years: add to previous cumulative total
        annual_net <- baseline_emissions[i] - qty_mitig[i] - qty_remov[i]
        cumulative_emissions[i] <- cumulative_emissions[i-1] + annual_net * dt
      }
    }
    
    # Calculate temperature anomaly based on cumulative emissions
    temperature_anomaly <- clim_temp_init + (cumulative_emissions / 1000) * tcre
    
    # Ensure temperature stays positive for power calculations
    temperature_anomaly <- pmax(temperature_anomaly, 0.1)
    
    # Check for temperature anomalies
    if (any(!is.finite(temperature_anomaly))) {
      cat("ERROR: Non-finite temperature values at iteration", iter, "\n")
      break
    }
    
    # === BACKWARD SWEEP ===
    current_final <- cumulative_emissions[n_years]
    emission_error <- current_final - target_emissions
    
    # Initialize terminal adjoint
    if (iter == 1) {
      adjoint_var[n_years] <- cost_mitig_unit * emission_error / n_years
    }
    
    # Backward integration of adjoint equation
    for (i in (n_years-1):1) {
      j <- i  # Current time index
      
      # Calculate adjoint derivative
      temp_base <- temperature_anomaly[j]
      discount_factor <- exp(-disc_rate * years_rel[j])
      gwp_value <- baseline_gwp[j]
      
      # Check for numerical issues before calculation
      if (!is.finite(temp_base) || !is.finite(discount_factor) || !is.finite(gwp_value)) {
        cat("WARNING: Non-finite values at time", j, "- temp:", temp_base, "discount:", discount_factor, "gwp:", gwp_value, "\n")
        adjoint_derivative <- 0
      } else {
        # The adjoint derivative from your KKT formulation
        adjoint_derivative <- -(exp_temp_anom * gwp_value * econ_dam_pct * 
                                  (tcre / 1000) * discount_factor * 
                                  (temp_base^(exp_temp_anom - 1)))
        
        # Check result
        if (!is.finite(adjoint_derivative)) {
          cat("WARNING: Non-finite adjoint derivative at time", j, "\n")
          adjoint_derivative <- 0
        }
      }
      
      # Backward Euler step
      adjoint_var[j] <- adjoint_var[j+1] - adjoint_derivative * dt
    }
    
    # Check adjoint for problems
    if (any(!is.finite(adjoint_var))) {
      cat("ERROR: Non-finite adjoint values at iteration", iter, "\n")
      break
    }
    
    # === UPDATE CONTROLS ===
    new_mitig <- rep(0, n_years)
    new_remov <- rep(0, n_years)
    
    for (i in 1:n_years) {
      
      discount_factor <- exp(-disc_rate * years_rel[i])
      
      # --- MITIGATION CONTROL ---
      mitig_denominator <- 2 * cost_mitig_unit * discount_factor
      if (mitig_denominator > 1e-10) {
        um_unconstrained <- adjoint_var[i] / mitig_denominator
      } else {
        um_unconstrained <- 0
      }
      
      # Apply bounds
      if (um_unconstrained <= 0) {
        new_mitig[i] <- 0
      } else if (um_unconstrained >= baseline_emissions[i]) {
        new_mitig[i] <- baseline_emissions[i]
      } else {
        new_mitig[i] <- um_unconstrained
      }
      
      # --- CDR CONTROL ---
      remov_denominator <- 2 * cost_remov_unit * discount_factor
      if (remov_denominator > 1e-10) {
        ur_unconstrained <- adjoint_var[i] / remov_denominator
      } else {
        ur_unconstrained <- 0
      }
      
      # Apply non-negativity constraint
      if (ur_unconstrained <= 0) {
        new_remov[i] <- 0
      } else {
        new_remov[i] <- ur_unconstrained
      }
    }
    
    # Check controls for problems
    if (any(!is.finite(new_mitig)) || any(!is.finite(new_remov))) {
      cat("ERROR: Non-finite control values at iteration", iter, "\n")
      break
    }
    
    # Conservative update
    qty_mitig <- update_weight * new_mitig + (1 - update_weight) * prev_mitig
    qty_remov <- update_weight * new_remov + (1 - update_weight) * prev_remov
    
    # === CHECK CONVERGENCE ===
    mitig_change <- sum(abs(qty_mitig - prev_mitig))
    remov_change <- sum(abs(qty_remov - prev_remov))
    cumulative_change <- sum(abs(cumulative_emissions - prev_cumulative))
    adjoint_change <- sum(abs(adjoint_var - prev_adjoint))
    
    total_change <- mitig_change + remov_change + cumulative_change + adjoint_change
    
    if (total_change < tolerance) {
      cat("Converged in", iter, "iterations\n")
      break
    }
    
    if (iter %% 25 == 0) {
      cat("Iteration", iter, ":\n")
      cat("  Emission gap:", round(emission_error, 3), "\n")
      cat("  Total change:", round(total_change, 6), "\n")
      cat("  Max temperature:", round(max(temperature_anomaly), 2), "°C\n")
      cat("  Adjoint range: [", round(min(adjoint_var), 2), ",", 
          round(max(adjoint_var), 2), "]\n")
      cat("  Control ranges: mitig [", round(min(qty_mitig), 2), ",", round(max(qty_mitig), 2), 
          "], cdr [", round(min(qty_remov), 2), ",", round(max(qty_remov), 2), "]\n\n")
    }
    
    if (iter == max_iterations) {
      cat("WARNING: Max iterations reached\n")
    }
  }
  
  # Calculate comprehensive results
  final_emissions <- cumulative_emissions[n_years]
  final_temperature <- temperature_anomaly[n_years]
  
  # Calculate costs with proper discounting
  discount_factors <- exp(-disc_rate * years_rel)
  mitig_costs <- cost_mitig_unit * qty_mitig^exp_mitig * discount_factors
  remov_costs <- cost_remov_unit * qty_remov^exp_remov * discount_factors
  temp_costs <- baseline_gwp * econ_dam_pct * (temperature_anomaly^exp_temp_anom) * discount_factors
  
  total_mitig_cost <- sum(mitig_costs)
  total_remov_cost <- sum(remov_costs)
  total_temp_cost <- sum(temp_costs)
  total_cost <- total_mitig_cost + total_remov_cost + total_temp_cost
  
  return(list(
    # Input data characteristics
    scenario = scenario,
    data_years = range(years),
    n_years = n_years,
    baseline_emissions_range = range(baseline_emissions),
    baseline_gwp_range = range(baseline_gwp),
    
    # Solution arrays
    years = years,
    years_rel = years_rel,
    baseline_annual_emissions = baseline_emissions,
    baseline_annual_gwp = baseline_gwp,
    cumulative_emissions = cumulative_emissions,
    temperature_anomaly = temperature_anomaly,
    qty_mitig = qty_mitig,
    qty_remov = qty_remov,
    adjoint_var = adjoint_var,
    
    # Solution quality
    final_emissions = final_emissions,
    final_temperature = final_temperature,
    target_emissions = target_emissions,
    emission_gap = final_emissions - target_emissions,
    
    total_cost = total_cost,
    mitig_cost = total_mitig_cost,
    remov_cost = total_remov_cost,
    temp_cost = total_temp_cost,
    
    iterations = iter,
    converged = (total_change < tolerance)
  ))
}

#' Test Step 6 with Real Data
test_step6 <- function() {
  
  cat("=== Testing Step 6: Real IPCC Data with Working Algorithm ===\n\n")
  
  # Load your actual parameters
  parameter_df <- create_params_dataframe()
  
  # Import your real IPCC data using your existing functions
  cat("Loading real IPCC data...\n")
  emissions_imported <- import_ssp_emissions("emissions.csv")
  economic_imported <- import_ssp_economic("gwp.csv")
  
  # Interpolate using your existing functions
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
  
  # Test with your chosen scenario
  scenario <- "SSP3-Baseline"
  
  cat("Testing scenario:", scenario, "\n\n")
  
  # Solve with real data
  result <- real_data_full_model(parameter_df, emissions_df, economic_df, scenario, 600)
  
  # Report results
  cat("\n=== RESULTS ===\n")
  cat("Scenario:", result$scenario, "\n")
  cat("Data period:", result$data_years[1], "to", result$data_years[2], "\n")
  cat("Converged:", result$converged, "in", result$iterations, "iterations\n")
  cat("Final emissions:", round(result$final_emissions, 1), "GtCO2\n")
  cat("Final temperature:", round(result$final_temperature, 2), "°C\n")
  cat("Emission gap:", round(result$emission_gap, 3), "\n\n")
  
  cat("Data Characteristics:\n")
  cat("Baseline emissions range:", round(result$baseline_emissions_range[1], 1), 
      "to", round(result$baseline_emissions_range[2], 1), "GtCO2/year\n")
  cat("Baseline GWP range:", round(result$baseline_gwp_range[1], 1), 
      "to", round(result$baseline_gwp_range[2], 1), "trillion USD/year\n\n")
  
  cat("Cost Breakdown:\n")
  cat("Total cost:", round(result$total_cost, 1), "trillion $\n")
  cat("Mitigation cost:", round(result$mitig_cost, 1), "trillion $ (",
      round(100*result$mitig_cost/result$total_cost, 1), "%)\n")
  cat("CDR cost:", round(result$remov_cost, 1), "trillion $ (",
      round(100*result$remov_cost/result$total_cost, 1), "%)\n")
  cat("Temperature damage cost:", round(result$temp_cost, 1), "trillion $ (",
      round(100*result$temp_cost/result$total_cost, 1), "%)\n")
  
  return(result)
}

# Run the test
if (TRUE) {
  result_step6 <- test_step6()
}