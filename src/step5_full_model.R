#' Step 5: Full Model with Temperature Dynamics
#' Problem: min ∫[Fm*um² + Fr*ur² + b(T₀ + γc(t))²] e^(-δt) dt
#' Subject to: ċ = E(t) - um(t) - ur(t)
#'            0 ≤ um(t) ≤ E(t), ur(t) ≥ 0
#'            c(0) = 0, c(T) = target
#' This is your actual problem!

library(deSolve)
library(ggplot2)
library(dplyr)
library(here)
library(yaml)
library(tidyr)

# Load your existing functions
source(here::here("src", "model_parameters.R"))

#' Full Model Forward-Backward Sweep
full_model_sweep <- function(parameter_df, emissions_data, target_emissions = 600) {
  
  # Extract all parameters from your YAML
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
  
  # Time setup
  years <- seq(2020, 2100, by = 1)
  years_rel <- years - min(years)  # Relative years for discounting
  n_years <- length(years)
  dt <- 1
  
  # Baseline data
  baseline_emissions <- emissions_data$Value[1:n_years]
  
  # Create simple baseline GWP for testing (you'd normally load this)
  baseline_gwp <- rep(100, n_years)  # Trillion $ per year
  
  # Initialize variables
  cumulative_emissions <- rep(0, n_years)
  temperature_anomaly <- rep(clim_temp_init, n_years)
  qty_mitig <- rep(1, n_years)    
  qty_remov <- rep(0.1, n_years)  
  adjoint_var <- rep(0, n_years)
  
  # Algorithm parameters
  max_iterations <- 500
  tolerance <- 0.01
  update_weight <- 0.1  # Much more conservative for the full model
  
  cat("Starting full model with temperature dynamics...\n")
  cat("Initial temperature:", clim_temp_init, "°C\n")
  cat("TCRE:", tcre, "°C per 1000 GtCO2\n")
  cat("Discount rate:", disc_rate, "\n")
  cat("Economic damage:", econ_dam_pct * 100, "% of GWP\n\n")
  
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
    
    # Calculate temperature anomaly based on cumulative emissions
    temperature_anomaly <- clim_temp_init + (cumulative_emissions / 1000) * tcre
    
    # Ensure temperature stays positive for power calculations
    temperature_anomaly <- pmax(temperature_anomaly, 0.1)
    
    # === BACKWARD SWEEP (THE CRITICAL PART) ===
    # Terminal condition
    current_final <- cumulative_emissions[n_years]
    emission_error <- current_final - target_emissions
    
    # Initialize terminal adjoint (this would normally come from shooting method)
    if (iter == 1) {
      adjoint_var[n_years] <- cost_mitig_unit * emission_error / n_years
    }
    
    # Backward integration of adjoint equation
    for (i in (n_years-1):1) {
      j <- i  # Current time index
      
      # This is the key equation from your KKT formulation:
      # dλ/dt = -∂H/∂c = -sbγe^(-δt) * (T₀ + γc)^(s-1)
      # Where s = exp_temp_anom, b = baseline_gwp * econ_dam_pct, γ = tcre/1000
      
      temp_base <- temperature_anomaly[j]
      discount_factor <- exp(-disc_rate * years_rel[j])
      
      # Calculate adjoint derivative (this is from your original formulation)
      adjoint_derivative <- -(exp_temp_anom * baseline_gwp[j] * econ_dam_pct * 
                                (tcre / 1000) * discount_factor * 
                                (temp_base^(exp_temp_anom - 1)))
      
      # Check for numerical problems
      if (!is.finite(adjoint_derivative)) {
        cat("WARNING: Non-finite adjoint derivative at time", j, "\n")
        cat("Temperature:", temp_base, "Discount:", discount_factor, "\n")
        adjoint_derivative <- 0  # Fallback
      }
      
      # Backward Euler step
      adjoint_var[j] <- adjoint_var[j+1] - adjoint_derivative * dt
    }
    
    # === UPDATE CONTROLS WITH FULL COST STRUCTURE ===
    new_mitig <- rep(0, n_years)
    new_remov <- rep(0, n_years)
    
    for (i in 1:n_years) {
      
      discount_factor <- exp(-disc_rate * years_rel[i])
      
      # --- MITIGATION CONTROL ---
      # From ∂H/∂um = 2*Fm*um*e^(-δt) - λ = 0
      # Therefore: um = λ / (2*Fm*e^(-δt))
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
      # From ∂H/∂ur = 2*Fr*ur*e^(-δt) - λ = 0
      # Therefore: ur = λ / (2*Fr*e^(-δt))
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
    
    # Very conservative update for stability
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
    
    if (iter %% 50 == 0) {
      cat("Iteration", iter, ":\n")
      cat("  Emission gap:", round(emission_error, 3), "\n")
      cat("  Total change:", round(total_change, 6), "\n")
      cat("  Max temperature:", round(max(temperature_anomaly), 2), "°C\n")
      cat("  Adjoint range: [", round(min(adjoint_var), 2), ",", 
          round(max(adjoint_var), 2), "]\n\n")
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
    years = years,
    years_rel = years_rel,
    n_years = n_years,
    baseline_annual_emissions = baseline_emissions,
    cumulative_emissions = cumulative_emissions,
    temperature_anomaly = temperature_anomaly,
    qty_mitig = qty_mitig,
    qty_remov = qty_remov,
    adjoint_var = adjoint_var,
    
    # Cost components
    mitig_costs_annual = mitig_costs,
    remov_costs_annual = remov_costs,
    temp_costs_annual = temp_costs,
    
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

#' Test Full Model
test_step5 <- function() {
  
  # Load your actual parameters
  parameter_df <- create_params_dataframe()
  
  # Use realistic emissions scenario
  years <- seq(2020, 2100, by = 1)
  realistic_emissions <- data.frame(
    Year = years,
    Value = pmax(2, 15 - 0.1 * (years - 2020))  # Declining emissions
  )
  
  cat("=== Testing Step 5: Full Model ===\n")
  cat("Problem: Your complete optimal control formulation\n")
  cat("Includes: Temperature dynamics, exponential discounting, damage costs\n")
  cat("Target: 600 GtCO2 cumulative by 2100\n\n")
  
  # Solve full model
  result <- full_model_sweep(parameter_df, realistic_emissions, 600)
  
  cat("\nResults:\n")
  cat("Converged:", result$converged, "in", result$iterations, "iterations\n")
  cat("Final emissions:", round(result$final_emissions, 1), "GtCO2\n")
  cat("Final temperature:", round(result$final_temperature, 2), "°C\n")
  cat("Emission gap:", round(result$emission_gap, 3), "\n\n")
  
  cat("Cost Breakdown (discounted):\n")
  cat("Total cost:", round(result$total_cost, 1), "trillion $\n")
  cat("Mitigation cost:", round(result$mitig_cost, 1), "trillion $ (",
      round(100*result$mitig_cost/result$total_cost, 1), "%)\n")
  cat("CDR cost:", round(result$remov_cost, 1), "trillion $ (",
      round(100*result$remov_cost/result$total_cost, 1), "%)\n")
  cat("Temperature damage cost:", round(result$temp_cost, 1), "trillion $ (",
      round(100*result$temp_cost/result$total_cost, 1), "%)\n\n")
  
  cat("Control Summary:\n")
  cat("Avg mitigation:", round(mean(result$qty_mitig), 2), "GtCO2/year\n")
  cat("Avg CDR:", round(mean(result$qty_remov), 2), "GtCO2/year\n")
  cat("Peak temperature:", round(max(result$temperature_anomaly), 2), "°C\n")
  
  # Create comprehensive plots
  plot_data <- data.frame(
    Year = result$years,
    Baseline = result$baseline_annual_emissions,
    Mitigation = result$qty_mitig,
    CDR = result$qty_remov,
    Net_Emissions = result$baseline_annual_emissions - result$qty_mitig - result$qty_remov,
    Cumulative = result$cumulative_emissions,
    Temperature = result$temperature_anomaly,
    Adjoint = result$adjoint_var
  )
  
  # Control strategies
  p1 <- plot_data %>%
    select(Year, Mitigation, CDR) %>%
    pivot_longer(cols = -Year, names_to = "Control", values_to = "Value") %>%
    ggplot(aes(x = Year, y = Value, color = Control)) +
    geom_line(size = 1) +
    labs(title = "Step 5: Full Model Control Strategies",
         y = "Annual Rate (GtCO2/year)") +
    theme_minimal()
  
  # Temperature trajectory
  p2 <- ggplot(plot_data, aes(x = Year, y = Temperature)) +
    geom_line() +
    geom_hline(yintercept = 1.5, linetype = "dashed", color = "red") +
    labs(title = "Temperature Anomaly",
         subtitle = "Red line shows 1.5°C target",
         y = "Temperature Anomaly (°C)") +
    theme_minimal()
  
  # Adjoint variable (shadow price)
  p3 <- ggplot(plot_data, aes(x = Year, y = Adjoint)) +
    geom_line() +
    labs(title = "Adjoint Variable (Shadow Price)",
         y = "Lambda(t)") +
    theme_minimal()
  
  # Cumulative emissions
  p4 <- ggplot(plot_data, aes(x = Year, y = Cumulative)) +
    geom_line() +
    geom_hline(yintercept = 600, linetype = "dashed", color = "red") +
    labs(title = "Cumulative Emissions",
         y = "Cumulative Emissions (GtCO2)") +
    theme_minimal()
  
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  
  return(result)
}

# Run the test
if (TRUE) {
  result_step5 <- test_step5()
}