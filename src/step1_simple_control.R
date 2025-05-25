#' Step 1: Simple Optimal Control Implementation
#' Uses your existing parameter structure but solves simplified problem
#' 
#' Problem: min ∫[Fm*um²] dt
#' Subject to: ċ = E(t) - um(t), c(0) = 0, c(T) = target

library(deSolve)
library(ggplot2)
library(dplyr)
library(here)
library(yaml)

# Load your existing functions
source(here::here("src", "model_parameters.R"))

#' Simple Forward-Backward Sweep (Step 1)
#' Only mitigation control, no inequality constraints
simple_forward_backward_sweep <- function(parameter_df, emissions_data, target_emissions = 600) {
  
  # Extract parameters using your structure
  cost_mitig_unit <- parameter_df$cost_mitig_unit
  exp_mitig <- parameter_df$exp_mitig
  
  # Time setup (using 2020-2100, annual)
  years <- seq(2020, 2100, by = 1)
  n_years <- length(years)
  dt <- 1
  
  # Get baseline emissions (simplified - just take first scenario)
  baseline_emissions <- emissions_data$Value[1:n_years]
  
  # Initialize state and control variables
  cumulative_emissions <- rep(0, n_years)
  qty_mitig <- rep(0, n_years)
  adjoint_var <- rep(0, n_years)
  
  # Algorithm parameters
  max_iterations <- 1000
  tolerance <- 0.001
  update_weight <- 0.1  # Much higher than your original 0.01
  
  # Forward-Backward Sweep Loop
  for (iter in 1:max_iterations) {
    
    # Store previous values for convergence check
    prev_mitig <- qty_mitig
    prev_cumulative <- cumulative_emissions
    prev_adjoint <- adjoint_var
    
    # === FORWARD SWEEP ===
    cumulative_emissions[1] <- 0
    for (i in 2:n_years) {
      annual_net <- baseline_emissions[i-1] - qty_mitig[i-1]
      cumulative_emissions[i] <- cumulative_emissions[i-1] + annual_net * dt
    }
    
    # === BACKWARD SWEEP ===
    # For this simple problem, adjoint derivative is 0, so adjoint is constant
    # Use shooting method principle: find lambda that gives correct terminal condition
    
    # Calculate required lambda to hit terminal constraint exactly
    current_final <- cumulative_emissions[n_years]
    emission_error <- current_final - target_emissions
    
    # Adjust lambda based on current error
    if (iter == 1) {
      # Initial guess
      total_baseline <- sum(baseline_emissions)
      lambda_required <- 2 * cost_mitig_unit * (total_baseline - target_emissions) / n_years
    } else {
      # Adjust lambda to correct emission error
      # If emissions too high, need more mitigation (higher lambda)
      # If emissions too low, need less mitigation (lower lambda)
      lambda_adjustment <- 2 * cost_mitig_unit * emission_error / n_years
      lambda_required <- adjoint_var[1] + lambda_adjustment * 0.1  # Small adjustment
    }
    
    # Set constant adjoint
    adjoint_var[] <- lambda_required
    
    # === UPDATE CONTROLS ===
    # From optimality condition: ∂H/∂um = 2*Fm*um - λ = 0
    # Therefore: um = λ/(2*Fm)
    new_mitig <- adjoint_var / (2 * cost_mitig_unit)
    
    # Apply bounds (non-negative, can't exceed baseline)
    new_mitig <- pmax(new_mitig, 0)
    new_mitig <- pmin(new_mitig, baseline_emissions)
    
    # Smooth update
    qty_mitig <- update_weight * new_mitig + (1 - update_weight) * prev_mitig
    
    # === CHECK CONVERGENCE ===
    mitig_change <- sum(abs(qty_mitig - prev_mitig))
    cumulative_change <- sum(abs(cumulative_emissions - prev_cumulative))
    
    if (mitig_change < tolerance && cumulative_change < tolerance) {
      cat("Converged in", iter, "iterations\n")
      break
    }
    
    if (iter == max_iterations) {
      cat("Warning: Max iterations reached\n")
    }
  }
  
  # Calculate final results
  final_emissions <- cumulative_emissions[n_years]
  total_cost <- sum(cost_mitig_unit * qty_mitig^exp_mitig)
  
  # Return results in format similar to your existing structure
  return(list(
    years = years,
    n_years = n_years,
    baseline_annual_emissions = baseline_emissions,
    cumulative_emissions = cumulative_emissions,
    qty_mitig = qty_mitig,
    adjoint_var = adjoint_var,
    
    # Solution quality metrics
    final_emissions = final_emissions,
    target_emissions = target_emissions,
    emission_gap = final_emissions - target_emissions,
    total_cost = total_cost,
    iterations = iter,
    converged = (mitig_change < tolerance && cumulative_change < tolerance)
  ))
}

#' Analytical Solution for Verification
analytical_solution_step1 <- function(parameter_df, emissions_data, target_emissions = 600) {
  
  cost_mitig_unit <- parameter_df$cost_mitig_unit
  baseline_emissions <- emissions_data$Value[1:81]  # 2020-2100
  n_years <- length(baseline_emissions)
  
  # For linear-quadratic problem with quadratic costs:
  # Optimal lambda is constant and given by:
  lambda_opt <- 2 * cost_mitig_unit * (sum(baseline_emissions) - target_emissions) / n_years
  
  # Optimal control
  um_opt <- lambda_opt / (2 * cost_mitig_unit)
  
  # Final emissions check
  final_emissions <- sum(baseline_emissions - um_opt)
  
  return(list(
    lambda_optimal = lambda_opt,
    mitigation_optimal = um_opt,
    final_emissions = final_emissions,
    cost_optimal = cost_mitig_unit * um_opt^2 * n_years
  ))
}

#' Test the Simple Implementation
test_step1 <- function() {
  
  # Load your parameters
  parameter_df <- create_params_dataframe()
  
  # Create simple emissions data (constant for testing)
  simple_emissions <- data.frame(
    Year = seq(2020, 2100, by = 1),
    Value = rep(10, 81)  # 10 GtCO2/year constant
  )
  
  cat("=== Testing Step 1: Simple Control ===\n")
  cat("Problem: Single mitigation control, no constraints\n")
  cat("Target: 600 GtCO2 cumulative by 2100\n")
  cat("Baseline: 10 GtCO2/year = 810 total\n\n")
  
  # Analytical solution
  analytical <- analytical_solution_step1(parameter_df, simple_emissions, 600)
  cat("Analytical Solution:\n")
  cat("Optimal lambda:", round(analytical$lambda_optimal, 6), "\n")
  cat("Optimal mitigation:", round(analytical$mitigation_optimal, 3), "GtCO2/year\n")
  cat("Final emissions:", round(analytical$final_emissions, 1), "GtCO2\n")
  cat("Total cost:", round(analytical$cost_optimal, 3), "\n\n")
  
  # Numerical solution
  cat("Numerical Solution:\n")
  result <- simple_forward_backward_sweep(parameter_df, simple_emissions, 600)
  cat("Final emissions:", round(result$final_emissions, 1), "GtCO2\n")
  cat("Emission gap:", round(result$emission_gap, 3), "\n")
  cat("Average mitigation:", round(mean(result$qty_mitig), 3), "GtCO2/year\n")
  cat("Converged:", result$converged, "in", result$iterations, "iterations\n")
  
  # Create simple plot
  plot_data <- data.frame(
    Year = result$years,
    Baseline = result$baseline_annual_emissions,
    Mitigation = result$qty_mitig,
    Net_Emissions = result$baseline_annual_emissions - result$qty_mitig,
    Cumulative = result$cumulative_emissions
  )
  
  p1 <- ggplot(plot_data, aes(x = Year)) +
    geom_line(aes(y = Baseline, color = "Baseline")) +
    geom_line(aes(y = Mitigation, color = "Mitigation")) +
    geom_line(aes(y = Net_Emissions, color = "Net Emissions")) +
    labs(title = "Step 1: Simple Control Solution",
         y = "Annual Emissions (GtCO2/year)",
         color = "Series") +
    theme_minimal()
  
  p2 <- ggplot(plot_data, aes(x = Year, y = Cumulative)) +
    geom_line() +
    geom_hline(yintercept = 600, linetype = "dashed", color = "red") +
    labs(title = "Cumulative Emissions",
         y = "Cumulative Emissions (GtCO2)") +
    theme_minimal()
  
  print(p1)
  print(p2)
  
  return(result)
}

# Run the test
if (TRUE) {  # Set to FALSE to prevent auto-run
  result <- test_step1()
}