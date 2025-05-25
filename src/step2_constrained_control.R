#' Step 2: Optimal Control with Inequality Constraints
#' Adds the constraint: 0 ≤ um(t) ≤ E(t)
#' This introduces KKT multipliers and switching behavior

library(deSolve)
library(ggplot2)
library(dplyr)
library(here)
library(yaml)

# Load your existing functions
source(here::here("src", "model_parameters.R"))

#' Forward-Backward Sweep with Inequality Constraints (Step 2)
constrained_forward_backward_sweep <- function(parameter_df, emissions_data, target_emissions = 600) {
  
  # Extract parameters
  cost_mitig_unit <- parameter_df$cost_mitig_unit
  exp_mitig <- parameter_df$exp_mitig
  
  # Time setup
  years <- seq(2020, 2100, by = 1)
  n_years <- length(years)
  dt <- 1
  
  # Get baseline emissions - make this more interesting with variation
  baseline_emissions <- emissions_data$Value[1:n_years]
  
  # Initialize variables
  cumulative_emissions <- rep(0, n_years)
  qty_mitig <- rep(1, n_years)  # Start with small positive value
  adjoint_var <- rep(0, n_years)
  
  # Algorithm parameters
  max_iterations <- 500
  tolerance <- 0.01
  update_weight <- 0.3
  
  # Forward-Backward Sweep Loop
  for (iter in 1:max_iterations) {
    
    # Store previous values
    prev_mitig <- qty_mitig
    prev_cumulative <- cumulative_emissions
    
    # === FORWARD SWEEP ===
    cumulative_emissions[1] <- 0
    for (i in 2:n_years) {
      annual_net <- baseline_emissions[i-1] - qty_mitig[i-1]
      cumulative_emissions[i] <- cumulative_emissions[i-1] + annual_net * dt
    }
    
    # === BACKWARD SWEEP ===
    # Adjoint is still constant for this problem
    current_final <- cumulative_emissions[n_years]
    emission_error <- current_final - target_emissions
    
    if (iter == 1) {
      lambda_required <- 2 * cost_mitig_unit * emission_error / n_years
    } else {
      lambda_adjustment <- 2 * cost_mitig_unit * emission_error / n_years
      lambda_required <- adjoint_var[1] + lambda_adjustment * 0.1
    }
    
    adjoint_var[] <- lambda_required
    
    # === UPDATE CONTROLS WITH KKT CONDITIONS ===
    new_mitig <- rep(0, n_years)
    
    for (i in 1:n_years) {
      # Unconstrained optimum from stationarity condition
      # ∂H/∂um = 2*Fm*um - λ = 0 → um = λ/(2*Fm)
      um_unconstrained <- adjoint_var[i] / (2 * cost_mitig_unit)
      
      # Apply KKT conditions with bounds
      if (um_unconstrained <= 0) {
        # Would want negative mitigation - set to lower bound
        new_mitig[i] <- 0
        # KKT: μ2 > 0 (active lower bound constraint)
      } else if (um_unconstrained >= baseline_emissions[i]) {
        # Would exceed baseline emissions - set to upper bound  
        new_mitig[i] <- baseline_emissions[i]
        # KKT: μ1 > 0 (active upper bound constraint)
      } else {
        # Interior solution - unconstrained optimum is feasible
        new_mitig[i] <- um_unconstrained
        # KKT: μ1 = μ2 = 0 (no active constraints)
      }
    }
    
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
  
  # === VERIFY KKT CONDITIONS ===
  kkt_violations <- rep(0, n_years)
  bound_status <- rep("interior", n_years)
  
  for (i in 1:n_years) {
    # Calculate stationarity condition violation
    stationarity <- 2 * cost_mitig_unit * qty_mitig[i] - adjoint_var[i]
    
    if (abs(qty_mitig[i]) < 1e-6) {
      # At lower bound
      bound_status[i] <- "lower_bound"
      kkt_violations[i] <- max(0, -stationarity)  # Should be ≥ 0
    } else if (abs(qty_mitig[i] - baseline_emissions[i]) < 1e-6) {
      # At upper bound
      bound_status[i] <- "upper_bound" 
      kkt_violations[i] <- max(0, stationarity)   # Should be ≤ 0
    } else {
      # Interior point
      bound_status[i] <- "interior"
      kkt_violations[i] <- abs(stationarity)      # Should be = 0
    }
  }
  
  # Calculate results
  final_emissions <- cumulative_emissions[n_years]
  total_cost <- sum(cost_mitig_unit * qty_mitig^exp_mitig)
  
  return(list(
    years = years,
    n_years = n_years,
    baseline_annual_emissions = baseline_emissions,
    cumulative_emissions = cumulative_emissions,
    qty_mitig = qty_mitig,
    adjoint_var = adjoint_var,
    
    # KKT diagnostics
    kkt_violations = kkt_violations,
    bound_status = bound_status,
    mean_kkt_violation = mean(kkt_violations),
    max_kkt_violation = max(kkt_violations),
    
    # Solution quality
    final_emissions = final_emissions,
    target_emissions = target_emissions,
    emission_gap = final_emissions - target_emissions,
    total_cost = total_cost,
    iterations = iter,
    converged = (mitig_change < tolerance && cumulative_change < tolerance)
  ))
}

#' Test Step 2 with Variable Emissions
test_step2 <- function() {
  
  # Load parameters
  parameter_df <- create_params_dataframe()
  
  # Create more interesting emissions data with variation
  # This will force some controls to hit bounds
  years <- seq(2020, 2100, by = 1)
  variable_emissions <- data.frame(
    Year = years,
    Value = pmax(2, 15 - 0.1 * (years - 2020) + 3 * sin(0.3 * (years - 2020)))
  )
  
  cat("=== Testing Step 2: Constrained Control ===\n")
  cat("Problem: Single mitigation with bounds 0 ≤ um ≤ E(t)\n")
  cat("Target: 600 GtCO2 cumulative by 2100\n")
  cat("Baseline: Variable emissions 2-18 GtCO2/year\n\n")
  
  # Solve constrained problem
  result <- constrained_forward_backward_sweep(parameter_df, variable_emissions, 600)
  
  cat("Results:\n")
  cat("Final emissions:", round(result$final_emissions, 1), "GtCO2\n")
  cat("Emission gap:", round(result$emission_gap, 3), "\n")
  cat("Converged:", result$converged, "in", result$iterations, "iterations\n")
  cat("Mean KKT violation:", round(result$mean_kkt_violation, 6), "\n")
  cat("Max KKT violation:", round(result$max_kkt_violation, 6), "\n\n")
  
  # Analyze constraint activity
  bound_summary <- table(result$bound_status)
  cat("Constraint Activity:\n")
  cat("Interior points:", bound_summary["interior"], "\n")
  cat("At lower bound (um=0):", bound_summary["lower_bound"], "\n") 
  cat("At upper bound (um=E):", bound_summary["upper_bound"], "\n\n")
  
  # Create plots
  plot_data <- data.frame(
    Year = result$years,
    Baseline = result$baseline_annual_emissions,
    Mitigation = result$qty_mitig,
    Net_Emissions = result$baseline_annual_emissions - result$qty_mitig,
    Cumulative = result$cumulative_emissions,
    Bound_Status = result$bound_status,
    KKT_Violation = result$kkt_violations
  )
  
  # Control plot with constraint regions
  p1 <- ggplot(plot_data, aes(x = Year)) +
    geom_line(aes(y = Baseline, color = "Baseline")) +
    geom_line(aes(y = Mitigation, color = "Mitigation"), size = 1.2) +
    geom_point(aes(y = Mitigation, color = Bound_Status), size = 2) +
    labs(title = "Step 2: Constrained Control Solution",
         subtitle = "Points show constraint activity",
         y = "Annual Emissions (GtCO2/year)",
         color = "Series") +
    theme_minimal()
  
  # KKT violations
  p2 <- ggplot(plot_data, aes(x = Year, y = KKT_Violation, color = Bound_Status)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0.001, linetype = "dashed", color = "red") +
    labs(title = "KKT Condition Violations",
         subtitle = "Points below red line satisfy KKT conditions",
         y = "KKT Violation",
         color = "Constraint Status") +
    theme_minimal()
  
  # Cumulative emissions
  p3 <- ggplot(plot_data, aes(x = Year, y = Cumulative)) +
    geom_line() +
    geom_hline(yintercept = 600, linetype = "dashed", color = "red") +
    labs(title = "Cumulative Emissions",
         y = "Cumulative Emissions (GtCO2)") +
    theme_minimal()
  
  print(p1)
  print(p2) 
  print(p3)
  
  return(result)
}

# Run the test
if (TRUE) {
  result_step2 <- test_step2()
}