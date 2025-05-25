#' Step 3: Dual Control Problem - Mitigation + CDR
#' Problem: min ∫[Fm*um² + Fr*ur²] dt
#' Subject to: ċ = E(t) - um(t) - ur(t)
#'            0 ≤ um(t) ≤ E(t), ur(t) ≥ 0

library(deSolve)
library(ggplot2)
library(dplyr)
library(here)
library(yaml)
library(tidyr)

# Load your existing functions
source(here::here("src", "model_parameters.R"))

#' Dual Control Forward-Backward Sweep (Step 3)
dual_control_sweep <- function(parameter_df, emissions_data, target_emissions = 600) {
  
  # Extract parameters
  cost_mitig_unit <- parameter_df$cost_mitig_unit
  cost_remov_unit <- parameter_df$cost_remov_unit
  exp_mitig <- parameter_df$exp_mitig
  exp_remov <- parameter_df$exp_remov
  
  # Time setup
  years <- seq(2020, 2100, by = 1)
  n_years <- length(years)
  dt <- 1
  
  # Baseline emissions
  baseline_emissions <- emissions_data$Value[1:n_years]
  
  # Initialize variables
  cumulative_emissions <- rep(0, n_years)
  qty_mitig <- rep(1, n_years)    # Mitigation control
  qty_remov <- rep(0.5, n_years)  # CDR control (new!)
  adjoint_var <- rep(0, n_years)
  
  # Algorithm parameters
  max_iterations <- 500
  tolerance <- 0.01
  update_weight <- 0.3
  
  # Bounds for CDR
  remov_min <- 0
  remov_max <- 50  # Reasonable upper bound for CDR
  
  cat("Starting dual control optimization...\n")
  
  # Forward-Backward Sweep Loop
  for (iter in 1:max_iterations) {
    
    # Store previous values
    prev_mitig <- qty_mitig
    prev_remov <- qty_remov
    prev_cumulative <- cumulative_emissions
    
    # === FORWARD SWEEP ===
    cumulative_emissions[1] <- 0
    for (i in 2:n_years) {
      # Net emissions = baseline - mitigation - CDR
      annual_net <- baseline_emissions[i-1] - qty_mitig[i-1] - qty_remov[i-1]
      cumulative_emissions[i] <- cumulative_emissions[i-1] + annual_net * dt
    }
    
    # === BACKWARD SWEEP ===
    current_final <- cumulative_emissions[n_years]
    emission_error <- current_final - target_emissions
    
    if (iter == 1) {
      lambda_required <- cost_mitig_unit * emission_error / n_years
    } else {
      lambda_adjustment <- cost_mitig_unit * emission_error / n_years
      lambda_required <- adjoint_var[1] + lambda_adjustment * 0.1
    }
    
    adjoint_var[] <- lambda_required
    
    # === UPDATE CONTROLS ===
    new_mitig <- rep(0, n_years)
    new_remov <- rep(0, n_years)
    
    for (i in 1:n_years) {
      
      # --- MITIGATION CONTROL (with bounds) ---
      # From ∂H/∂um = 2*Fm*um - λ = 0 → um = λ/(2*Fm)
      um_unconstrained <- adjoint_var[i] / (2 * cost_mitig_unit)
      
      if (um_unconstrained <= 0) {
        new_mitig[i] <- 0
      } else if (um_unconstrained >= baseline_emissions[i]) {
        new_mitig[i] <- baseline_emissions[i]
      } else {
        new_mitig[i] <- um_unconstrained
      }
      
      # --- CDR CONTROL (with bounds) ---
      # From ∂H/∂ur = 2*Fr*ur - λ = 0 → ur = λ/(2*Fr)
      ur_unconstrained <- adjoint_var[i] / (2 * cost_remov_unit)
      
      if (ur_unconstrained <= remov_min) {
        new_remov[i] <- remov_min
      } else if (ur_unconstrained >= remov_max) {
        new_remov[i] <- remov_max
      } else {
        new_remov[i] <- ur_unconstrained
      }
    }
    
    # Smooth update
    qty_mitig <- update_weight * new_mitig + (1 - update_weight) * prev_mitig
    qty_remov <- update_weight * new_remov + (1 - update_weight) * prev_remov
    
    # === CHECK CONVERGENCE ===
    mitig_change <- sum(abs(qty_mitig - prev_mitig))
    remov_change <- sum(abs(qty_remov - prev_remov))
    cumulative_change <- sum(abs(cumulative_emissions - prev_cumulative))
    
    total_change <- mitig_change + remov_change + cumulative_change
    
    if (total_change < tolerance) {
      cat("Converged in", iter, "iterations\n")
      break
    }
    
    if (iter %% 50 == 0) {
      cat("Iteration", iter, "- Emission gap:", round(emission_error, 3), 
          "Total change:", round(total_change, 6), "\n")
    }
    
    if (iter == max_iterations) {
      cat("Warning: Max iterations reached\n")
    }
  }
  
  # === VERIFY KKT CONDITIONS ===
  mitig_kkt <- rep(0, n_years)
  remov_kkt <- rep(0, n_years)
  mitig_status <- rep("interior", n_years)
  remov_status <- rep("interior", n_years)
  
  for (i in 1:n_years) {
    
    # Mitigation KKT conditions
    mitig_stationarity <- 2 * cost_mitig_unit * qty_mitig[i] - adjoint_var[i]
    
    if (abs(qty_mitig[i]) < 1e-6) {
      mitig_status[i] <- "lower_bound"
      mitig_kkt[i] <- max(0, -mitig_stationarity)
    } else if (abs(qty_mitig[i] - baseline_emissions[i]) < 1e-6) {
      mitig_status[i] <- "upper_bound"
      mitig_kkt[i] <- max(0, mitig_stationarity)
    } else {
      mitig_status[i] <- "interior"
      mitig_kkt[i] <- abs(mitig_stationarity)
    }
    
    # CDR KKT conditions
    remov_stationarity <- 2 * cost_remov_unit * qty_remov[i] - adjoint_var[i]
    
    if (abs(qty_remov[i] - remov_min) < 1e-6) {
      remov_status[i] <- "lower_bound"
      remov_kkt[i] <- max(0, -remov_stationarity)
    } else if (abs(qty_remov[i] - remov_max) < 1e-6) {
      remov_status[i] <- "upper_bound"
      remov_kkt[i] <- max(0, remov_stationarity)
    } else {
      remov_status[i] <- "interior"
      remov_kkt[i] <- abs(remov_stationarity)
    }
  }
  
  # Calculate results
  final_emissions <- cumulative_emissions[n_years]
  total_mitig_cost <- sum(cost_mitig_unit * qty_mitig^exp_mitig)
  total_remov_cost <- sum(cost_remov_unit * qty_remov^exp_remov)
  total_cost <- total_mitig_cost + total_remov_cost
  
  return(list(
    years = years,
    n_years = n_years,
    baseline_annual_emissions = baseline_emissions,
    cumulative_emissions = cumulative_emissions,
    qty_mitig = qty_mitig,
    qty_remov = qty_remov,
    adjoint_var = adjoint_var,
    
    # KKT diagnostics
    mitig_kkt_violations = mitig_kkt,
    remov_kkt_violations = remov_kkt,
    mitig_status = mitig_status,
    remov_status = remov_status,
    combined_kkt_violations = pmax(mitig_kkt, remov_kkt),
    mean_kkt_violation = mean(pmax(mitig_kkt, remov_kkt)),
    max_kkt_violation = max(pmax(mitig_kkt, remov_kkt)),
    
    # Solution quality
    final_emissions = final_emissions,
    target_emissions = target_emissions,
    emission_gap = final_emissions - target_emissions,
    total_cost = total_cost,
    mitig_cost = total_mitig_cost,
    remov_cost = total_remov_cost,
    iterations = iter,
    converged = (total_change < tolerance)
  ))
}

#' Test Step 3 - Dual Control
test_step3 <- function() {
  
  # Load parameters
  parameter_df <- create_params_dataframe()
  
  # Use the same variable emissions as Step 2
  years <- seq(2020, 2100, by = 1)
  variable_emissions <- data.frame(
    Year = years,
    Value = pmax(2, 15 - 0.1 * (years - 2020) + 3 * sin(0.3 * (years - 2020)))
  )
  
  cat("=== Testing Step 3: Dual Control ===\n")
  cat("Problem: min ∫[Fm*um² + Fr*ur²] dt\n")
  cat("Constraints: 0 ≤ um ≤ E(t), 0 ≤ ur ≤ 50\n")
  cat("Target: 600 GtCO2 cumulative by 2100\n\n")
  
  # Solve dual control problem
  result <- dual_control_sweep(parameter_df, variable_emissions, 600)
  
  cat("\nResults:\n")
  cat("Final emissions:", round(result$final_emissions, 1), "GtCO2\n")
  cat("Emission gap:", round(result$emission_gap, 3), "\n")
  cat("Converged:", result$converged, "in", result$iterations, "iterations\n")
  cat("Mean KKT violation:", round(result$mean_kkt_violation, 6), "\n")
  cat("Max KKT violation:", round(result$max_kkt_violation, 6), "\n\n")
  
  # Cost breakdown
  cat("Cost Analysis:\n")
  cat("Total cost:", round(result$total_cost, 2), "\n")
  cat("Mitigation cost:", round(result$mitig_cost, 2), 
      "(", round(100*result$mitig_cost/result$total_cost, 1), "%)\n")
  cat("CDR cost:", round(result$remov_cost, 2), 
      "(", round(100*result$remov_cost/result$total_cost, 1), "%)\n\n")
  
  # Control summary
  cat("Control Summary:\n")
  cat("Avg mitigation:", round(mean(result$qty_mitig), 2), "GtCO2/year\n")
  cat("Avg CDR:", round(mean(result$qty_remov), 2), "GtCO2/year\n")
  cat("Max mitigation:", round(max(result$qty_mitig), 2), "GtCO2/year\n")
  cat("Max CDR:", round(max(result$qty_remov), 2), "GtCO2/year\n\n")
  
  # Constraint analysis
  mitig_bounds <- table(result$mitig_status)
  remov_bounds <- table(result$remov_status)
  
  cat("Mitigation Constraints:\n")
  print(mitig_bounds)
  cat("\nCDR Constraints:\n")
  print(remov_bounds)
  
  # Create comprehensive plots
  plot_data <- data.frame(
    Year = result$years,
    Baseline = result$baseline_annual_emissions,
    Mitigation = result$qty_mitig,
    CDR = result$qty_remov,
    Net_Emissions = result$baseline_annual_emissions - result$qty_mitig - result$qty_remov,
    Cumulative = result$cumulative_emissions,
    Mitig_Status = result$mitig_status,
    CDR_Status = result$remov_status,
    Mitig_KKT = result$mitig_kkt_violations,
    CDR_KKT = result$remov_kkt_violations
  )
  
  # Controls comparison
  p1 <- plot_data %>%
    select(Year, Baseline, Mitigation, CDR) %>%
    pivot_longer(cols = -Year, names_to = "Control", values_to = "Value") %>%
    ggplot(aes(x = Year, y = Value, color = Control)) +
    geom_line() +
    labs(title = "Step 3: Dual Control Solution",
         subtitle = "Mitigation vs CDR strategies",
         y = "Annual Rate (GtCO2/year)") +
    theme_minimal()
  
  # Net emissions
  p2 <- ggplot(plot_data, aes(x = Year)) +
    geom_line(aes(y = Baseline, color = "Baseline")) +
    geom_line(aes(y = Net_Emissions, color = "Net Emissions")) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    labs(title = "Net Emissions After Controls",
         y = "Annual Emissions (GtCO2/year)") +
    theme_minimal()
  
  # Cumulative with target
  p3 <- ggplot(plot_data, aes(x = Year, y = Cumulative)) +
    geom_line() +
    geom_hline(yintercept = 600, linetype = "dashed", color = "red") +
    labs(title = "Cumulative Emissions",
         y = "Cumulative Emissions (GtCO2)") +
    theme_minimal()
  
  # KKT violations
  kkt_plot_data <- plot_data %>%
    select(Year, Mitig_KKT, CDR_KKT) %>%
    pivot_longer(cols = -Year, names_to = "Control", values_to = "KKT_Violation")
  
  p4 <- ggplot(kkt_plot_data, aes(x = Year, y = KKT_Violation, color = Control)) +
    geom_point() +
    geom_hline(yintercept = 0.001, linetype = "dashed", color = "red") +
    labs(title = "KKT Condition Violations",
         subtitle = "Both controls should be below red line",
         y = "KKT Violation") +
    theme_minimal()
  
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  
  return(result)
}

# Run the test
if (TRUE) {
  result_step3 <- test_step3()
}