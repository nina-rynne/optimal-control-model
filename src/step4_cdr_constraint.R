#' Step 4: Add CDR Non-negativity Constraint
#' Problem: min ∫[Fm*um² + Fr*ur²] dt
#' Subject to: ċ = E(t) - um(t) - ur(t)
#'            0 ≤ um(t) ≤ E(t), ur(t) ≥ 0
#' This tests the constraint that causes issues in your full model

library(deSolve)
library(ggplot2)
library(dplyr)
library(here)
library(yaml)
library(tidyr)

# Load your existing functions
source(here::here("src", "model_parameters.R"))

#' Step 4: Dual Control with CDR Non-negativity
constrained_dual_control <- function(parameter_df, emissions_data, target_emissions = 600) {
  
  # Extract parameters
  cost_mitig_unit <- parameter_df$cost_mitig_unit
  cost_remov_unit <- parameter_df$cost_remov_unit
  exp_mitig <- parameter_df$exp_mitig
  exp_remov <- parameter_df$exp_remov
  
  # Time setup
  years <- seq(2020, 2100, by = 1)
  n_years <- length(years)
  dt <- 1
  
  # Make emissions more challenging to force some constraints
  baseline_emissions <- emissions_data$Value[1:n_years]
  
  # Initialize variables
  cumulative_emissions <- rep(0, n_years)
  qty_mitig <- rep(1, n_years)    
  qty_remov <- rep(0.1, n_years)  # Start small to test non-negativity
  adjoint_var <- rep(0, n_years)
  
  # Algorithm parameters
  max_iterations <- 1000
  tolerance <- 0.01
  update_weight <- 0.2  # Slightly more conservative
  
  cat("Starting constrained dual control optimization...\n")
  
  # Forward-Backward Sweep Loop
  for (iter in 1:max_iterations) {
    
    # Store previous values
    prev_mitig <- qty_mitig
    prev_remov <- qty_remov
    prev_cumulative <- cumulative_emissions
    
    # === FORWARD SWEEP ===
    cumulative_emissions[1] <- 0
    for (i in 2:n_years) {
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
    
    # === UPDATE CONTROLS WITH EXACT KKT CONDITIONS ===
    new_mitig <- rep(0, n_years)
    new_remov <- rep(0, n_years)
    
    for (i in 1:n_years) {
      
      # --- MITIGATION CONTROL ---
      # From ∂H/∂um = 2*Fm*um - λ = 0 → um = λ/(2*Fm)
      um_unconstrained <- adjoint_var[i] / (2 * cost_mitig_unit)
      
      if (um_unconstrained <= 0) {
        new_mitig[i] <- 0
        # μ2 > 0 (lower bound multiplier active)
      } else if (um_unconstrained >= baseline_emissions[i]) {
        new_mitig[i] <- baseline_emissions[i] 
        # μ1 > 0 (upper bound multiplier active)
      } else {
        new_mitig[i] <- um_unconstrained
        # μ1 = μ2 = 0 (interior solution)  
      }
      
      # --- CDR CONTROL (CRITICAL: Only non-negativity constraint) ---
      # From ∂H/∂ur = 2*Fr*ur - λ = 0 → ur = λ/(2*Fr)
      ur_unconstrained <- adjoint_var[i] / (2 * cost_remov_unit)
      
      if (ur_unconstrained <= 0) {
        # This is the key case that causes problems in your full model!
        new_remov[i] <- 0
        # μ3 > 0 (non-negativity multiplier active)
        # The complementary slackness condition: μ3 * ur = 0 is satisfied
      } else {
        # No upper bound on CDR, so unconstrained optimum is used
        new_remov[i] <- ur_unconstrained
        # μ3 = 0 (no active constraint)
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
    
    if (iter %% 100 == 0) {
      cat("Iteration", iter, "- Emission gap:", round(emission_error, 3), 
          "Total change:", round(total_change, 6), "\n")
    }
    
    if (iter == max_iterations) {
      cat("Warning: Max iterations reached. Total change:", round(total_change, 6), "\n")
    }
  }
  
  # === COMPREHENSIVE KKT VERIFICATION ===
  mitig_kkt <- rep(0, n_years)
  remov_kkt <- rep(0, n_years)
  mitig_status <- rep("interior", n_years)
  remov_status <- rep("interior", n_years)
  
  # KKT multipliers (for diagnostic purposes)
  mu1 <- rep(0, n_years)  # um ≤ E(t) multiplier  
  mu2 <- rep(0, n_years)  # um ≥ 0 multiplier
  mu3 <- rep(0, n_years)  # ur ≥ 0 multiplier
  
  for (i in 1:n_years) {
    
    # Mitigation KKT analysis
    mitig_stationarity <- 2 * cost_mitig_unit * qty_mitig[i] - adjoint_var[i]
    
    if (abs(qty_mitig[i]) < 1e-8) {
      mitig_status[i] <- "at_zero"
      mu2[i] <- -mitig_stationarity  # Should be ≥ 0
      mitig_kkt[i] <- max(0, -mu2[i])  # Violation if μ2 < 0
    } else if (abs(qty_mitig[i] - baseline_emissions[i]) < 1e-8) {
      mitig_status[i] <- "at_maximum"
      mu1[i] <- mitig_stationarity   # Should be ≥ 0
      mitig_kkt[i] <- max(0, -mu1[i])  # Violation if μ1 < 0
    } else {
      mitig_status[i] <- "interior"
      mitig_kkt[i] <- abs(mitig_stationarity)  # Should be = 0
    }
    
    # CDR KKT analysis (MOST IMPORTANT FOR YOUR PROBLEM)
    remov_stationarity <- 2 * cost_remov_unit * qty_remov[i] - adjoint_var[i]
    
    if (abs(qty_remov[i]) < 1e-8) {
      remov_status[i] <- "at_zero"
      mu3[i] <- -remov_stationarity  # Should be ≥ 0 for feasibility
      remov_kkt[i] <- max(0, -mu3[i])  # Violation if μ3 < 0
    } else {
      remov_status[i] <- "interior"  
      remov_kkt[i] <- abs(remov_stationarity)  # Should be = 0
    }
  }
  
  # Calculate final metrics
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
    
    # Detailed KKT diagnostics
    mitig_kkt_violations = mitig_kkt,
    remov_kkt_violations = remov_kkt,
    mitig_status = mitig_status,
    remov_status = remov_status,
    
    # KKT multipliers (for advanced diagnostics)
    mu1_multipliers = mu1,
    mu2_multipliers = mu2, 
    mu3_multipliers = mu3,
    
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

#' Test Step 4 with Challenging Scenario
test_step4 <- function() {
  
  # Load parameters
  parameter_df <- create_params_dataframe()
  
  # Create a more challenging emissions scenario that might force CDR to zero
  years <- seq(2020, 2100, by = 1)
  
  # Decreasing emissions over time - this might make CDR unnecessary early on
  challenging_emissions <- data.frame(
    Year = years,
    Value = pmax(1, 20 - 0.2 * (years - 2020))  # Decreases from 20 to 4 GtCO2/year
  )
  
  cat("=== Testing Step 4: CDR Non-negativity Constraint ===\n")
  cat("Problem: min ∫[Fm*um² + Fr*ur²] dt\n")  
  cat("Constraints: 0 ≤ um ≤ E(t), ur ≥ 0 (no upper bound on CDR)\n")
  cat("Target: 600 GtCO2 cumulative by 2100\n")
  cat("Baseline: Decreasing emissions 20→4 GtCO2/year\n\n")
  
  # Solve constrained problem
  result <- constrained_dual_control(parameter_df, challenging_emissions, 600)
  
  cat("\nResults:\n")
  cat("Final emissions:", round(result$final_emissions, 1), "GtCO2\n")
  cat("Emission gap:", round(result$emission_gap, 3), "\n")
  cat("Converged:", result$converged, "in", result$iterations, "iterations\n")
  cat("Mean KKT violation:", round(result$mean_kkt_violation, 6), "\n")
  cat("Max KKT violation:", round(result$max_kkt_violation, 6), "\n\n")
  
  # Detailed constraint analysis
  mitig_bounds <- table(result$mitig_status)
  remov_bounds <- table(result$remov_status)
  
  cat("Mitigation Constraint Status:\n")
  print(mitig_bounds)
  cat("\nCDR Constraint Status:\n")
  print(remov_bounds)
  
  # Count periods where CDR = 0 (this is what causes problems!)
  zero_cdr_periods <- sum(result$qty_remov < 1e-6)
  cat("\nCDR at zero for", zero_cdr_periods, "out of", result$n_years, "time periods\n")
  
  if (zero_cdr_periods > 0) {
    cat("This tests the complementary slackness condition that causes issues!\n")
  }
  
  # Cost analysis
  cat("\nCost Analysis:\n")
  cat("Total cost:", round(result$total_cost, 2), "\n")
  cat("Mitigation cost:", round(result$mitig_cost, 2), 
      "(", round(100*result$mitig_cost/result$total_cost, 1), "%)\n")
  cat("CDR cost:", round(result$remov_cost, 2), 
      "(", round(100*result$remov_cost/result$total_cost, 1), "%)\n")
  
  # Create diagnostic plots
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
  
  # Controls with constraint status
  p1 <- plot_data %>%
    select(Year, Mitigation, CDR, CDR_Status) %>%
    pivot_longer(cols = c(Mitigation, CDR), names_to = "Control", values_to = "Value") %>%
    ggplot(aes(x = Year, y = Value, color = Control)) +
    geom_line(size = 1) +
    geom_point(data = filter(plot_data, CDR_Status == "at_zero"), 
               aes(x = Year, y = CDR), color = "red", size = 2) +
    labs(title = "Step 4: CDR Non-negativity Constraint",
         subtitle = "Red points show where CDR = 0 (constraint active)",
         y = "Annual Rate (GtCO2/year)") +
    theme_minimal()
  
  # Focus on CDR behavior
  p2 <- ggplot(plot_data, aes(x = Year)) +
    geom_line(aes(y = CDR, color = "CDR Rate")) +
    geom_point(aes(y = CDR, color = CDR_Status), size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    labs(title = "CDR Control Behavior",
         subtitle = "Shows periods where non-negativity constraint is active",
         y = "CDR Rate (GtCO2/year)") +
    theme_minimal()
  
  # KKT violations by constraint type
  kkt_plot_data <- plot_data %>%
    select(Year, Mitig_KKT, CDR_KKT, CDR_Status) %>%
    pivot_longer(cols = c(Mitig_KKT, CDR_KKT), names_to = "Control", values_to = "KKT_Violation")
  
  p3 <- ggplot(kkt_plot_data, aes(x = Year, y = KKT_Violation, color = Control)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0.001, linetype = "dashed", color = "red") +
    labs(title = "KKT Violations with CDR Constraint",
         subtitle = "Tests complementary slackness when CDR = 0",
         y = "KKT Violation") +
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
  result_step4 <- test_step4()
}