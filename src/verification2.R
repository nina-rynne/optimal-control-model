#' KKT Verification Functions for Optimal Control Solutions
#' 
#' This script contains functions to verify that a solution satisfies the
#' Karush-Kuhn-Tucker (KKT) optimality conditions for the climate control problem
#' 
#' UPDATED FOR STRICT INEQUALITY FORMULATION: 0 <= u_m(t) < E(t)
#' This eliminates the need for μ₁ (upper bound multiplier for mitigation)

library(dplyr)
library(ggplot2)
library(gridExtra)

#' Main KKT Verification Function
#' 
#' @param result List containing the solution from shooting method (or other solver)
#' @param parameter_df DataFrame with model parameters
#' @param tolerance Numerical tolerance for violations (default: 1e-6)
#' @param verbose Whether to print detailed diagnostics (default: TRUE)
#' @param target_emissions Target cumulative emissions (default: 650)
#' @return List with verification results and diagnostics
verify_kkt_conditions <- function(result, parameter_df, tolerance = 1e-6, verbose = TRUE, target_emissions = 650) {
  
  if (verbose) {
    cat("=== KKT OPTIMALITY VERIFICATION ===\n")
    cat("Using STRICT INEQUALITY formulation: 0 <= u_m < E(t)\n")
    cat("Tolerance:", tolerance, "\n")
    cat("Target emissions:", target_emissions, "GtCO2\n\n")
  }
  
  # Extract solution data
  n_years <- result$n_years
  years <- result$years
  years_rel <- result$years_rel
  dt <- 1  # Assuming dt = 1 from your code
  
  # Controls and state
  um <- result$qty_mitig
  ur <- result$qty_remov
  c_traj <- result$cumulative_emissions
  lambda <- result$adjoint_var
  
  # Baseline data
  E_baseline <- result$baseline_annual_emissions
  temperature <- result$temperature_anomaly
  
  # Parameters
  cost_mitig_unit <- parameter_df$cost_mitig_unit
  cost_remov_unit <- parameter_df$cost_remov_unit
  exp_mitig <- parameter_df$exp_mitig
  exp_remov <- parameter_df$exp_remov
  exp_temp_anom <- parameter_df$exp_temp_anom
  clim_temp_init <- parameter_df$clim_temp_init
  tcre <- parameter_df$tcre
  econ_dam_pct <- parameter_df$econ_dam_pct
  disc_rate <- parameter_df$disc_rate
  
  # Extract epsilon if available
  epsilon <- if (!is.null(result$epsilon_used)) result$epsilon_used else 0.01
  
  # Discount factors
  discount_factors <- exp(-disc_rate * years_rel)
  
  # Initialize verification results
  verification <- list()
  
  # ===== 1. PRIMAL FEASIBILITY =====
  if (verbose) cat("1. Checking Primal Feasibility...\n")
  
  # Check bounds on mitigation: 0 ≤ um < E (STRICT INEQUALITY)
  mitig_lower_viol <- pmax(0, -um)  # Violations of um ≥ 0
  mitig_upper_viol <- pmax(0, um - (E_baseline - epsilon))  # Violations of um < E (allowing epsilon tolerance)
  
  # Check bounds on CDR: ur ≥ 0  
  cdr_lower_viol <- pmax(0, -ur)  # Violations of ur ≥ 0
  
  max_mitig_lower <- max(mitig_lower_viol)
  max_mitig_upper <- max(mitig_upper_viol)
  max_cdr_lower <- max(cdr_lower_viol)
  
  primal_feasible <- (max_mitig_lower <= tolerance) & 
    (max_mitig_upper <= tolerance) & 
    (max_cdr_lower <= tolerance)
  
  verification$primal_feasibility <- list(
    feasible = primal_feasible,
    max_mitig_lower_violation = max_mitig_lower,
    max_mitig_upper_violation = max_mitig_upper,
    max_cdr_lower_violation = max_cdr_lower,
    mitig_lower_violations = mitig_lower_viol,
    mitig_upper_violations = mitig_upper_viol,
    cdr_lower_violations = cdr_lower_viol,
    epsilon_used = epsilon
  )
  
  if (verbose) {
    cat("  Mitigation lower bound max violation:", round(max_mitig_lower, 8), "\n")
    cat("  Mitigation upper bound max violation (with epsilon =", epsilon, "):", round(max_mitig_upper, 8), "\n")
    cat("  CDR lower bound max violation:", round(max_cdr_lower, 8), "\n")
    cat("  Primal feasibility:", ifelse(primal_feasible, "PASS", "FAIL"), "\n\n")
  }
  
  # ===== 2. STATE EQUATION VERIFICATION (DISCRETE-TIME) =====
  if (verbose) cat("2. Checking State Equation (Discrete-Time)...\n")
  
  # UPDATED: First year starts from first year's net emissions, not zero
  # c[1] = (E[1] - um[1] - ur[1]) * dt
  # c[i] = c[i-1] + (E[i] - um[i] - ur[i]) * dt for i > 1
  
  # Calculate what cumulative emissions should be theoretically
  c_theoretical <- rep(0, n_years)
  for (i in 1:n_years) {
    annual_net <- E_baseline[i] - um[i] - ur[i]
    if (i == 1) {
      c_theoretical[i] <- annual_net * dt
    } else {
      c_theoretical[i] <- c_theoretical[i-1] + annual_net * dt
    }
  }
  
  # Check if state equation is satisfied
  state_eq_error <- abs(c_traj - c_theoretical)
  max_state_error <- max(state_eq_error)
  state_eq_satisfied <- max_state_error <= tolerance
  
  verification$state_equation <- list(
    satisfied = state_eq_satisfied,
    max_error = max_state_error,
    errors = state_eq_error,
    cumulative_actual = c_traj,
    cumulative_theoretical = c_theoretical
  )
  
  if (verbose) {
    cat("  State equation max error:", round(max_state_error, 8), "\n")
    cat("  Initial cumulative emissions (actual):", round(c_traj[1], 3), "\n")
    cat("  Initial cumulative emissions (theoretical):", round(c_theoretical[1], 3), "\n")
    cat("  State equation:", ifelse(state_eq_satisfied, "PASS", "FAIL"), "\n\n")
  }
  
  # ===== 3. ADJOINT EQUATION VERIFICATION =====
  if (verbose) cat("3. Checking Adjoint Equation...\n")
  
  # Calculate adjoint derivative numerically (backward difference for stability)
  lambda_dot_numerical <- c(-diff(lambda) / dt, 0)  # Backward difference
  
  # Calculate theoretical adjoint derivative: λ̇ = -sbγe^(-δt)(T₀ + γc)^(s-1)
  baseline_gwp <- result$baseline_annual_gwp
  
  lambda_dot_theoretical <- rep(0, n_years)
  for (i in 1:(n_years-1)) {  # Don't calculate for last point
    temp_base <- temperature[i]
    gwp_value <- baseline_gwp[i]
    discount_factor <- discount_factors[i]
    
    lambda_dot_theoretical[i] <- -(exp_temp_anom * gwp_value * econ_dam_pct * 
                                     (tcre / 1000) * discount_factor * 
                                     (temp_base^(exp_temp_anom - 1)))
  }
  
  # Check only interior points (not terminal)
  adjoint_eq_error <- abs(lambda_dot_numerical[1:(n_years-1)] - lambda_dot_theoretical[1:(n_years-1)])
  max_adjoint_error <- max(adjoint_eq_error)
  adjoint_eq_satisfied <- max_adjoint_error <= (tolerance * 1000)  # More lenient for adjoint
  
  verification$adjoint_equation <- list(
    satisfied = adjoint_eq_satisfied,
    max_error = max_adjoint_error,
    errors = adjoint_eq_error,
    lambda_dot_numerical = lambda_dot_numerical,
    lambda_dot_theoretical = lambda_dot_theoretical
  )
  
  if (verbose) {
    cat("  Adjoint equation max error:", round(max_adjoint_error, 6), "\n")
    cat("  Adjoint equation:", ifelse(adjoint_eq_satisfied, "PASS", "FAIL"), "\n\n")
  }
  
  # ===== 4. STATIONARITY CONDITIONS (MODIFIED FOR STRICT INEQUALITY) =====
  if (verbose) cat("4. Checking Stationarity Conditions...\n")
  
  # Calculate KKT multipliers based on active constraints
  # NOTE: NO μ₁ for strict inequality upper bound!
  mu2 <- rep(0, n_years)  # um ≥ 0 constraint  
  mu3 <- rep(0, n_years)  # ur ≥ 0 constraint
  
  # Identify active constraints
  mitig_at_lower <- (um <= tolerance)
  mitig_at_upper_strict <- (um >= E_baseline - epsilon - tolerance)  # Near the strict upper bound
  mitig_interior <- (!mitig_at_lower) & (!mitig_at_upper_strict)
  
  cdr_at_lower <- (ur <= tolerance)
  cdr_interior <- !cdr_at_lower
  
  # Stationarity for mitigation: qFm*um^(q-1)*e^(-δt) - λ + μ₂ = 0
  # NOTE: No μ₁ term because of strict inequality!
  stationarity_mitig_error <- rep(0, n_years)
  
  for (i in 1:n_years) {
    # Calculate marginal cost of mitigation
    if (um[i] > tolerance) {
      marginal_cost_mitig <- exp_mitig * cost_mitig_unit * (um[i]^(exp_mitig - 1)) * discount_factors[i]
    } else {
      marginal_cost_mitig <- 0  # Handle zero case
    }
    
    if (mitig_interior[i]) {
      # Interior solution: marginal cost = shadow price
      mu2[i] <- 0
      stationarity_mitig_error[i] <- marginal_cost_mitig - lambda[i]
      
    } else if (mitig_at_lower[i]) {
      # At lower bound: μ₂ > 0
      mu2[i] <- lambda[i] - marginal_cost_mitig  # Should be > 0
      stationarity_mitig_error[i] <- 0  # Automatically satisfied
      
    } else if (mitig_at_upper_strict[i]) {
      # Near strict upper bound: no KKT multiplier, just check if we want to go higher
      mu2[i] <- 0
      # The control is at its maximum feasible value
      # We check if the unconstrained optimal would be higher
      if (marginal_cost_mitig < lambda[i]) {
        # We want to go higher but can't due to strict inequality
        stationarity_mitig_error[i] <- 0  # This is acceptable
      } else {
        # Something is wrong - we're at the bound but don't want to be
        stationarity_mitig_error[i] <- marginal_cost_mitig - lambda[i]
      }
    }
  }
  
  # Stationarity for CDR: rFr*ur^(r-1)*e^(-δt) - λ + μ₃ = 0
  stationarity_cdr_error <- rep(0, n_years)
  
  for (i in 1:n_years) {
    # Calculate marginal cost of CDR
    if (ur[i] > tolerance) {
      marginal_cost_cdr <- exp_remov * cost_remov_unit * (ur[i]^(exp_remov - 1)) * discount_factors[i]
    } else {
      marginal_cost_cdr <- 0
    }
    
    if (cdr_interior[i]) {
      # Interior solution: marginal cost = shadow price
      mu3[i] <- 0
      stationarity_cdr_error[i] <- marginal_cost_cdr - lambda[i]
      
    } else if (cdr_at_lower[i]) {
      # At lower bound: μ₃ > 0
      mu3[i] <- lambda[i] - marginal_cost_cdr  # Should be > 0
      stationarity_cdr_error[i] <- 0  # Automatically satisfied
    }
  }
  
  max_stationarity_mitig_error <- max(abs(stationarity_mitig_error))
  max_stationarity_cdr_error <- max(abs(stationarity_cdr_error))
  
  stationarity_satisfied <- (max_stationarity_mitig_error <= tolerance) & 
    (max_stationarity_cdr_error <= tolerance)
  
  verification$stationarity <- list(
    satisfied = stationarity_satisfied,
    max_mitig_error = max_stationarity_mitig_error,
    max_cdr_error = max_stationarity_cdr_error,
    mitig_errors = stationarity_mitig_error,
    cdr_errors = stationarity_cdr_error,
    mu2 = mu2,
    mu3 = mu3,
    mitig_interior = mitig_interior,
    mitig_at_lower = mitig_at_lower,
    mitig_at_upper_strict = mitig_at_upper_strict,
    cdr_interior = cdr_interior,
    cdr_at_lower = cdr_at_lower
  )
  
  if (verbose) {
    cat("  Stationarity mitigation max error:", round(max_stationarity_mitig_error, 8), "\n")
    cat("  Stationarity CDR max error:", round(max_stationarity_cdr_error, 8), "\n")
    cat("  Stationarity conditions:", ifelse(stationarity_satisfied, "PASS", "FAIL"), "\n")
    
    # Report how many time periods are at strict upper bound
    n_at_upper <- sum(mitig_at_upper_strict)
    if (n_at_upper > 0) {
      cat("  Note:", n_at_upper, "time periods at strict upper bound for mitigation\n")
    }
    cat("\n")
  }
  
  # ===== 5. COMPLEMENTARY SLACKNESS (MODIFIED) =====
  if (verbose) cat("5. Checking Complementary Slackness...\n")
  
  # Only check: μ₂*um = 0, μ₃*ur = 0
  # NO CHECK for μ₁ since it doesn't exist with strict inequality
  comp_slack_2 <- abs(mu2 * um)
  comp_slack_3 <- abs(mu3 * ur)
  
  max_comp_slack_2 <- max(comp_slack_2)
  max_comp_slack_3 <- max(comp_slack_3)
  
  comp_slack_satisfied <- (max_comp_slack_2 <= tolerance) & 
    (max_comp_slack_3 <= tolerance)
  
  verification$complementary_slackness <- list(
    satisfied = comp_slack_satisfied,
    max_violation_2 = max_comp_slack_2,
    max_violation_3 = max_comp_slack_3,
    violations_2 = comp_slack_2,
    violations_3 = comp_slack_3
  )
  
  if (verbose) {
    cat("  Complementary slackness μ₂*um max violation:", round(max_comp_slack_2, 8), "\n")
    cat("  Complementary slackness μ₃*ur max violation:", round(max_comp_slack_3, 8), "\n")
    cat("  Complementary slackness:", ifelse(comp_slack_satisfied, "PASS", "FAIL"), "\n\n")
  }
  
  # ===== 6. DUAL FEASIBILITY (MODIFIED) =====
  if (verbose) cat("6. Checking Dual Feasibility...\n")
  
  # Only check μ₂ ≥ 0 and μ₃ ≥ 0 (no μ₁)
  dual_feasible_2 <- all(mu2 >= -tolerance)
  dual_feasible_3 <- all(mu3 >= -tolerance)
  dual_feasible <- dual_feasible_2 & dual_feasible_3
  
  min_mu2 <- min(mu2)
  min_mu3 <- min(mu3)
  
  negative_mu2_indices <- which(mu2 < -tolerance)
  negative_mu3_indices <- which(mu3 < -tolerance)
  
  verification$dual_feasibility <- list(
    feasible = dual_feasible,
    min_mu2 = min_mu2,
    min_mu3 = min_mu3,
    negative_mu2_indices = negative_mu2_indices,
    negative_mu3_indices = negative_mu3_indices,
    mu2 = mu2,
    mu3 = mu3
  )
  
  if (verbose) {
    cat("  Min μ₂:", round(min_mu2, 8), "\n")
    cat("  Min μ₃:", round(min_mu3, 8), "\n")
    cat("  Dual feasibility:", ifelse(dual_feasible, "PASS", "FAIL"), "\n\n")
  }
  
  # ===== 7. BOUNDARY CONDITION VERIFICATION =====
  if (verbose) cat("7. Checking Boundary Conditions...\n")
  
  # UPDATED: Initial condition is now the first year's net emissions
  initial_expected <- E_baseline[1] - um[1] - ur[1]
  initial_condition_error <- abs(c_traj[1] - initial_expected)
  
  # Terminal condition with target_emissions parameter
  terminal_condition_error <- abs(c_traj[n_years] - target_emissions)
  
  boundary_satisfied <- (initial_condition_error <= tolerance) & 
    (terminal_condition_error <= 1.0)  # Allow 1 GtCO2 tolerance for terminal condition
  
  verification$boundary_conditions <- list(
    satisfied = boundary_satisfied,
    initial_error = initial_condition_error,
    initial_expected = initial_expected,
    initial_actual = c_traj[1],
    terminal_error = terminal_condition_error,
    terminal_expected = target_emissions,
    terminal_actual = c_traj[n_years]
  )
  
  if (verbose) {
    cat("  Initial condition error:", round(initial_condition_error, 8), "\n")
    cat("    Expected:", round(initial_expected, 3), "Actual:", round(c_traj[1], 3), "\n")
    cat("  Terminal condition error:", round(terminal_condition_error, 3), "\n")
    cat("    Expected:", target_emissions, "Actual:", round(c_traj[n_years], 3), "\n")
    cat("  Boundary conditions:", ifelse(boundary_satisfied, "PASS", "FAIL"), "\n\n")
  }
  
  # ===== OVERALL ASSESSMENT =====
  overall_kkt_satisfied <- primal_feasible & state_eq_satisfied & adjoint_eq_satisfied & 
    stationarity_satisfied & comp_slack_satisfied & dual_feasible & 
    boundary_satisfied
  
  verification$overall <- list(
    kkt_satisfied = overall_kkt_satisfied,
    summary = list(
      primal_feasibility = primal_feasible,
      state_equation = state_eq_satisfied,
      adjoint_equation = adjoint_eq_satisfied,
      stationarity = stationarity_satisfied,
      complementary_slackness = comp_slack_satisfied,
      dual_feasibility = dual_feasible,
      boundary_conditions = boundary_satisfied
    )
  )
  
  if (verbose) {
    cat("=== OVERALL KKT VERIFICATION ===\n")
    cat("KKT Conditions Satisfied:", ifelse(overall_kkt_satisfied, "YES", "NO"), "\n")
    
    if (!overall_kkt_satisfied) {
      cat("\nFailed conditions:\n")
      if (!primal_feasible) cat("  - Primal Feasibility\n")
      if (!state_eq_satisfied) cat("  - State Equation\n")
      if (!adjoint_eq_satisfied) cat("  - Adjoint Equation\n")
      if (!stationarity_satisfied) cat("  - Stationarity\n")
      if (!comp_slack_satisfied) cat("  - Complementary Slackness\n")
      if (!dual_feasible) cat("  - Dual Feasibility\n")
      if (!boundary_satisfied) cat("  - Boundary Conditions\n")
    }
    cat("\n")
  }
  
  # Add metadata
  verification$metadata <- list(
    tolerance = tolerance,
    n_years = n_years,
    target_emissions = target_emissions,
    epsilon = epsilon,
    formulation = "strict_inequality",
    verification_time = Sys.time()
  )
  
  return(verification)
}

#' Create KKT Verification Plots (Updated for Strict Inequality)
#' 
#' @param result Solution result from shooting method
#' @param verification_result Result from verify_kkt_conditions()
#' @return List of ggplot objects
create_kkt_plots <- function(result, verification_result) {
  
  years <- result$years
  epsilon <- verification_result$metadata$epsilon
  
  # Plot 1: Control variables and constraints
  controls_df <- data.frame(
    Year = years,
    Mitigation = result$qty_mitig,
    CDR = result$qty_remov,
    Baseline_Emissions = result$baseline_annual_emissions,
    Strict_Upper_Bound = result$baseline_annual_emissions - epsilon
  )
  
  p1 <- ggplot(controls_df, aes(x = Year)) +
    geom_line(aes(y = Baseline_Emissions), color = "red", linetype = "dashed", 
              size = 1, alpha = 0.5) +
    geom_line(aes(y = Strict_Upper_Bound), color = "darkred", linetype = "solid", 
              size = 1, alpha = 0.7) +
    geom_line(aes(y = Mitigation), color = "blue", size = 1) +
    geom_line(aes(y = CDR), color = "green", size = 1) +
    labs(title = "Control Variables vs Constraints",
         subtitle = paste("Mitigation must stay strictly below baseline (ε =", epsilon, ")"),
         y = "GtCO2/year", x = "Year") +
    theme_minimal() +
    annotate("text", x = min(years) + 5, y = max(controls_df$Baseline_Emissions) * 0.95,
             label = "Baseline", color = "red") +
    annotate("text", x = min(years) + 5, y = max(controls_df$Strict_Upper_Bound) * 0.95,
             label = "Strict bound", color = "darkred") +
    annotate("text", x = min(years) + 5, y = max(controls_df$Mitigation) * 0.9,
             label = "Mitigation", color = "blue") +
    annotate("text", x = min(years) + 5, y = max(controls_df$CDR) * 1.1,
             label = "CDR", color = "green")
  
  # Plot 2: Shadow price (adjoint variable)
  shadow_df <- data.frame(
    Year = years,
    Shadow_Price = result$adjoint_var
  )
  
  p2 <- ggplot(shadow_df, aes(x = Year, y = Shadow_Price)) +
    geom_line(color = "purple", size = 1) +
    labs(title = "Shadow Price (λ) Over Time",
         subtitle = "Controls optimality via marginal cost = shadow price",
         y = "Shadow Price", x = "Year") +
    theme_minimal()
  
  # Plot 3: KKT violation heatmap (simplified for strict inequality)
  violations_df <- data.frame(
    Year = years,
    Primal_Mitig_Lower = verification_result$primal_feasibility$mitig_lower_violations,
    Primal_Mitig_Upper = verification_result$primal_feasibility$mitig_upper_violations,
    Primal_CDR_Lower = verification_result$primal_feasibility$cdr_lower_violations,
    State_Equation = verification_result$state_equation$errors,
    Stationarity_Mitig = abs(verification_result$stationarity$mitig_errors),
    Stationarity_CDR = abs(verification_result$stationarity$cdr_errors)
  )
  
  violations_long <- violations_df %>%
    tidyr::pivot_longer(-Year, names_to = "Condition", values_to = "Violation") %>%
    mutate(
      Condition = factor(Condition, levels = c(
        "Primal_Mitig_Lower", "Primal_Mitig_Upper", "Primal_CDR_Lower",
        "State_Equation", "Stationarity_Mitig", "Stationarity_CDR"
      )),
      Log_Violation = log10(pmax(Violation, 1e-12))  # Avoid log(0)
    )
  
  p3 <- ggplot(violations_long, aes(x = Year, y = Condition, fill = Log_Violation)) +
    geom_tile() +
    scale_fill_gradient2(low = "green", mid = "yellow", high = "red",
                         midpoint = -6, name = "log₁₀(violation)") +
    labs(title = "KKT Condition Violations Over Time",
         subtitle = "Green = satisfied, Red = violated",
         x = "Year", y = "KKT Condition") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
  
  # Plot 4: Active constraints over time (updated for strict inequality)
  active_df <- data.frame(
    Year = years,
    Mitig_At_Lower = verification_result$stationarity$mitig_at_lower,
    Mitig_Interior = verification_result$stationarity$mitig_interior,
    Mitig_At_Upper_Strict = verification_result$stationarity$mitig_at_upper_strict,
    CDR_At_Lower = verification_result$stationarity$cdr_at_lower,
    CDR_Interior = verification_result$stationarity$cdr_interior
  ) %>%
    mutate(
      Mitig_Status = case_when(
        Mitig_At_Lower ~ "At Lower Bound",
        Mitig_At_Upper_Strict ~ "At Strict Upper Bound", 
        Mitig_Interior ~ "Interior",
        TRUE ~ "Unknown"
      ),
      CDR_Status = case_when(
        CDR_At_Lower ~ "At Lower Bound",
        CDR_Interior ~ "Interior",
        TRUE ~ "Unknown"
      )
    )
  
  p4a <- ggplot(active_df, aes(x = Year, y = 1, fill = Mitig_Status)) +
    geom_tile(height = 0.8) +
    scale_fill_manual(values = c("At Lower Bound" = "red", 
                                 "Interior" = "green", 
                                 "At Strict Upper Bound" = "orange")) +
    labs(title = "Mitigation Constraint Status", 
         subtitle = "Strict inequality: no dual variable at upper bound",
         y = "", fill = "Status") +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  p4b <- ggplot(active_df, aes(x = Year, y = 1, fill = CDR_Status)) +
    geom_tile(height = 0.8) +
    scale_fill_manual(values = c("At Lower Bound" = "red", 
                                 "Interior" = "green")) +
    labs(title = "CDR Constraint Status", y = "", fill = "Status") +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  return(list(
    controls_constraints = p1,
    shadow_price = p2,
    violations_heatmap = p3,
    mitig_status = p4a,
    cdr_status = p4b
  ))
}

#' Generate KKT Verification Report
#' 
#' @param result Solution result from shooting method
#' @param parameter_df DataFrame with model parameters  
#' @param tolerance Numerical tolerance for violations
#' @param target_emissions Target cumulative emissions (default: 650)
#' @param create_plots Whether to generate diagnostic plots
#' @return List with verification results and optional plots
generate_kkt_report <- function(result, parameter_df, tolerance = 1e-6, 
                                target_emissions = 650, create_plots = TRUE) {
  
  cat("Generating KKT verification report for strict inequality formulation...\n\n")
  
  # Run verification
  verification <- verify_kkt_conditions(result, parameter_df, tolerance, 
                                        verbose = TRUE, target_emissions = target_emissions)
  
  # Create plots if requested
  if (create_plots) {
    plots <- create_kkt_plots(result, verification)
    verification$plots <- plots
  }
  
  return(verification)
}