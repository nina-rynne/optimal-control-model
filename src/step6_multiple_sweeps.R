# Step 6: Clean Parameter Sensitivity Analysis
# Core function only - takes parameter_df and runs sensitivity analysis

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
forward_backward_sweep_shooting <- function(parameter_df_row, emissions_df, economic_df, scenario, 
                                            terminal_adjoint) {
  
  # Filter data by scenario
  emissions_scenario <- emissions_df %>%
    filter(Scenario == scenario) %>%
    arrange(Year)
  
  economic_scenario <- economic_df %>%
    filter(Scenario == scenario) %>%
    arrange(Year)
  
  # Extract parameters from the single row
  cost_mitig_unit <- parameter_df_row$cost_mitig_unit
  cost_remov_unit <- parameter_df_row$cost_remov_unit
  exp_mitig <- parameter_df_row$exp_mitig
  exp_remov <- parameter_df_row$exp_remov
  exp_temp_anom <- parameter_df_row$exp_temp_anom
  clim_temp_init <- parameter_df_row$clim_temp_init
  tcre <- parameter_df_row$tcre
  econ_dam_pct <- parameter_df_row$econ_dam_pct
  disc_rate <- parameter_df_row$disc_rate
  
  # Extract target emissions from parameter row instead of function parameter
  target_emissions <- parameter_df_row$co2_target_2100
  
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
  max_iterations <- 500
  tolerance <- 0.01
  update_weight <- 0.05
  epsilon <- 0.01  # Strict inequality parameter
  
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
        annual_net <- baseline_emissions[i] - qty_mitig[i] - qty_remov[i]
        cumulative_emissions[i] <- 0 + annual_net * dt
      } else {
        annual_net <- baseline_emissions[i] - qty_mitig[i] - qty_remov[i]
        cumulative_emissions[i] <- cumulative_emissions[i-1] + annual_net * dt
      }
    }
    
    # Calculate temperature anomaly
    temperature_anomaly <- clim_temp_init + (cumulative_emissions / 1000) * tcre
    temperature_anomaly <- pmax(temperature_anomaly, 0.1)
    
    # Check for numerical issues
    if (any(!is.finite(temperature_anomaly))) {
      return(list(converged = FALSE, error = "Non-finite temperature"))
    }
    
    # === BACKWARD SWEEP ===
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
      
      # MITIGATION CONTROL with STRICT INEQUALITY
      mitig_denominator <- exp_mitig * cost_mitig_unit * discount_factor
      if (mitig_denominator > 1e-10 && adjoint_var[i] > 0) {
        um_unconstrained <- adjoint_var[i] / mitig_denominator
      } else {
        um_unconstrained <- 0
      }
      
      # Apply bounds with STRICT INEQUALITY at upper bound
      if (um_unconstrained <= 0) {
        new_mitig[i] <- 0
      } else if (um_unconstrained >= baseline_emissions[i] - epsilon) {
        new_mitig[i] <- baseline_emissions[i] - epsilon
      } else {
        new_mitig[i] <- um_unconstrained
      }
      
      # CDR CONTROL (only non-negativity constraint)
      remov_denominator <- exp_remov * cost_remov_unit * discount_factor
      if (remov_denominator > 1e-10 && adjoint_var[i] > 0) {
        ur_unconstrained <- adjoint_var[i] / remov_denominator
      } else {
        ur_unconstrained <- 0
      }
      
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
  
  # Calculate final results
  final_emissions <- cumulative_emissions[n_years]
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
    cumulative_emissions = cumulative_emissions,
    temperature_anomaly = temperature_anomaly,
    qty_mitig = qty_mitig,
    qty_remov = qty_remov,
    adjoint_var = adjoint_var,
    
    # Cost breakdown
    total_cost = total_cost,
    mitig_cost = total_mitig_cost,
    remov_cost = total_remov_cost,
    temp_cost = total_temp_cost,
    final_temperature = final_temperature,
    
    # Parameters for reference
    parameters = parameter_df_row
  ))
}

#' Shooting Method (for single parameter set)
real_data_shooting_method_single <- function(parameter_df_row, emissions_df, economic_df, scenario) {
  
  # Extract target emissions from parameter row
  target_emissions <- parameter_df_row$co2_target_2100
  
  # Shooting method parameters
  max_shooting_iterations <- 100
  shooting_tolerance <- 5.0
  
  # Initial bounds for terminal adjoint value - FIXED TO MATCH WORKING VERSION
  lambda_low <- 0
  lambda_high <- 5000  # Increased from 2000 to match working code
  
  # Test bounds
  result_low <- forward_backward_sweep_shooting(parameter_df_row, emissions_df, economic_df, 
                                                scenario, lambda_low)
  
  if (!result_low$converged) {
    return(list(converged = FALSE, error = "Lower bound failed"))
  }
  
  result_high <- forward_backward_sweep_shooting(parameter_df_row, emissions_df, economic_df, 
                                                 scenario, lambda_high)
  
  if (!result_high$converged) {
    return(list(converged = FALSE, error = "Upper bound failed"))
  }
  
  emission_gap_low <- result_low$emission_gap
  emission_gap_high <- result_high$emission_gap
  
  # Check if we have a valid bracket
  if (sign(emission_gap_low) == sign(emission_gap_high)) {
    # Try to find a better bracket
    if (emission_gap_low > 0 && emission_gap_high > 0) {
      lambda_low <- lambda_low / 2
      lambda_high <- lambda_low + 500
    } else if (emission_gap_low < 0 && emission_gap_high < 0) {
      lambda_high <- lambda_high * 2
      lambda_low <- lambda_high - 500
    }
  }
  
  # Main shooting method loop (secant method)
  best_result <- NULL
  best_gap <- Inf
  
  for (iteration in 1:max_shooting_iterations) {
    
    # Secant method update
    if (abs(emission_gap_high - emission_gap_low) < 1e-10) {
      break
    }
    
    # Calculate new lambda using secant method
    lambda_new <- lambda_low - emission_gap_low * (lambda_high - lambda_low) / 
      (emission_gap_high - emission_gap_low)
    
    # Evaluate at new point
    result_new <- forward_backward_sweep_shooting(parameter_df_row, emissions_df, economic_df, 
                                                  scenario, lambda_new)
    
    if (!result_new$converged) {
      lambda_new <- (lambda_low + lambda_high) / 2
      result_new <- forward_backward_sweep_shooting(parameter_df_row, emissions_df, economic_df, 
                                                    scenario, lambda_new)
    }
    
    if (!result_new$converged) {
      return(list(converged = FALSE, error = "Inner loop failed"))
    }
    
    emission_gap_new <- result_new$emission_gap
    
    # Track best result
    if (abs(emission_gap_new) < abs(best_gap)) {
      best_result <- result_new
      best_gap <- emission_gap_new
    }
    
    # Check convergence
    if (abs(emission_gap_new) <= shooting_tolerance) {
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
  
  # Return best result found
  return(best_result)
}

#' Main Parameter Sensitivity Analysis Function
run_parameter_sensitivity_analysis <- function(parameter_df, 
                                               emissions_df,
                                               economic_df,
                                               scenario = "SSP3-Baseline") {
  
  n_samples <- nrow(parameter_df)
  
  cat("=== Parameter Sensitivity Analysis ===\n")
  cat("Number of parameter samples:", n_samples, "\n")
  cat("Scenario:", scenario, "\n")
  cat("Target emissions: Using co2_target_2100 from parameter_df (varies by sample)\n\n")
  
  # Initialize storage for results
  successful_runs <- list()
  failed_runs <- list()
  run_counter <- 0
  
  cat("Starting sensitivity analysis runs...\n")
  
  # Progress tracking
  start_time <- Sys.time()
  
  # Loop through parameter samples
  for (i in 1:n_samples) {
    
    # Progress reporting
    if (i %% 50 == 0 || i == 1) {
      elapsed <- difftime(Sys.time(), start_time, units = "mins")
      cat("Sample", i, "of", n_samples, "- Elapsed:", round(elapsed, 1), "minutes\n")
    }
    
    # Extract current parameter set (single row)
    current_params <- parameter_df[i, ]
    
    # Try to solve the optimal control problem
    tryCatch({
      result <- real_data_shooting_method_single(current_params, emissions_df, economic_df, 
                                                 scenario)
      
      if (!is.null(result) && result$converged) {
        run_counter <- run_counter + 1
        run_name <- paste0("run_", run_counter)
        successful_runs[[run_name]] <- result
        
        if (i %% 50 == 0) {
          cat("  -> Successful runs so far:", run_counter, "\n")
        }
      } else {
        failed_runs[[paste0("failed_", i)]] <- list(
          sample_id = i,
          parameters = current_params,
          error = ifelse(is.null(result), "NULL result", result$error)
        )
      }
      
    }, error = function(e) {
      failed_runs[[paste0("failed_", i)]] <- list(
        sample_id = i,
        parameters = current_params,
        error = as.character(e)
      )
    })
  }
  
  total_time <- difftime(Sys.time(), start_time, units = "mins")
  
  cat("\n=== Sensitivity Analysis Complete ===\n")
  cat("Total time:", round(total_time, 1), "minutes\n")
  cat("Successful runs:", length(successful_runs), "out of", n_samples, 
      "(", round(100 * length(successful_runs) / n_samples, 1), "%)\n")
  cat("Failed runs:", length(failed_runs), "\n")
  
  # Return results structure
  results <- list(
    successful_runs = successful_runs,
    failed_runs = failed_runs,
    n_samples = n_samples,
    n_successful = length(successful_runs),
    n_failed = length(failed_runs),
    scenario = scenario,
    parameter_samples = parameter_df
  )
  
  return(results)
}

# Example usage:
# 
# # Load your data
# emissions_df <- interpolate_ssp_emissions(import_ssp_emissions("emissions.csv"), dt = 1, start_year = 2020, end_year = 2100)
# economic_df <- interpolate_ssp_economic(import_ssp_economic("gwp.csv"), dt = 1, start_year = 2020, end_year = 2100)
# parameter_df <- create_params_dataframe()  # Your existing function with multiple parameter samples
# 
# # Run sensitivity analysis
multi_results <- run_parameter_sensitivity_analysis(parameter_df, emissions_df, economic_df)
# 
# # Check results
# print(paste("Success rate:", round(100 * results$n_successful / results$n_samples, 1), "%"))

cat("=== Clean Parameter Sensitivity Analysis Ready ===\n")
cat("Core function: run_parameter_sensitivity_analysis(parameter_df, emissions_df, economic_df)\n")


# =============================================================================
# VISUALIZATION FUNCTIONS - Only the spaghetti plots needed for dashboard
# =============================================================================

#' Create Temperature Anomaly Spaghetti Plot
create_temperature_spaghetti_plot <- function(oc_solution, 
                                              color_base = "steelblue",
                                              alpha_background = 0.1,
                                              median_width = 0.7,
                                              bounds_width = 0.5) {
  # Extract and prepare data
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  temperature_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      temp_anomaly = map(run_list, "temperature_anomaly")
    ) %>%
    select(run_id, years, temp_anomaly) %>%
    unnest(cols = c(years, temp_anomaly))
  
  # Calculate summary statistics for each year
  temp_summary <- temperature_data %>%
    group_by(years) %>%
    summarize(
      min_val = min(temp_anomaly),
      max_val = max(temp_anomaly),
      median_val = median(temp_anomaly),
      .groups = "drop"
    )
  
  # Create the plot
  plot_temperature <- ggplot() +
    # Reference line at zero
    geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
    
    # Background lines (all individual runs faded)
    geom_line(data = temperature_data,
              aes(x = years, y = temp_anomaly, group = run_id),
              color = color_base, alpha = alpha_background, linewidth = 0.2) +
    
    # Min and max boundary lines (thinner than median)
    geom_line(data = temp_summary,
              aes(x = years, y = min_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    geom_line(data = temp_summary,
              aes(x = years, y = max_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    
    # Median line (thicker than boundary lines)
    geom_line(data = temp_summary,
              aes(x = years, y = median_val),
              color = colorspace::darken(color_base, 0.3), 
              alpha = 1.0, 
              linewidth = median_width) +
    
    # Clean, publication-ready theme
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      text = element_text(family = "serif")
    ) +
    labs(
      x = "Year",
      y = "Temperature Anomaly (°C)"
    )
  
  return(plot_temperature)
}

#' Create Cumulative Emissions Spaghetti Plot
create_emissions_spaghetti_plot <- function(oc_solution, 
                                            color_base = "firebrick",
                                            alpha_background = 0.1,
                                            median_width = 0.7,
                                            bounds_width = 0.5) {
  # Extract and prepare data
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  emissions_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      cum_emissions = map(run_list, "cumulative_emissions")
    ) %>%
    select(run_id, years, cum_emissions) %>%
    unnest(cols = c(years, cum_emissions))
  
  # Calculate summary statistics for each year
  emissions_summary <- emissions_data %>%
    group_by(years) %>%
    summarize(
      min_val = min(cum_emissions),
      max_val = max(cum_emissions),
      median_val = median(cum_emissions),
      .groups = "drop"
    )
  
  # Create the plot
  plot_emissions <- ggplot() +
    # Background lines (all individual runs faded)
    geom_line(data = emissions_data,
              aes(x = years, y = cum_emissions, group = run_id),
              color = color_base, alpha = alpha_background, linewidth = 0.2) +
    
    # Min and max boundary lines (thinner than median)
    geom_line(data = emissions_summary,
              aes(x = years, y = min_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    geom_line(data = emissions_summary,
              aes(x = years, y = max_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    
    # Median line (thicker than boundary lines)
    geom_line(data = emissions_summary,
              aes(x = years, y = median_val),
              color = colorspace::darken(color_base, 0.3), 
              alpha = 1.0, 
              linewidth = median_width) +
    
    # Clean, publication-ready theme
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      text = element_text(family = "serif")
    ) +
    labs(
      x = "Year",
      y = "Cumulative Emissions (GtCO2)"
    )
  
  return(plot_emissions)
}

#' Create Mitigation Quantity Spaghetti Plot
create_mitigation_spaghetti_plot <- function(oc_solution, 
                                             color_base = "forestgreen",
                                             alpha_background = 0.1,
                                             median_width = 0.7,
                                             bounds_width = 0.5) {
  # Extract and prepare data
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  mitigation_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      qty_mitig = map(run_list, "qty_mitig")
    ) %>%
    select(run_id, years, qty_mitig) %>%
    unnest(cols = c(years, qty_mitig))
  
  # Calculate summary statistics for each year
  mitig_summary <- mitigation_data %>%
    group_by(years) %>%
    summarize(
      min_val = min(qty_mitig),
      max_val = max(qty_mitig),
      median_val = median(qty_mitig),
      .groups = "drop"
    )
  
  # Create the plot
  plot_mitigation <- ggplot() +
    # Background lines (all individual runs faded)
    geom_line(data = mitigation_data,
              aes(x = years, y = qty_mitig, group = run_id),
              color = color_base, alpha = alpha_background, linewidth = 0.2) +
    
    # Min and max boundary lines (thinner than median)
    geom_line(data = mitig_summary,
              aes(x = years, y = min_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    geom_line(data = mitig_summary,
              aes(x = years, y = max_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    
    # Median line (thicker than boundary lines)
    geom_line(data = mitig_summary,
              aes(x = years, y = median_val),
              color = colorspace::darken(color_base, 0.3), 
              alpha = 1.0, 
              linewidth = median_width) +
    
    # Clean, publication-ready theme
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      text = element_text(family = "serif")
    ) +
    labs(
      x = "Year",
      y = "Mitigation Quantity (GtCO2/yr)"
    )
  
  return(plot_mitigation)
}

#' Create Carbon Dioxide Removal Spaghetti Plot
create_cdr_spaghetti_plot <- function(oc_solution, 
                                      color_base = "purple4",
                                      alpha_background = 0.1,
                                      median_width = 0.7,
                                      bounds_width = 0.5) {
  # Extract and prepare data
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  cdr_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      qty_remov = map(run_list, "qty_remov")
    ) %>%
    select(run_id, years, qty_remov) %>%
    unnest(cols = c(years, qty_remov))
  
  # Calculate summary statistics for each year
  cdr_summary <- cdr_data %>%
    group_by(years) %>%
    summarize(
      min_val = min(qty_remov),
      max_val = max(qty_remov),
      median_val = median(qty_remov),
      .groups = "drop"
    )
  
  # Create the plot
  plot_cdr <- ggplot() +
    # Background lines (all individual runs faded)
    geom_line(data = cdr_data,
              aes(x = years, y = qty_remov, group = run_id),
              color = color_base, alpha = alpha_background, linewidth = 0.2) +
    
    # Min and max boundary lines (thinner than median)
    geom_line(data = cdr_summary,
              aes(x = years, y = min_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    geom_line(data = cdr_summary,
              aes(x = years, y = max_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    
    # Median line (thicker than boundary lines)
    geom_line(data = cdr_summary,
              aes(x = years, y = median_val),
              color = colorspace::darken(color_base, 0.3), 
              alpha = 1.0, 
              linewidth = median_width) +
    
    # Clean, publication-ready theme
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      text = element_text(family = "serif")
    ) +
    labs(
      x = "Year",
      y = "Carbon Dioxide Removal (GtCO2/yr)"
    )
  
  return(plot_cdr)
}

#' Create Adjoint Variable Spaghetti Plot
create_adjoint_spaghetti_plot <- function(oc_solution, 
                                          color_base = "darkorange",
                                          alpha_background = 0.1,
                                          median_width = 0.7,
                                          bounds_width = 0.5) {
  # Extract and prepare data
  run_data <- tibble(
    run_id = names(oc_solution),
    run_list = oc_solution
  ) %>%
    filter(str_detect(run_id, "^run_"))
  
  adjoint_data <- run_data %>%
    mutate(
      years = map(run_list, "years"),
      adjoint_var = map(run_list, "adjoint_var")
    ) %>%
    select(run_id, years, adjoint_var) %>%
    unnest(cols = c(years, adjoint_var))
  
  # Calculate summary statistics for each year
  adjoint_summary <- adjoint_data %>%
    group_by(years) %>%
    summarize(
      min_val = min(adjoint_var),
      max_val = max(adjoint_var),
      median_val = median(adjoint_var),
      .groups = "drop"
    )
  
  # Create the plot
  plot_adjoint <- ggplot() +
    # Background lines (all individual runs faded)
    geom_line(data = adjoint_data,
              aes(x = years, y = adjoint_var, group = run_id),
              color = color_base, alpha = alpha_background, linewidth = 0.2) +
    
    # Min and max boundary lines (thinner than median)
    geom_line(data = adjoint_summary,
              aes(x = years, y = min_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    geom_line(data = adjoint_summary,
              aes(x = years, y = max_val),
              color = color_base, alpha = 0.3, linewidth = bounds_width) +
    
    # Median line (thicker than boundary lines)
    geom_line(data = adjoint_summary,
              aes(x = years, y = median_val),
              color = colorspace::darken(color_base, 0.3), 
              alpha = 1.0, 
              linewidth = median_width) +
    
    # Clean, publication-ready theme
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      text = element_text(family = "serif")
    ) +
    labs(
      x = "Year",
      y = "Adjoint Variable Value"
    )
  
  return(plot_adjoint)
}

#' Create Combined Model Output Dashboard
create_combined_dashboard <- function(oc_solution, 
                                      save_plot = TRUE,
                                      show_legend = FALSE,
                                      width = 190,
                                      height = 260,
                                      filename = NULL) {
  
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
  
  # Create the individual plots
  # 1. Emissions plot (top-left)
  p1 <- create_emissions_spaghetti_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Cumulative Emissions")
  
  # 2. Temperature plot (top-right)
  p2 <- create_temperature_spaghetti_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Temperature Anomaly")
  
  # 3. Mitigation plot (middle-left)
  p3 <- create_mitigation_spaghetti_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Mitigation Quantity")
  
  # 4. Carbon Dioxide Removal plot (middle-right)
  p4 <- create_cdr_spaghetti_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Carbon Dioxide Removal")
  
  # 5. Adjoint Variable plot (bottom-left)
  p5 <- create_adjoint_spaghetti_plot(oc_solution) +
    my_theme +
    theme(legend.position = "none") +
    labs(title = "Adjoint Variable")
  
  # 6. Create a blank plot for the bottom-right area
  p6 <- ggplot() + 
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA))
  
  # Combine all plots using patchwork
  combined_plot <- (p1 + p2) /
    (p3 + p4) /
    (p5 + p6)
  
  # Add annotation
  final_plot <- combined_plot + 
    patchwork::plot_annotation(
      theme = theme(plot.margin = margin(10, 0, 10, 0))
    )
  
  # Save the plot if requested
  if (save_plot) {
    # Generate filename if not provided
    if (is.null(filename)) {
      filename <- paste0("combined_plot_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".pdf")
    }
    
    # Create figs directory if it doesn't exist
    figs_dir <- here::here("figs")
    if (!dir.exists(figs_dir)) {
      dir.create(figs_dir, recursive = TRUE)
    }
    
    # Full path to save the file
    filepath <- file.path(figs_dir, filename)
    
    # Save using Cairo device for better quality
    ggsave(filepath,
           final_plot,
           width = width,
           height = height,
           units = "mm",
           device = cairo_pdf)
    
    cat("Dashboard saved to:", filepath, "\n")
  }
  
  return(final_plot)
}