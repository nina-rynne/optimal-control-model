#' Delayed Deployment Analysis for Optimal Control Problem
#' Iterates through all combinations of mitigation and CDR deployment delays
#' and creates a heatmap of peak temperature anomalies

library(deSolve)
library(ggplot2)
library(dplyr)
library(here)
library(yaml)
library(tidyr)
library(viridis)

# Load your existing functions (assuming they're already sourced)
# source(here::here("src", "model_parameters.R"))
# source(here::here("src", "data_preparation.R"))

#' Modified Forward-Backward Sweep Function with Delayed Deployment
#' 
#' @param parameter_df Parameter dataframe
#' @param emissions_df Emissions dataframe  
#' @param economic_df Economic dataframe
#' @param scenario Scenario name
#' @param target_emissions Target cumulative emissions
#' @param terminal_adjoint Terminal adjoint value from shooting method
#' @param mitigation_delay_years Years to delay mitigation deployment (0-80)
#' @param cdr_delay_years Years to delay CDR deployment (0-80)
forward_backward_sweep_delayed <- function(parameter_df, emissions_df, economic_df, scenario, 
                                           target_emissions, terminal_adjoint,
                                           mitigation_delay_years = 0, cdr_delay_years = 0) {
  
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
  
  # Calculate deployment start years
  mitigation_start_year <- min(years) + mitigation_delay_years
  cdr_start_year <- min(years) + cdr_delay_years
  
  # Initialize variables
  cumulative_emissions <- rep(0, n_years)
  temperature_anomaly <- rep(clim_temp_init, n_years)
  qty_mitig <- rep(0, n_years)    # Start at zero (delayed deployment)
  qty_remov <- rep(0, n_years)    # Start at zero (delayed deployment)
  adjoint_var <- rep(0, n_years)
  
  # Set terminal condition from shooting method
  adjoint_var[n_years] <- terminal_adjoint
  
  # Algorithm parameters
  max_iterations <- 500
  tolerance <- 0.001
  update_weight <- 0.05
  
  # STRICT INEQUALITY PARAMETER
  epsilon <- 0.01
  
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
    
    # Calculate temperature anomaly based on cumulative emissions
    temperature_anomaly <- clim_temp_init + (cumulative_emissions / 1000) * tcre
    temperature_anomaly <- pmax(temperature_anomaly, 0.1)
    
    if (any(!is.finite(temperature_anomaly))) {
      break
    }
    
    # === BACKWARD SWEEP ===
    for (i in (n_years-1):1) {
      j <- i  
      
      temp_base <- temperature_anomaly[j]
      discount_factor <- exp(-disc_rate * years_rel[j])
      gwp_value <- baseline_gwp[j]
      
      adjoint_derivative <- -(exp_temp_anom * gwp_value * econ_dam_pct * 
                                (tcre / 1000) * discount_factor * 
                                (temp_base^(exp_temp_anom - 1)))
      
      if (!is.finite(adjoint_derivative)) {
        adjoint_derivative <- 0
      }
      
      adjoint_var[j] <- adjoint_var[j+1] - adjoint_derivative * dt
    }
    
    # === UPDATE CONTROLS WITH DELAYED DEPLOYMENT ===
    new_mitig <- rep(0, n_years)
    new_remov <- rep(0, n_years)
    
    for (i in 1:n_years) {
      
      current_year <- years[i]
      discount_factor <- exp(-disc_rate * years_rel[i])
      
      # MITIGATION CONTROL - only active after deployment date
      if (current_year >= mitigation_start_year) {
        mitig_denominator <- exp_mitig * cost_mitig_unit * discount_factor
        if (mitig_denominator > 1e-10 && adjoint_var[i] > 0) {
          um_unconstrained <- adjoint_var[i] / mitig_denominator
        } else {
          um_unconstrained <- 0
        }
        
        # Apply bounds with strict inequality
        if (um_unconstrained <= 0) {
          new_mitig[i] <- 0
        } else if (um_unconstrained >= baseline_emissions[i] - epsilon) {
          new_mitig[i] <- baseline_emissions[i] - epsilon
        } else {
          new_mitig[i] <- um_unconstrained
        }
      } else {
        # Before deployment date - mitigation forced to zero
        new_mitig[i] <- 0
      }
      
      # CDR CONTROL - only active after deployment date
      if (current_year >= cdr_start_year) {
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
      } else {
        # Before deployment date - CDR forced to zero
        new_remov[i] <- 0
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
  
  # Calculate peak temperature anomaly
  peak_temperature <- max(temperature_anomaly)
  
  # Final calculations
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
    peak_temperature = peak_temperature,  # NEW: Peak temperature
    
    # Delay information
    mitigation_delay_years = mitigation_delay_years,
    cdr_delay_years = cdr_delay_years,
    mitigation_start_year = mitigation_start_year,
    cdr_start_year = cdr_start_year,
    
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
    
    # Cost breakdown
    total_cost = total_cost,
    mitig_cost = total_mitig_cost,
    remov_cost = total_remov_cost,
    temp_cost = total_temp_cost,
    final_temperature = final_temperature
  ))
}

#' Modified Shooting Method for Delayed Deployment
delayed_shooting_method <- function(parameter_df, emissions_df, economic_df, scenario, 
                                    target_emissions = 600, 
                                    mitigation_delay_years = 0, cdr_delay_years = 0,
                                    max_shooting_iterations = 100, shooting_tolerance = 2.0) {
  
  # Shooting method parameters - reduced for efficiency in batch mode
  lambda_low <- 0
  lambda_high <- 5000
  
  # Test bounds
  result_low <- forward_backward_sweep_delayed(parameter_df, emissions_df, economic_df, 
                                               scenario, target_emissions, lambda_low,
                                               mitigation_delay_years, cdr_delay_years)
  
  result_high <- forward_backward_sweep_delayed(parameter_df, emissions_df, economic_df, 
                                                scenario, target_emissions, lambda_high,
                                                mitigation_delay_years, cdr_delay_years)
  
  if (!result_low$converged || !result_high$converged) {
    # Return best available result if convergence issues
    if (abs(result_low$emission_gap) < abs(result_high$emission_gap)) {
      return(result_low)
    } else {
      return(result_high)
    }
  }
  
  emission_gap_low <- result_low$emission_gap
  emission_gap_high <- result_high$emission_gap
  
  # Check for valid bracket
  if (sign(emission_gap_low) == sign(emission_gap_high)) {
    # Adjust bounds if needed
    if (emission_gap_low > 0 && emission_gap_high > 0) {
      lambda_low <- lambda_low / 2
      lambda_high <- lambda_low + 1000
    } else if (emission_gap_low < 0 && emission_gap_high < 0) {
      lambda_high <- lambda_high * 2
      lambda_low <- lambda_high - 1000
    }
  }
  
  # Main shooting loop
  best_result <- NULL
  best_gap <- Inf
  
  for (iteration in 1:max_shooting_iterations) {
    
    # Secant method update
    if (abs(emission_gap_high - emission_gap_low) < 1e-10) {
      break
    }
    
    lambda_new <- lambda_low - emission_gap_low * (lambda_high - lambda_low) / 
      (emission_gap_high - emission_gap_low)
    
    result_new <- forward_backward_sweep_delayed(parameter_df, emissions_df, economic_df, 
                                                 scenario, target_emissions, lambda_new,
                                                 mitigation_delay_years, cdr_delay_years)
    
    if (!result_new$converged) {
      lambda_new <- (lambda_low + lambda_high) / 2
      result_new <- forward_backward_sweep_delayed(parameter_df, emissions_df, economic_df, 
                                                   scenario, target_emissions, lambda_new,
                                                   mitigation_delay_years, cdr_delay_years)
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
    
    # Update bounds
    if (sign(emission_gap_new) == sign(emission_gap_low)) {
      lambda_low <- lambda_new
      emission_gap_low <- emission_gap_new
    } else {
      lambda_high <- lambda_new  
      emission_gap_high <- emission_gap_new
    }
  }
  
  return(best_result)
}

#' Run Full Delayed Deployment Analysis
#' Iterates through all combinations of delay years and captures results
#' Uses existing dataframes: parameter_df, emissions_df, economic_df
run_delayed_deployment_analysis <- function(parameter_df, emissions_df, economic_df,
                                            scenario = "SSP3-Baseline", 
                                            target_emissions = 650,
                                            max_delay_years = 80,
                                            step_size = 5) {
  
  cat("=== DELAYED DEPLOYMENT ANALYSIS ===\n")
  cat("Scenario:", scenario, "\n")
  cat("Target emissions:", target_emissions, "GtCO2\n")
  cat("Maximum delay:", max_delay_years, "years\n")
  cat("Step size:", step_size, "years\n\n")
  
  # Check that required dataframes exist and have expected structure
  if (!exists("parameter_df") || !is.data.frame(parameter_df)) {
    stop("parameter_df not found or not a dataframe")
  }
  if (!exists("emissions_df") || !is.data.frame(emissions_df)) {
    stop("emissions_df not found or not a dataframe")
  }
  if (!exists("economic_df") || !is.data.frame(economic_df)) {
    stop("economic_df not found or not a dataframe")
  }
  
  cat("Using existing dataframes - no data loading required\n")
  
  # Create delay sequences
  delay_sequence <- seq(0, max_delay_years, by = step_size)
  n_combinations <- length(delay_sequence)^2
  
  cat("Total combinations to test:", n_combinations, "\n")
  cat("Delay values:", delay_sequence, "\n\n")
  
  # Initialize results storage
  results_df <- data.frame(
    mitigation_delay = numeric(n_combinations),
    cdr_delay = numeric(n_combinations),
    peak_temperature = numeric(n_combinations),
    final_emissions = numeric(n_combinations),
    emission_gap = numeric(n_combinations),
    total_cost = numeric(n_combinations),
    converged = logical(n_combinations),
    feasible = logical(n_combinations)
  )
  
  # Progress tracking
  combination_count <- 0
  start_time <- Sys.time()
  
  cat("Starting analysis...\n")
  
  # Iterate through all combinations
  for (mitig_delay in delay_sequence) {
    for (cdr_delay in delay_sequence) {
      
      combination_count <- combination_count + 1
      
      # Progress reporting
      if (combination_count %% 20 == 1 || combination_count <= 5) {
        elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
        cat("Combination", combination_count, "of", n_combinations, 
            sprintf("(%.1f%%) - Mitig delay: %d, CDR delay: %d, Elapsed: %.1f min\n", 
                    100*combination_count/n_combinations, mitig_delay, cdr_delay, elapsed_time))
      }
      
      # Run shooting method for this combination
      tryCatch({
        result <- delayed_shooting_method(
          parameter_df = parameter_df,
          emissions_df = emissions_df, 
          economic_df = economic_df,
          scenario = scenario,
          target_emissions = target_emissions,
          mitigation_delay_years = mitig_delay,
          cdr_delay_years = cdr_delay,
          max_shooting_iterations = 50,  # Reduced for efficiency
          shooting_tolerance = 3.0       # Slightly relaxed for efficiency
        )
        
        # Store results
        results_df[combination_count, ] <- list(
          mitigation_delay = mitig_delay,
          cdr_delay = cdr_delay,
          peak_temperature = result$peak_temperature,
          final_emissions = result$final_emissions,
          emission_gap = result$emission_gap,
          total_cost = result$total_cost,
          converged = result$converged,
          feasible = abs(result$emission_gap) <= 50  # Define feasibility threshold
        )
        
      }, error = function(e) {
        # Handle errors gracefully
        cat("ERROR in combination", combination_count, ":", e$message, "\n")
        results_df[combination_count, ] <- list(
          mitigation_delay = mitig_delay,
          cdr_delay = cdr_delay,
          peak_temperature = NA,
          final_emissions = NA,
          emission_gap = NA,
          total_cost = NA,
          converged = FALSE,
          feasible = FALSE
        )
      })
    }
  }
  
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  cat("\nAnalysis completed in", sprintf("%.1f", total_time), "minutes\n")
  
  # Summary statistics
  n_converged <- sum(results_df$converged, na.rm = TRUE)
  n_feasible <- sum(results_df$feasible, na.rm = TRUE)
  
  cat("\nSUMMARY:\n")
  cat("Total combinations:", n_combinations, "\n")
  cat("Converged solutions:", n_converged, sprintf("(%.1f%%)", 100*n_converged/n_combinations), "\n")
  cat("Feasible solutions:", n_feasible, sprintf("(%.1f%%)", 100*n_feasible/n_combinations), "\n")
  
  if (n_feasible > 0) {
    feasible_results <- results_df[results_df$feasible & !is.na(results_df$peak_temperature), ]
    cat("Peak temperature range:", sprintf("%.2f - %.2f°C", 
                                           min(feasible_results$peak_temperature, na.rm = TRUE),
                                           max(feasible_results$peak_temperature, na.rm = TRUE)), "\n")
  }
  
  return(results_df)
}

#' Create Heatmap of Peak Temperatures
create_peak_temperature_heatmap <- function(results_df, title_suffix = "") {
  
  # Filter for valid results
  plot_data <- results_df %>%
    filter(!is.na(peak_temperature)) %>%
    mutate(
      peak_temp_capped = pmin(peak_temperature, 8.0),  # Cap extreme values for better visualisation
      feasible_factor = ifelse(feasible, "Feasible", "Infeasible")
    )
  
  if (nrow(plot_data) == 0) {
    cat("No valid data for plotting\n")
    return(NULL)
  }
  
  # Create base heatmap
  p1 <- ggplot(plot_data, aes(x = mitigation_delay, y = cdr_delay, fill = peak_temp_capped)) +
    geom_tile() +
    scale_fill_viridis_c(
      name = "Peak\nTemperature\n(°C)",
      option = "plasma",
      direction = -1,
      limits = c(min(plot_data$peak_temp_capped, na.rm = TRUE), 
                 max(plot_data$peak_temp_capped, na.rm = TRUE))
    ) +
    labs(
      title = paste("Peak Temperature Anomaly by Deployment Delay", title_suffix),
      subtitle = paste("Target:", unique(results_df$final_emissions)[1], "GtCO2 |", 
                       sum(plot_data$feasible, na.rm = TRUE), "feasible combinations"),
      x = "Mitigation Deployment Delay (years)",
      y = "CDR Deployment Delay (years)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 10),
      panel.grid = element_blank()
    ) +
    coord_equal()
  
  # Add contour lines for better readability (optional - remove if metR not available)
  p1 <- p1 +
    geom_contour(aes(z = peak_temp_capped), color = "white", alpha = 0.3, size = 0.3)
  
  # Create feasibility overlay plot
  infeasible_data <- plot_data %>% filter(!feasible)
  
  if (nrow(infeasible_data) > 0) {
    p2 <- p1 + 
      geom_point(data = infeasible_data, 
                 aes(x = mitigation_delay, y = cdr_delay), 
                 shape = 4, size = 1, color = "red", alpha = 0.8, inherit.aes = FALSE) +
      labs(caption = "Red X marks indicate infeasible combinations (emission target not met)")
  } else {
    p2 <- p1
  }
  
  print(p2)
  
  return(list(plot = p2, data = plot_data))
}

#' Create Additional Analysis Plots
create_additional_plots <- function(results_df) {
  
  valid_data <- results_df %>%
    filter(!is.na(peak_temperature) & feasible)
  
  if (nrow(valid_data) == 0) {
    cat("No feasible data for additional plots\n")
    return(NULL)
  }
  
  # 1. Cost heatmap
  p_cost <- ggplot(valid_data, aes(x = mitigation_delay, y = cdr_delay, fill = total_cost)) +
    geom_tile() +
    scale_fill_viridis_c(name = "Total Cost\n(Trillion $)", option = "viridis") +
    labs(title = "Total Cost by Deployment Delay",
         x = "Mitigation Deployment Delay (years)",
         y = "CDR Deployment Delay (years)") +
    theme_minimal() +
    coord_equal()
  
  # 2. Trade-off scatter plot
  p_tradeoff <- ggplot(valid_data, aes(x = total_cost, y = peak_temperature)) +
    geom_point(aes(color = mitigation_delay + cdr_delay), alpha = 0.7, size = 2) +
    scale_color_viridis_c(name = "Total\nDelay\n(years)") +
    labs(title = "Cost vs Peak Temperature Trade-off",
         x = "Total Cost (Trillion $)",
         y = "Peak Temperature (°C)") +
    theme_minimal()
  
  # 3. Delay impact analysis
  delay_summary <- valid_data %>%
    group_by(mitigation_delay) %>%
    summarise(
      mean_peak_temp = mean(peak_temperature, na.rm = TRUE),
      min_peak_temp = min(peak_temperature, na.rm = TRUE),
      max_peak_temp = max(peak_temperature, na.rm = TRUE),
      .groups = 'drop'
    )
  
  p_delay_impact <- ggplot(delay_summary, aes(x = mitigation_delay)) +
    geom_ribbon(aes(ymin = min_peak_temp, ymax = max_peak_temp), alpha = 0.3, fill = "blue") +
    geom_line(aes(y = mean_peak_temp), color = "darkblue", size = 1) +
    labs(title = "Impact of Mitigation Delay on Peak Temperature",
         subtitle = "Blue band shows min-max range, line shows mean across CDR delays",
         x = "Mitigation Deployment Delay (years)",
         y = "Peak Temperature (°C)") +
    theme_minimal()
  
  # Print all plots
  print(p_cost)
  print(p_tradeoff)  
  print(p_delay_impact)
  
  return(list(cost_plot = p_cost, tradeoff_plot = p_tradeoff, delay_impact_plot = p_delay_impact))
}

#' Main function to run complete analysis
#' Uses existing dataframes in environment
run_complete_delayed_analysis <- function(parameter_df, emissions_df, economic_df,
                                          scenario = "SSP3-Baseline", 
                                          target_emissions = 650,
                                          max_delay_years = 40,
                                          step_size = 5) {
  
  cat("Starting complete delayed deployment analysis...\n\n")
  
  # Run the main analysis
  results <- run_delayed_deployment_analysis(
    parameter_df = parameter_df,
    emissions_df = emissions_df,
    economic_df = economic_df,
    scenario = scenario,
    target_emissions = target_emissions, 
    max_delay_years = max_delay_years,
    step_size = step_size
  )
  
  # Create visualisations
  cat("\nCreating visualisations...\n")
  
  heatmap_result <- create_peak_temperature_heatmap(results, paste("-", scenario))
  additional_plots <- create_additional_plots(results)
  
  # Save results
  write.csv(results, paste0("delayed_deployment_results_", scenario, "_", 
                            target_emissions, "GtCO2.csv"), row.names = FALSE)
  
  cat("\nAnalysis complete! Results saved to CSV file.\n")
  
  return(list(
    results_data = results,
    heatmap_plot = heatmap_result$plot,
    additional_plots = additional_plots
  ))
}

# Example usage:
# Run with your existing dataframes:
complete_analysis <- run_complete_delayed_analysis(
  parameter_df = parameter_df,
  emissions_df = emissions_df,
  economic_df = economic_df,
  scenario = "SSP3-Baseline",
  target_emissions = 650,
  max_delay_years = 70,
  step_size = 1
)