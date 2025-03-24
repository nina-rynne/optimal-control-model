#' @title Optimal Control Methods for Climate Temperature Overshoot
#' @description
#' This script contains functions for solving optimal control problems related to
#' climate temperature overshoot. It implements forward-backward sweep and shooting methods
#' to find optimal mitigation and carbon dioxide removal (CDR) strategies.
#'
#' @author Nina Rynne
#' @project Optimal Control of Temperature Overshoot
#' @date March 2025
#' @license MIT
#' @version 0.1.0
#'
#' @citation If published, include how to cite this work, e.g.:
#' Rynne, N. (2025). Optimal control methods for climate temperature overshoot.
#' Journal of Climate Modeling, XX(X), XXX-XXX. doi:XX.XXXX/XXXXX
#' 
#' @funding Include any grant or funding information here
#'
#' @collaborators List any collaborators or contributors
#'
#' @contact nina.rynne@griffithuni.edu.au
#'
#' @repository https://github.com/nina-rynne/optimal-control-model
#'
#' @references
#' - Lenhart, S., & Workman, J. T. (2007). Optimal control applied to biological models. CRC press.
#' - Nordhaus, W. D. (2017). Revisiting the social cost of carbon. Proceedings of the National Academy of Sciences.
#'
#' @dependencies
#' Required packages: dplyr, tidyr
#' 

# Load required packages
library(dplyr)    # For data manipulation
library(tidyr)    # For reshaping data
library(here)     # For file path management

#' @title Initialize Log File for Optimal Control Run
#' @description
#' Creates a log file with timestamp for recording simulation progress and debugging information.
#' @param prefix Prefix for the log file name (default: "optimal_control")
#' @param log_dir Directory for storing log files (default: "logs")
#' @return Path to the created log file
#' @examples
#' log_file <- init_log_file()
#' write_log(log_file, "Starting simulation with parameters X, Y, Z")
#'
init_log_file <- function(prefix = "optimal_control", log_dir = "logs") {
  # Create logs directory if it doesn't exist
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Create log file with timestamp
  log_filename <- file.path(log_dir, 
                            paste0(prefix, "_log_", 
                                   format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                   ".txt"))
  
  # Initialize the file with a header
  cat(paste0("Optimal Control Simulation Log\n",
             "Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
             "-------------------------------------------\n\n"),
      file = log_filename)
  
  return(log_filename)
}

#' @title Write to Log File
#' @description
#' Appends a message to the specified log file with timestamp.
#' @param log_file Path to the log file
#' @param message Message to write to the log
#' @param include_timestamp Whether to include timestamp (default: FALSE)
#' @return None
#' @examples
#' write_log(log_file, "Starting forward sweep")
#'
write_log <- function(log_file, message, include_timestamp = FALSE) {
  if (include_timestamp) {
    message <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", message)
  }
  
  cat(paste0(message, "\n"), file = log_file, append = TRUE)
}

#' @title Create Vector List for Optimal Control
#' @description 
#' This function
#' creates and initialises all the vectors required for the optimal control solution 
#' function.
#' 
#' @param parameter_df Dataframe containing parameters from LHS (experimental_design.R) 
#' and the fixed model and algorithm parameters (model_parameters.R). 
#' @param emissions_df Data frame containing emissions scenario data by year
#' 

create_vector_list <- function(parameter_df,
                               emissions_df,
                               economic_df,
                               scenario){
  # Time setup
  years <- unique(emissions_df$Year) # to avoid getting duplicate years
  years_rel <- years - min(years)
  n_years <- length(years)
  
  # State variables
  # numeric vector containing the yearly baseline CO2 emissions from IPCC scenarios
  baseline_annual_emissions <- emissions_df %>%
    filter(Scenario == scenario) %>%
    pull(Value) # annual emissions for chosen scenario
  # numeric vector filled with NA values. updated during forward sweep.
  temperature_anomaly <- rep(NA, n_years)
  # numeric vector to track cumulative CO2 emissions (after mit and CDR). updated during forward sweep.
  cumulative_emissions <- rep(0, n_years)
  
  # Initial state
  cumulative_emissions[1] <- baseline_annual_emissions[1]
  temperature_anomaly[1] <- temp_init + tcre * cumulative_emissions[1]
  
  # Control variables with bounds
  # numeric vector of mitigation control at each time step, initialised to 0
  mitigation_amount <- rep(0, n_years)
  # numeric vector of CDR control at each time step, initialised to 0
  removal_amount <- rep(0, n_years)
  mitigation_max <- baseline_annual_emissions
  # note: all other min/max values are single parameters, stored in parameter_df
  
  # Adjoint variable
  # numeric vector of adjoint variable, initialised to 0
  adjoint_var <- rep(0, n_years)
  adjoint_var[n_years] <- trans_cond  # Set Terminal condition for final value of lambda
  
  # Cost components
  mitigation_cost <- rep(NA, n_years)
  removal_cost <- rep(NA, n_years)
  residual_damage <- rep(NA, n_years)
  total_cost <- rep(NA, n_years)
  
  # Gross World Product
  baseline_annual_gwp <- economic_df %>%
    filter(Scenario == scenario) %>%
    pull(Value)
  
  # Cumulative Cost components, initialised to NA
  cost_mitig_cumul <- rep(NA, n_years)
  cost_remov_cumul <- rep(NA, n_years)
  cost_resid_cumul <- rep(NA, n_years)
  cost_total_cumul <- rep(NA, n_years)
  
}

#' @title Forward-Backward Sweep for Optimal Control
#' @description
#' Implements the forward-backward sweep method for finding optimal control
#' strategies for temperature overshoot management. The function finds optimal
#' mitigation and carbon dioxide removal paths.
#' 
#' @param emissions_df Data frame containing emissions scenario data by year
#' @param parameters List or data frame containing model parameters
#' @param gwp_df Optional data frame with Gross World Product forecast by year
#' @param log_file Optional log file path for detailed logging
#' @param max_iterations Maximum number of iterations (default: 10000)
#' @param convergence_tol Convergence tolerance (default: 0.001)
#' @return Data frame with optimal control solution and all state variables
#' @examples
#' # Run with standard parameter set
#' result <- forward_backward_sweep(emissions_df, parameters)
#' 
#' # Run with custom settings and logging
#' log_file <- init_log_file()
#' result <- forward_backward_sweep(emissions_df, parameters, gwp_df, 
#'                                 log_file, max_iterations = 5000)
#'
forward_backward_sweep <- function(emissions_df,
                                   economic_df,
                                   parameter_df,
                                   vector_list,
                                   log_file = NULL
                                   ) {
  
  # Extract parameters
  tcre <- parameters$tcre                  # Transient climate response to emissions (gam)
  cost_mitig <- parameters$cost_mitig      # Cost of mitigation (F_m)
  cost_remov <- parameters$cost_remov      # Cost of removal (F_r)
  econ_dam_pct <- parameters$econ_dam_pct  # Economic damage coefficient
  disc_rate <- parameters$disc_rate        # Discount rate (del)
  
  # Extract or set fixed parameters
  temp_init <- parameters$temp_init          # Initial temperature anomaly (T_0)
  co2_conc_preind <- parameters$co2_conc_preind  # Pre-industrial CO2 concentration (c0)
  co2_target_2100 <- parameters$co2_target_2100  # Target CO2 concentration in 2100
  time_step <- parameters$time_step          # Time step (dt)
  exp_mitig <- parameters$exp_mitig          # Mitigation exponent (q)
  exp_remov <- parameters$exp_remov          # Removal exponent (r)
  exp_dam <- parameters$exp_dam              # Damage exponent (s)
  upd_weight <- parameters$upd_weight        # Update weight
  trans_cond <- parameters$trans_cond        # Transversality condition (d)
  
  # Setup logging
  using_log <- !is.null(log_file)
  log_message <- function(msg) {
    if (using_log) {
      write_log(log_file, msg)
    }
  }
  
  log_message("Starting forward-backward sweep simulation")
  log_message(paste("Parameters:",
                    "\ntcre:", tcre,
                    "\ncost_mitig:", cost_mitig,
                    "\ncost_remov:", cost_remov,
                    "\necon_dam_pct:", econ_dam_pct,
                    "\ndisc_rate:", disc_rate,
                    "\ntemp_init:", temp_init,
                    "\nco2_target_2100:", co2_target_2100))
  
  # Initialize simulation constants
  dt <- time_step
  tol <- convergence_tol
  test <- -1   # convergence test variable
  n <- max_iterations
  counter <- 0 # start counter
  
  # Extract emissions data
  e_base <- emissions_df$Value  # yearly baseline emissions from SSP scenario
  years <- emissions_df$Year
  year_rel <- years - min(years)  # years relative to start year
  n_years <- length(years)
  
  # State and control variables initialization
  T_anom <- rep(NA, n_years)  # temperature anomaly
  damage <- rep(NA, n_years)  # damage costs
  e_real <- rep(0, n_years)   # realized emissions (after mitigation and CDR)
  
  # Control bounds
  u_m_min <- 0
  u_m_max <- e_base  # Maximum mitigation cannot exceed baseline emissions
  u_r_min <- 0
  u_r_max <- rep(100, n_years)  # Maximum CDR capacity
  
  # Initialize controls and adjoint
  u_m <- rep(0, n_years)  # Mitigation control
  u_r <- rep(0, n_years)  # Removal control
  lambda <- rep(0, n_years)  # Adjoint variable
  lambda[n_years] <- trans_cond  # Terminal condition
  
  # Initialize GWP (Gross World Product) for damage calculation
  if (!is.null(gwp_df)) {
    gwp <- gwp_df$value
    log_message("Using provided GWP forecasts")
  } else {
    stop("GWP forecast data is required but was not provided.")
  }
  
  # Calculate baseline temperature
  T_anom_baseline <- temp_init + tcre * cumsum(e_base)
  
  # Initialize first time step
  # Initialize first time step - with error handling
  if(length(e_base) > 0 && !is.na(e_base[1])) {
    e_real[1] <- e_base[1]
  } else {
    # Handle the case where e_base is empty or NA
    e_real[1] <- 0
    warning("Baseline emissions data is empty or starts with NA. Setting initial e_real to 0.")
  }
  T_anom[1] <- T_anom_baseline[1]
  
  # Calculate damage function
  # Using economic damage percentage applied to GWP instead of fixed b parameter
  damage_coefficient <- econ_dam_pct * gwp / (temp_init^exp_dam)
  
  damage[1] <- (cost_mitig * u_m[1]^exp_mitig + 
                  cost_remov * u_r[1]^exp_remov + 
                  damage_coefficient[1] * (T_anom[1])^exp_dam) * 
    exp(-disc_rate * year_rel[1])
  
  log_message(paste("Initial values:", 
                    "\ne_real[1]:", e_real[1],
                    "\nT_anom[1]:", T_anom[1],
                    "\nDamage[1]:", damage[1]))
  
  # Main iteration loop
  while (test < 0 && counter < n) {
    log_message(paste("\nIteration", counter + 1, "starting"))
    
    # Store old values for convergence test
    oldu_m <- u_m
    oldu_r <- u_r
    old_e_real <- e_real
    oldlambda <- lambda
    
    # Forward sweep
    for (i in 2:n_years) {
      # Ensure mitigation doesn't exceed baseline emissions
      u_m_bounded <- min(u_m[i-1], e_base[i-1])
      
      # State equation: change in emissions
      dc_dt <- e_base[i-1] - u_m_bounded - u_r[i-1]
      e_real[i] <- e_real[i-1] + dc_dt
      
      log_message(paste("Forward step:", i-1,
                        "\ne_base:", e_base[i-1],
                        "\nu_m:", u_m[i-1],
                        "\nu_m_bounded:", u_m_bounded,
                        "\nu_r:", u_r[i-1],
                        "\ndc_dt:", dc_dt,
                        "\ne_real:", e_real[i]))
    }
    
    # Calculate temperature from cumulative emissions
    T_anom <- temp_init + tcre * e_real
    
    # Backward sweep for adjoint
    for (i in 1:(n_years-1)) {
      j <- n_years - i  # backward index
      if (j >= 1) {
        # Adjoint equation: note damage depends on temperature which depends on emissions
        dlambda_dt <- -(damage_coefficient[j] * exp_dam) * exp(-disc_rate * (year_rel[j] - year_rel[1])) * 
          (temp_init + tcre * e_real[j])^(exp_dam-1) * tcre
        
        lambda[j] <- lambda[j+1] - dlambda_dt
      }
    }
    
    # Update mitigation control
    u_m1 <- (lambda / (exp_mitig * cost_mitig) * exp(disc_rate * year_rel))^(1/(exp_mitig-1))
    
    # Debug NA values in u_m1
    if (any(is.na(u_m1))) {
      cat("Debug info for NA detection in u_m1:\n")
      cat("Iteration:", counter, "\n")
      cat("lambda[1:5]:", paste(lambda[1:5], collapse=", "), "\n")
      cat("exp_mitig:", exp_mitig, "\n")
      cat("cost_mitig:", cost_mitig, "\n")
      cat("disc_rate:", disc_rate, "\n")
      cat("year_rel[1:5]:", paste(year_rel[1:5], collapse=", "), "\n")
      
      # Find problematic indices
      na_indices <- which(is.na(u_m1))
      cat("NA indices:", paste(na_indices, collapse=", "), "\n")
      
      if (length(na_indices) > 0) {
        i <- na_indices[1]  # Examine first problematic index
        cat("Problem at index", i, ":\n")
        cat("lambda[i]:", lambda[i], "\n")
        cat("Expression value:", (lambda[i] / (exp_mitig * cost_mitig) * exp(disc_rate * year_rel[i])), "\n")
        cat("Exponent value:", (1/(exp_mitig-1)), "\n")
      }
      
      stop("NA values detected in control calculation")
    }
    
    # Apply control bounds
    u_m1[u_m1 < u_m_min] <- u_m_min
    # Add validation at the beginning of the function
    if (any(is.na(e_base))) {
      stop("Baseline emissions data contains NA values, which will cause errors in the optimization. Please clean your data before proceeding.")
    }
    
    # For the control bounds check, instead of setting safe values:
    for (i in 1:n_years) {
      if (is.na(u_m1[i]) || is.na(u_m_max[i])) {
        stop("NA values detected in control or bounds during iteration. This indicates a problem with input data or model configuration.")
      }
      
      if (u_m1[i] > u_m_max[i]) {
        u_m1[i] <- u_m_max[i]
      }
    }
    
    # Update with weighted average to improve convergence
    u_m <- upd_weight * u_m1 + (1 - upd_weight) * oldu_m
    
    log_message(paste("\nMitigation control update example values:",
                      "\nlambda[1:5]:", paste(lambda[1:5], collapse=", "),
                      "\nu_m1[1:5]:", paste(u_m1[1:5], collapse=", "),
                      "\nu_m[1:5]:", paste(u_m[1:5], collapse=", ")))
    
    # Update removal control
    u_r1 <- (lambda * exp(disc_rate * year_rel) / (exp_remov * cost_remov))^(1/(exp_remov-1))
    
    # Apply control bounds
    u_r1[u_r1 < u_r_min] <- u_r_min
    u_r1[u_r1 > u_r_max] <- u_r_max
    
    # Update with weighted average
    u_r <- upd_weight * u_r1 + (1 - upd_weight) * oldu_r
    
    log_message(paste("\nRemoval control update example values:",
                      "\nu_r1[1:5]:", paste(u_r1[1:5], collapse=", "),
                      "\nu_r[1:5]:", paste(u_r[1:5], collapse=", ")))
    
    # Convergence test
    temp1 <- tol * sum(abs(u_m)) - sum(abs(oldu_m - u_m))
    temp2 <- tol * sum(abs(u_r)) - sum(abs(oldu_r - u_r))
    temp3 <- tol * sum(abs(e_real)) - sum(abs(e_real - old_e_real))
    temp4 <- tol * sum(abs(lambda)) - sum(abs(oldlambda - lambda))
    test <- min(temp1, temp2, temp3, temp4) 
    
    log_message(paste("\nConvergence test results:",
                      "\ntemp1:", temp1,
                      "\ntemp2:", temp2,
                      "\ntemp3:", temp3,
                      "\ntemp4:", temp4,
                      "\ntest:", test))
    
    # Update temperature and damage
    T_anom <- temp_init + tcre * e_real
    damage <- (cost_mitig * u_m^exp_mitig + 
                 cost_remov * u_r^exp_remov + 
                 damage_coefficient * (T_anom)^exp_dam) * 
      exp(-disc_rate * year_rel)
    
    counter <- counter + 1
  }
  
  # Log convergence results
  if (test < 0) {
    log_message(paste("\nForward-backward sweep failed to converge after", n, "iterations"))
  } else {
    log_message(paste("\nConverged after", counter, "iterations"))
  }
  
  # Prepare output dataframe
  output_frame <- emissions_df
  output_frame$Cumulative_Emissions <- e_real
  output_frame$Cumulative_Baseline <- cumsum(e_base)
  output_frame$Temp_Anom_C <- T_anom
  output_frame$Temp_Anom_Baseline_C <- T_anom_baseline
  output_frame$Damage <- damage
  output_frame$Mitigation <- u_m
  output_frame$Removal <- u_r
  output_frame$Adjoint <- lambda
  
  # Optional: Add parameter values to output metadata for reference
  attr(output_frame, "parameters") <- parameters
  attr(output_frame, "convergence") <- list(
    converged = test >= 0,
    iterations = counter,
    tolerance = tol,
    final_test = test
  )
  
  log_message("\nSimulation completed")
  return(output_frame)
}

#' @title Shooting Method for Fixed-Endpoint Optimal Control
#' @description
#' Implements a secant-based shooting method to solve the optimal control problem
#' with a fixed terminal state. The function iteratively adjusts the terminal
#' adjoint value to achieve a desired endpoint for cumulative emissions.
#' 
#' @param emissions_df Data frame containing emissions scenario data by year
#' @param parameters List or data frame containing model parameters
#' @param gwp_df Optional data frame with Gross World Product forecast by year
#' @param log_file Optional log file path for detailed logging
#' @param max_iterations Maximum number of shooting iterations (default: 100)
#' @param adjoint_tol Tolerance for adjoint convergence (default: 1e-8)
#' @return Data frame with optimal control solution and all state variables
#' @examples
#' # Run shooting method with standard parameters
#' result <- shooting_method(emissions_df, parameters)
#' 
#' # Run with custom settings and logging
#' log_file <- init_log_file("shooting_method")
#' result <- shooting_method(emissions_df, parameters, gwp_df, 
#'                          log_file, max_iterations = 50)
#'
shooting_method <- function(emissions_df, 
                            parameters,
                            gwp_df = NULL,
                            log_file = NULL,
                            max_iterations = 100,
                            adjoint_tol = 1e-8) {
  
  # Setup logging
  using_log <- !is.null(log_file)
  log_message <- function(msg) {
    if (using_log) {
      write_log(log_file, msg)
    }
  }
  
  log_message("Starting shooting method simulation")
  
  # Extract target CO2 and adjoint bounds
  co2_target_2100 <- parameters$co2_target_2100
  trans_low <- parameters$trans_low      # Lower bound for terminal adjoint
  trans_high <- parameters$trans_high    # Upper bound for terminal adjoint
  
  # Initialize iteration control variables
  flag <- -1     # Convergence flag
  count <- 0     # Iteration counter
  
  log_message(paste("Target CO2 in 2100:", co2_target_2100, 
                    "GtCO2\nInitial adjoint bounds:", trans_low, "to", trans_high))
  
  # Create parameter copies for low and high bound simulations
  params_l <- parameters
  params_h <- parameters
  
  # Set terminal condition for each bound
  params_l$trans_cond <- trans_low
  params_h$trans_cond <- trans_high
  
  # First evaluation at lower bound
  log_message("Running simulation with lower bound terminal adjoint")
  output_l <- forward_backward_sweep(emissions_df, params_l, gwp_df, log_file)
  
  # Extract final cumulative emissions and compute deviation from target
  final_year_index <- which.max(emissions_df$Year)
  final_emissions_l <- output_l$Cumulative_Emissions[final_year_index]
  Va <- final_emissions_l - co2_target_2100
  
  log_message(paste("Lower bound result: Final emissions =", final_emissions_l,
                    "GtCO2, Deviation from target =", Va, "GtCO2"))
  
  # Second evaluation at upper bound
  log_message("Running simulation with upper bound terminal adjoint")
  output_h <- forward_backward_sweep(emissions_df, params_h, gwp_df, log_file)
  
  # Extract final cumulative emissions and compute deviation from target
  final_emissions_h <- output_h$Cumulative_Emissions[final_year_index]
  Vb <- final_emissions_h - co2_target_2100
  
  log_message(paste("Upper bound result: Final emissions =", final_emissions_h,
                    "GtCO2, Deviation from target =", Vb, "GtCO2"))
  
  # Check if target is bracketed by the bounds
  if (Va * Vb > 0) {
    log_message(paste("WARNING: Target CO2 value not bracketed by initial adjoint bounds.",
                      "Va =", Va, "Vb =", Vb))
    
    # Attempt to expand bounds if they're in the same direction
    if (Va < 0 && Vb < 0) {
      # Both below target - increase upper bound
      params_h$trans_cond <- params_h$trans_cond * 10
      log_message(paste("Increasing upper bound to", params_h$trans_cond))
      
      output_h <- forward_backward_sweep(emissions_df, params_h, gwp_df, log_file)
      final_emissions_h <- output_h$Cumulative_Emissions[final_year_index]
      Vb <- final_emissions_h - co2_target_2100
      
      log_message(paste("New upper bound result: Final emissions =", final_emissions_h,
                        "GtCO2, Deviation from target =", Vb, "GtCO2"))
    } else if (Va > 0 && Vb > 0) {
      # Both above target - decrease lower bound
      params_l$trans_cond <- params_l$trans_cond / 10
      if (params_l$trans_cond < 0) params_l$trans_cond <- 0
      
      log_message(paste("Decreasing lower bound to", params_l$trans_cond))
      
      output_l <- forward_backward_sweep(emissions_df, params_l, gwp_df, log_file)
      final_emissions_l <- output_l$Cumulative_Emissions[final_year_index]
      Va <- final_emissions_l - co2_target_2100
      
      log_message(paste("New lower bound result: Final emissions =", final_emissions_l,
                        "GtCO2, Deviation from target =", Va, "GtCO2"))
    }
    
    # Check if we've bracketed the target now
    if (Va * Vb > 0) {
      log_message("ERROR: Target CO2 value still not bracketed by adjoint bounds after adjustment")
      log_message("Returning best available result")
      
      # Return the result closest to the target
      if (abs(Va) < abs(Vb)) {
        return(output_l)
      } else {
        return(output_h)
      }
    }
  }
  
  # Store current adjoint values
  lam_l <- params_l$trans_cond
  lam_h <- params_h$trans_cond
  
  # Main secant iteration loop
  while (flag < 0 && count < max_iterations) {
    count <- count + 1
    log_message(paste("\nShooting method iteration", count))
    
    # Ensure we're working with the better approximation
    if (abs(Va) > abs(Vb)) {
      # Swap values to keep better approximation
      k <- lam_l
      lam_l <- lam_h
      lam_h <- k
      k <- Va
      Va <- Vb
      Vb <- k
      
      # Also swap outputs
      temp <- output_l
      output_l <- output_h
      output_h <- temp
    }
    
    # Calculate secant method step
    d <- Va * (lam_h - lam_l) / (Vb - Va)
    
    # Update values for next iteration
    lam_h <- lam_l
    Vb <- Va
    lam_l <- lam_l - d
    
    log_message(paste("New adjoint value:", lam_l, 
                      "Previous deviation:", Va,
                      "Secant step:", d))
    
    # Update parameters with new adjoint value
    params_l$trans_cond <- lam_l
    
    # Evaluate at new point
    output_l <- forward_backward_sweep(emissions_df, params_l, gwp_df, log_file)
    final_emissions_l <- output_l$Cumulative_Emissions[final_year_index]
    Va <- final_emissions_l - co2_target_2100
    
    log_message(paste("New simulation result: Final emissions =", final_emissions_l,
                      "GtCO2, Deviation from target =", Va, "GtCO2"))
    
    # Check convergence
    if (abs(Va) <= adjoint_tol) {
      flag <- 1
      log_message(paste("Converged with deviation", Va, "GtCO2"))
    }
  }
  
  # Warning if convergence failed
  if (abs(Va) > adjoint_tol) {
    log_message(paste("WARNING: Could not meet final boundary condition.",
                      "Best deviation:", Va, "GtCO2"))
  }
  
  # Add shooting method metadata to result
  attr(output_l, "shooting_method") <- list(
    target_co2 = co2_target_2100,
    final_co2 = final_emissions_l,
    deviation = Va,
    iterations = count,
    converged = flag > 0,
    final_adjoint = lam_l
  )
  
  log_message("Shooting method completed")
  return(output_l)
}

#' @title Run Multiple Optimal Control Simulations
#' @description
#' Executes multiple optimal control simulations using different parameter sets
#' from a Latin Hypercube Sample or other experimental design. Results are stored
#' for each parameter set to analyze sensitivity and robustness.
#' 
#' @param emissions_df Data frame containing emissions scenario data by year
#' @param parameter_sets Data frame where each row is a parameter set for simulation
#' @param gwp_df Optional data frame with Gross World Product forecast by year
#' @param method Control method: "forward_backward" or "shooting" (default)
#' @param parallel Whether to run simulations in parallel (default: FALSE)
#' @param log_dir Directory for log files (default: "logs")
#' @param save_results Whether to save interim results (default: TRUE)
#' @param results_dir Directory for results (default: "results")
#' @return List containing results for all parameter sets and summary statistics
#' @examples
#' # Run simulations for all parameter sets
#' results <- run_multiple_simulations(emissions_df, parameter_sets)
#' 
#' # Run with parallel processing
#' results <- run_multiple_simulations(emissions_df, parameter_sets, 
#'                                    method = "shooting", parallel = TRUE)
#'
run_multiple_simulations <- function(emissions_df, 
                                     parameter_sets,
                                     gwp_df = NULL,
                                     method = "shooting",
                                     parallel = FALSE,
                                     log_dir = "logs",
                                     save_results = TRUE,
                                     results_dir = "results") {
  
  # Create directories if they don't exist
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (save_results && !dir.exists(results_dir)) {
    dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Initialize main log file
  main_log <- file.path(log_dir, paste0("multiple_runs_", 
                                        format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                        ".txt"))
  
  write_log(main_log, paste0("Multiple Optimal Control Simulations\n",
                             "Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
                             "Number of parameter sets: ", nrow(parameter_sets), "\n",
                             "Method: ", method, "\n",
                             "-------------------------------------------\n"))
  
  # Choose the simulation function based on method
  sim_function <- if (method == "shooting") {
    shooting_method
  } else {
    forward_backward_sweep
  }
  
  # Function to run a single simulation
  run_single_simulation <- function(param_set_idx) {
    # Extract parameters for this run
    params <- parameter_sets[param_set_idx, ]
    
    # Create run-specific log file
    run_log <- file.path(log_dir, paste0("run_", param_set_idx, "_", 
                                         format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                         ".txt"))
    
    write_log(main_log, paste("Starting run", param_set_idx, 
                              "with parameters:", 
                              "\ntcre =", params$tcre,
                              "\ncost_mitig =", params$cost_mitig,
                              "\ncost_remov =", params$cost_remov,
                              "\necon_dam_pct =", params$econ_dam_pct,
                              "\ndisc_rate =", params$disc_rate))
    
    # Run the simulation
    tryCatch({
      result <- sim_function(emissions_df, params, gwp_df, run_log)
      
      # Save individual result if requested
      if (save_results) {
        result_file <- file.path(results_dir, paste0("result_", param_set_idx, ".rds"))
        saveRDS(result, result_file)
      }
      
      write_log(main_log, paste("Run", param_set_idx, "completed successfully"))
      return(list(success = TRUE, result = result, params = params))
    }, 
    error = function(e) {
      write_log(main_log, paste("ERROR in run", param_set_idx, ":", conditionMessage(e)))
      return(list(success = FALSE, error = conditionMessage(e), params = params))
    })
  }
  
  # Run simulations - either in parallel or sequentially
  if (parallel) {
    # Check if parallel package is available
    if (!requireNamespace("parallel", quietly = TRUE)) {
      warning("Package 'parallel' is not available, using sequential processing instead")
      parallel <- FALSE
    } else {
      write_log(main_log, paste("Running in parallel mode with", 
                                parallel::detectCores(), "cores"))
      
      # Setup parallel cluster
      cl <- parallel::makeCluster(parallel::detectCores() - 1)  # Leave one core free
      
      # Export necessary objects to the cluster
      parallel::clusterExport(cl, c("emissions_df", "gwp_df", "sim_function", 
                                    "forward_backward_sweep", "shooting_method",
                                    "write_log", "main_log", "log_dir", "results_dir",
                                    "save_results"))
      
      # Load required packages on each worker
      parallel::clusterEvalQ(cl, {
        library(dplyr)
        library(tidyr)
      })
      
      # Run the simulations in parallel
      results <- parallel::parLapply(cl, 1:nrow(parameter_sets), run_single_simulation)
      
      # Stop the cluster
      parallel::stopCluster(cl)
    }
  }
  
  # Run sequentially if not in parallel
  if (!parallel) {
    write_log(main_log, "Running in sequential mode")
    results <- lapply(1:nrow(parameter_sets), run_single_simulation)
  }
  
  # Process results
  successful_runs <- which(sapply(results, function(x) x$success))
  n_success <- length(successful_runs)
  n_failed <- nrow(parameter_sets) - n_success
  
  write_log(main_log, paste("\nSimulation completed.",
                            "\nSuccessful runs:", n_success,
                            "\nFailed runs:", n_failed))
  
  # Extract successful results for analysis
  if (n_success > 0) {
    # Create a summary data frame with key metrics
    summary_df <- data.frame(
      run_id = successful_runs,
      tcre = sapply(results[successful_runs], function(x) x$params$tcre),
      cost_mitig = sapply(results[successful_runs], function(x) x$params$cost_mitig),
      cost_remov = sapply(results[successful_runs], function(x) x$params$cost_remov),
      econ_dam_pct = sapply(results[successful_runs], function(x) x$params$econ_dam_pct),
      disc_rate = sapply(results[successful_runs], function(x) x$params$disc_rate),
      max_temp = sapply(results[successful_runs], function(x) max(x$result$Temp_Anom_C)),
      total_mitigation = sapply(results[successful_runs], function(x) sum(x$result$Mitigation)),
      total_removal = sapply(results[successful_runs], function(x) sum(x$result$Removal)),
      total_damage = sapply(results[successful_runs], function(x) sum(x$result$Damage)),
      final_emissions = sapply(results[successful_runs], function(x) {
        idx <- which.max(x$result$Year)
        x$result$Cumulative_Emissions[idx]
      })
    )
    
    # Save summary to results directory
    if (save_results) {
      write.csv(summary_df, 
                file.path(results_dir, paste0("summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")),
                row.names = FALSE)
    }
    
    write_log(main_log, "Summary statistics generated successfully")
  } else {
    write_log(main_log, "No successful runs to analyze")
  }
  
  # Return the complete results object
  return(list(
    results = results,
    summary = if (n_success > 0) summary_df else NULL,
    successful_runs = successful_runs,
    n_success = n_success,
    n_failed = n_failed,
    timestamp = Sys.time()
  ))
}

#' @title Run Single Optimal Control Simulation
#' @description
#' Executes a single optimal control simulation with the specified parameters.
#' This function serves as a convenient wrapper for either the forward-backward sweep
#' or shooting method, handling parameter preparation and logging.
#' 
#' @param emissions_df Data frame containing emissions scenario data by year
#' @param parameters List or data frame row containing model parameters
#' @param gwp_df Optional data frame with Gross World Product forecast by year
#' @param method Control method: "forward_backward" or "shooting" (default)
#' @param verbose Whether to print progress messages (default: FALSE)
#' @param log_file Optional log file path
#' @return Simulation result data frame with optimal control solution
#' @examples
#' # Run a single simulation with the shooting method
#' result <- run_single_simulation(emissions_df, parameters)
#' 
#' # Run with forward-backward sweep and logging
#' log_file <- init_log_file("single_run")
#' result <- run_single_simulation(emissions_df, parameters, 
#'                                method = "forward_backward", 
#'                                verbose = TRUE, log_file = log_file)
#'
run_single_simulation <- function(emissions_df,
                                  parameters,
                                  gwp_df = NULL,
                                  method = "shooting",
                                  verbose = FALSE,
                                  log_file = NULL) {
  
  # Create log file if requested but not provided
  if (verbose && is.null(log_file)) {
    log_file <- init_log_file("single_run")
  }
  
  # Log start of simulation if verbose
  log_message <- function(msg) {
    if (verbose) {
      cat(paste0(msg, "\n"))
      if (!is.null(log_file)) {
        write_log(log_file, msg)
      }
    }
  }
  
  log_message("Starting single optimal control simulation")
  log_message(paste("Method:", method))
  log_message(paste("Parameters:",
                    "\ntcre:", parameters$tcre,
                    "\ncost_mitig:", parameters$cost_mitig,
                    "\ncost_remov:", parameters$cost_remov,
                    "\necon_dam_pct:", parameters$econ_dam_pct,
                    "\ndisc_rate:", parameters$disc_rate))
  
  # Choose simulation method
  if (method == "shooting") {
    log_message("Running shooting method")
    result <- shooting_method(emissions_df, parameters, gwp_df, log_file)
  } else {
    log_message("Running forward-backward sweep")
    result <- forward_backward_sweep(emissions_df, parameters, gwp_df, log_file)
  }
  
  log_message("Simulation completed successfully")
  
  # Return the result
  return(result)
}