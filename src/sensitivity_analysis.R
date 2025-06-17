# Script to extract data from multi_results nested list for ShapleyX analysis
# Extracts varying parameters and total_cost for sensitivity analysis

# Function to extract data from multi_results list
extract_shapleyx_data <- function(multi_results_list, output_filename = "sensitivity_data.csv") {
  
  # Check if multi_results exists
  if (!exists("multi_results")) {
    stop("multi_results object not found in environment")
  }
  
  # Get the successful_runs list
  successful_runs <- multi_results_list$successful_runs
  
  # Initialize empty data frame
  sensitivity_data <- data.frame()
  
  # Loop through each run in successful_runs
  for (i in 1:length(successful_runs)) {
    
    # Extract parameters for this run
    run_params <- successful_runs[[i]]$parameters
    
    # Extract the varying parameters
    tcre <- run_params$tcre
    cost_mitig_unit <- run_params$cost_mitig_unit
    cost_remov_unit <- run_params$cost_remov_unit
    econ_dam_pct <- run_params$econ_dam_pct
    disc_rate <- run_params$disc_rate
    
    # Extract total_cost (output variable)
    total_cost <- successful_runs[[i]]$total_cost
    
    # Create a row for this run
    run_data <- data.frame(
      tcre = tcre,
      cost_mitig_unit = cost_mitig_unit,
      cost_remov_unit = cost_remov_unit,
      econ_dam_pct = econ_dam_pct,
      disc_rate = disc_rate,
      Y = total_cost  # ShapleyX requires output to be named 'Y'
    )
    
    # Add to main data frame
    sensitivity_data <- rbind(sensitivity_data, run_data)
  }
  
  # Print summary
  cat("Extracted data summary:\n")
  cat("Number of runs:", nrow(sensitivity_data), "\n")
  cat("Parameters included:", paste(names(sensitivity_data)[1:5], collapse = ", "), "\n")
  cat("Output variable: Y (total_cost)\n\n")
  
  # Display first few rows
  cat("First 6 rows of extracted data:\n")
  print(head(sensitivity_data))
  
  # Save to CSV
  write.csv(sensitivity_data, output_filename, row.names = FALSE)
  cat("\nData saved to:", output_filename, "\n")
  
  # Return the data frame for further analysis if needed
  return(sensitivity_data)
}

# Alternative function if you want to extract other output variables
extract_shapleyx_data_custom <- function(multi_results_list, output_variable = "total_cost", output_filename = NULL) {
  
  # Set default filename if not provided
  if (is.null(output_filename)) {
    output_filename <- paste0("sensitivity_data_", output_variable, ".csv")
  }
  
  # Get the successful_runs list
  successful_runs <- multi_results_list$successful_runs
  
  # Initialize empty data frame
  sensitivity_data <- data.frame()
  
  # Loop through each run in successful_runs
  for (i in 1:length(successful_runs)) {
    
    # Extract parameters for this run
    run_params <- successful_runs[[i]]$parameters
    
    # Extract the varying parameters
    tcre <- run_params$tcre
    cost_mitig_unit <- run_params$cost_mitig_unit
    cost_remov_unit <- run_params$cost_remov_unit
    econ_dam_pct <- run_params$econ_dam_pct
    disc_rate <- run_params$disc_rate
    
    # Extract specified output variable
    output_value <- successful_runs[[i]][[output_variable]]
    
    # Check if output variable exists
    if (is.null(output_value)) {
      warning(paste("Output variable", output_variable, "not found in run", i))
      next
    }
    
    # Create a row for this run
    run_data <- data.frame(
      tcre = tcre,
      cost_mitig_unit = cost_mitig_unit,
      cost_remov_unit = cost_remov_unit,
      econ_dam_pct = econ_dam_pct,
      disc_rate = disc_rate,
      Y = output_value  # ShapleyX requires output to be named 'Y'
    )
    
    # Add to main data frame
    sensitivity_data <- rbind(sensitivity_data, run_data)
  }
  
  # Print summary
  cat("Extracted data summary:\n")
  cat("Number of runs:", nrow(sensitivity_data), "\n")
  cat("Parameters included:", paste(names(sensitivity_data)[1:5], collapse = ", "), "\n")
  cat("Output variable: Y (", output_variable, ")\n\n")
  
  # Display first few rows
  cat("First 6 rows of extracted data:\n")
  print(head(sensitivity_data))
  
  # Save to CSV
  write.csv(sensitivity_data, output_filename, row.names = FALSE)
  cat("\nData saved to:", output_filename, "\n")
  
  # Return the data frame
  return(sensitivity_data)
}

# Usage examples:

# 1. Extract data for total_cost analysis
# sensitivity_data <- extract_shapleyx_data(multi_results, "total_cost_sensitivity.csv")

# 2. Extract data for other output variables (e.g., final_emissions)
# emissions_data <- extract_shapleyx_data_custom(multi_results, "final_emissions", "emissions_sensitivity.csv")

# 3. Extract data for mitigation costs
# mitig_data <- extract_shapleyx_data_custom(multi_results, "mitig_cost", "mitig_cost_sensitivity.csv")

# 4. Extract data for removal costs
# remov_data <- extract_shapleyx_data_custom(multi_results, "remov_cost", "remov_cost_sensitivity.csv")

# Quick check function to see available output variables
check_available_outputs <- function(multi_results_list) {
  
  if (length(multi_results_list$successful_runs) > 0) {
    # Get variable names from first run (excluding 'parameters')
    first_run <- multi_results_list$successful_runs[[1]]
    available_vars <- names(first_run)[names(first_run) != "parameters"]
    
    cat("Available output variables for sensitivity analysis:\n")
    for (var in available_vars) {
      cat("-", var, "\n")
    }
    
    return(available_vars)
  } else {
    cat("No successful runs found in multi_results\n")
    return(NULL)
  }
}