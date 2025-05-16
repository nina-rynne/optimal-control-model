# import qty_mitig and qty_remov from Mike's solution

# Load required packages
library(dplyr)
library(tidyr)
library(readr)  # For read_csv
library(here)   # For path management

# Import the numerical solution using read_csv and here
numerical_solution <- read_csv(here("data-raw", "numerical-control-vectors.csv"))

# Display the first few rows to verify import
head(numerical_solution)

# Ensure data is properly formatted 
numerical_solution <- numerical_solution %>%
  mutate(
    timestep = as.numeric(timestep),
    qty_mitig = as.numeric(qty_mitig),
    qty_remov = as.numeric(qty_remov),
    cumulative_emissions = as.numeric(cumul_emis)
  )

# If you need to check for any missing or invalid values
summary(numerical_solution)


# Create vector list
vector_list <- create_vector_list(
  parameter_df = parameter_df,
  emissions_df = emissions_df,
  economic_df = economic_df,
  scenario = scenario
)

# Function to incorporate numerical solution into your existing vector_list
# and derive the adjoint values from the optimal control values
initialize_with_numerical_solution <- function(vector_list, numerical_solution, parameter_df) {
  # Get the number of years in your model
  n_years <- vector_list$n_years
  years_rel <- vector_list$years_rel
  
  # Extract parameter values needed for adjoint calculation
  cost_mitig_unit <- parameter_df$cost_mitig_unit
  cost_remov_unit <- parameter_df$cost_remov_unit
  exp_mitig <- parameter_df$exp_mitig
  exp_remov <- parameter_df$exp_remov
  disc_rate <- parameter_df$disc_rate
  
  # Ensure numerical solution is sorted by timestep
  numerical_solution <- numerical_solution %>% arrange(timestep)
  
  # Update vector_list with numerical solution values
  vector_list$qty_mitig <- numerical_solution$qty_mitig
  vector_list$qty_remov <- numerical_solution$qty_remov
  vector_list$cumulative_emissions <- numerical_solution$cumulative_emissions
  
  # Calculate the adjoint values from the control variables using optimality conditions
  # Initialize adjoint variable vector
  adjoint_var <- numeric(n_years)
  
  for (i in 1:n_years) {
    # Base calculation on whichever control is interior (not at boundary)
    # Start with possible values from each control
    lambda_from_mitig <- NA
    lambda_from_remov <- NA
    
    # Calculate lambda from mitigation if it's an interior solution
    # (not at zero or maximum boundary)
    if (vector_list$qty_mitig[i] > 0 && 
        vector_list$qty_mitig[i] < vector_list$baseline_annual_emissions[i]) {
      lambda_from_mitig <- exp_mitig * cost_mitig_unit * 
        (vector_list$qty_mitig[i]^(exp_mitig-1)) * 
        exp(-disc_rate * years_rel[i])
    }
    
    # Calculate lambda from CDR if it's an interior solution (not at zero)
    if (vector_list$qty_remov[i] > 0) {
      lambda_from_remov <- exp_remov * cost_remov_unit * 
        (vector_list$qty_remov[i]^(exp_remov-1)) * 
        exp(-disc_rate * years_rel[i])
    }
    
    # Use the available lambda value, with preference for the CDR-derived one
    # as it's often more likely to be an interior solution
    if (!is.na(lambda_from_remov)) {
      adjoint_var[i] <- lambda_from_remov
    } else if (!is.na(lambda_from_mitig)) {
      adjoint_var[i] <- lambda_from_mitig
    } else {
      # If both controls are at boundaries, we need to make an educated guess
      # For the final timestep, use the terminal condition from parameter_df if available
      if (i == n_years && !is.null(parameter_df$trans_cond)) {
        adjoint_var[i] <- parameter_df$trans_cond
      } else if (i < n_years && !is.na(adjoint_var[i+1])) {
        # If we know the next timestep's adjoint, use that as a fallback
        adjoint_var[i] <- adjoint_var[i+1]
      } else {
        # Last resort: use a small positive value
        # (assuming adjoint is typically positive in this model)
        adjoint_var[i] <- 1e-6
      }
    }
  }
  
  # Update the adjoint variable in vector_list
  vector_list$adjoint_var <- adjoint_var
  
  # Return the updated vector_list
  return(vector_list)
}

numerical_vector_list <- initialize_with_numerical_solution(vector_list, numerical_solution, parameter_df)

# You can examine the derived adjoint values
plot(numerical_vector_list$years, numerical_vector_list$adjoint_var, 
     type = "l", col = "purple", lwd = 2,
     xlab = "Year", ylab = "Adjoint Value (λ)",
     main = "Derived Adjoint Values from Numerical Solution")

numerical_result <- forward_backward_sweep(parameter_df, numerical_vector_list)


#' @title Create Dashboard Plot for Numerical Solution
#' @description
#' Creates a comprehensive dashboard with key plots arranged in a grid showing model outputs
#' from your numerical solution: emissions, temperature, mitigation, removal, and costs.
#' 
#' @param numerical_result The numerical solution list containing model outputs
#' @param save_plot Logical indicating whether to save the plot (default: TRUE)
#' @param width Plot width in inches (default: 10)
#' @param height Plot height in inches (default: 8)
#' @param filename Custom filename (default: generated based on current time)
#'                   
#' @return A ggplot object combining plots in a layout

create_numerical_dashboard <- function(numerical_result, 
                                       save_plot = TRUE,
                                       width = 10,
                                       height = 8,
                                       filename = NULL) {
  
  # Required packages
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  
  # Extract data from numerical_result list
  years <- numerical_result$years
  temperature_anomaly <- numerical_result$temperature_anomaly
  cumulative_emissions <- numerical_result$cumulative_emissions
  qty_mitig <- numerical_result$qty_mitig
  qty_remov <- numerical_result$qty_remov
  
  # Create cost data
  cost_mitig_cumul <- numerical_result$cost_mitig_cumul
  cost_remov_cumul <- numerical_result$cost_remov_cumul
  cost_resid_cumul <- numerical_result$cost_resid_cumul
  cost_total_cumul <- numerical_result$cost_total_cumul
  
  # Convert to data frames for ggplot
  df_years <- data.frame(
    years = years,
    temperature_anomaly = temperature_anomaly,
    cumulative_emissions = cumulative_emissions,
    qty_mitig = qty_mitig, 
    qty_remov = qty_remov,
    cost_mitig_cumul = cost_mitig_cumul,
    cost_remov_cumul = cost_remov_cumul,
    cost_resid_cumul = cost_resid_cumul,
    cost_total_cumul = cost_total_cumul
  )
  
  # Convert costs into long format for stacked area plot
  df_costs <- df_years %>%
    select(years, cost_mitig_cumul, cost_remov_cumul, cost_resid_cumul) %>%
    tidyr::pivot_longer(
      cols = c(cost_mitig_cumul, cost_remov_cumul, cost_resid_cumul),
      names_to = "cost_type",
      values_to = "value"
    ) %>%
    mutate(cost_type = case_when(
      cost_type == "cost_mitig_cumul" ~ "Mitigation",
      cost_type == "cost_remov_cumul" ~ "Removal",
      cost_type == "cost_resid_cumul" ~ "Residual",
      TRUE ~ cost_type
    ))
  
  # Define common theme for all plots
  my_theme <- theme_minimal() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
    )
  
  # Create the individual plots
  # 1. Temperature Anomaly Plot
  p1 <- ggplot(df_years, aes(x = years, y = temperature_anomaly)) +
    geom_line(color = "steelblue", linewidth = 1) +
    my_theme +
    labs(title = "Temperature Anomaly",
         x = "Year",
         y = "Temperature (°C)")
  
  # 2. Cumulative Emissions Plot
  p2 <- ggplot(df_years, aes(x = years, y = cumulative_emissions)) +
    geom_line(color = "firebrick", linewidth = 1) +
    my_theme +
    labs(title = "Cumulative Emissions",
         x = "Year",
         y = "Emissions (GtCO2)")
  
  # 3. Mitigation Quantity Plot
  p3 <- ggplot(df_years, aes(x = years, y = qty_mitig)) +
    geom_line(color = "forestgreen", linewidth = 1) +
    my_theme +
    labs(title = "Mitigation Quantity",
         x = "Year",
         y = "Mitigation (GtCO2/yr)")
  
  # 4. Carbon Dioxide Removal Plot
  p4 <- ggplot(df_years, aes(x = years, y = qty_remov)) +
    geom_line(color = "purple4", linewidth = 1) +
    my_theme +
    labs(title = "Carbon Dioxide Removal",
         x = "Year",
         y = "CDR (GtCO2/yr)")
  
  # 5. Cost Components Plot (stacked area)
  p5 <- ggplot(df_costs, aes(x = years, y = value, fill = cost_type)) +
    geom_area(position = "stack") +
    scale_fill_brewer(palette = "Set2") +
    my_theme +
    theme(legend.position = "bottom") +
    labs(title = "Cumulative Costs",
         x = "Year",
         y = "Cost ($ Trillion)",
         fill = "Cost Component")
  
  # 6. Total Cost Plot
  p6 <- ggplot(df_years, aes(x = years, y = cost_total_cumul)) +
    geom_line(color = "darkorange", linewidth = 1) +
    my_theme +
    labs(title = "Total Cumulative Cost",
         x = "Year",
         y = "Cost ($ Trillion)")
  
  # Combine the plots using patchwork
  combined_plot <- (p1 + p2) / 
    (p3 + p4) / 
    (p5 + p6) +
    plot_layout(heights = c(1, 1, 1.2))
  
  # Add title
  final_plot <- combined_plot +
    plot_annotation(
      title = "Numerical Solution Dashboard",
      subtitle = paste("Total cost at end of period:", 
                       format(round(tail(cost_total_cumul, 1), 2), big.mark=","), 
                       "$ Trillion"),
      theme = theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5)
      )
    )
  
  # Save the plot if requested
  if (save_plot) {
    # Generate filename if not provided
    if (is.null(filename)) {
      filename <- paste0("numerical_solution_dashboard_", 
                         format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    }
    
    # Save the plot
    ggsave(filename, final_plot, width = width, height = height, dpi = 300)
    message(paste("Plot saved as", filename))
  }
  
  return(final_plot)
}

# After importing your numerical solution
dashboard_plot <- create_numerical_dashboard(numerical_result)
print(dashboard_plot)