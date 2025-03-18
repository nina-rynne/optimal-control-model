#' @title Data Preparation Functions for Climate Optimal Control
#' @description
#' This script contains functions for loading, processing and preparing data
#' for the optimal control of temperature overshoot model.
#'
#' @author Nina Rynne
#' @project Optimal Control of Temperature Overshoot
#' @date March 2025
#' @license MIT (or whatever license you plan to use)
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
#' @references
#' Riahi et al. (2017). The Shared Socioeconomic Pathways and their energy, 
#' land use, and greenhouse gas emissions implications: An overview, Global Environmental Change
#'  – processed by Our World in Data
#'  - https://ourworldindata.org/explorers/ipcc-scenarios
#'
#' @contact nina.rynne@griffithuni.edu.au
#'
#' @repository https://github.com/nina-rynne/optimal-control-model
#'
#'OTHER DATA TO CONSIDER INCLUDING
#'- data sources (version numbers, DOIs, access dates)
#'- methods documentation (descriptions of mathematical methods, interpolation techniques or assumptions)
#'- validation info (how was processing validated or verified)
#'- unit tests
#'- change log (document version changes over time)
#'- reproducibility notes (info on software versions and package dependencies)
#'- ORCID
#'- data dictionary (define what each variable represents, its units and valid ranges)
#'

#' This module provides the following key functions:
#' - import_ssp_emissions: Imports emissions scenario data from SSP sources
#' - import_ssp_economic: Imports economic data from SSP sources
#' - interpolate_ssp_emissions: Creates a complete emissions time series with interpolated values
#' - interpolate_ssp_economic: Creates a complete economic data time series with interpolated values
#' - align_ssp_datasets: Aligns emissions and economic datasets to the same time periods
#'

#' Each function includes detailed documentation on parameters and returns.
#' The functions are designed to be modular and reusable in different contexts
#' within the optimal control framework.

# Load required packages for data manipulation and CSV handling
library(dplyr)    # For data manipulation and filtering
library(tidyr)    # For reshaping data (pivot_wider)
library(readr)    # For reading CSV files

#' @title Import SSP Emissions Data
#' @description
#' Imports emissions scenario data from CSV files for SSP scenarios.
#' The function expects a CSV file with a structure containing columns for
#' Model, Scenario, Region, Variable, Unit, and year columns (2005, 2010, etc.).
#' Uses the here package for file path management.
#'
#' @param file_name Name of the file within the data directory (e.g., "emissions_data.csv")
#' @param scenarios Vector of scenario names to filter (e.g., c("SSP2-Baseline", "SSP5-Baseline")).
#'        If NULL (default), imports all available scenarios.
#' @param regions Vector of regions to filter (default: "world")
#' @param variables Vector of emission variables to import (e.g., "Emissions|CO2").
#'        If NULL (default), imports all available variables.
#' @return A data frame (tibble) containing filtered emissions data in wide format with
#'         columns for Model, Scenario, Region, Variable, Unit, and year columns
#' @examples
#' # Import all emissions data for all scenarios
#' all_emissions <- import_ssp_emissions("emissions_data.csv")
#'
#' # Import only specific scenarios and variables
#' ssp_emissions <- import_ssp_emissions(
#'   file_name = "emissions_data.csv",
#'   scenarios = c("SSP3-Baseline", "SSP5-Baseline"),
#'   variables = "Emissions|CO2"
#'   )
#' 

import_ssp_emissions <- function(file_name, scenarios = NULL, regions = "world", variables = NULL) {
  # Construct path using here
  file_path <- here::here("data", file_name)
  
  # Read the CSV file
  data <- readr::read_csv(file_path, show_col_types = FALSE)
  
  # Filter by scenarios if provided
  if (!is.null(scenarios)) {
    data <- data %>% dplyr::filter(Scenario %in% scenarios)
  }
  
  # Filter by regions if provided
  if (regions != "world") {
    data <- data %>% dplyr::filter(Region %in% regions)
  }
  
  # Filter by variables if provided
  if (!is.null(variables)) {
    data <- data %>% dplyr::filter(Variable %in% variables)
  }
  
  # Convert to wide format
  data_wide <- data %>%
    tidyr::pivot_wider(
      id_cols = c("Model", "Scenario", "Region", "Variable", "Unit"),
      names_from = "Year",
      values_from = "Value"
    )
  
  # Return the filtered data frame
  return(data_wide)
}

#' @title Import SSP Economic Data
#' @description
#' Imports economic data (GDP, GWP) for SSP scenarios from CSV files.
#' The function expects a CSV file with a structure containing columns for
#' Model, Scenario, Region, Variable, Unit, and year columns (2005, 2010, etc.).
#' Economic variables typically include "GDP|PPP" measured in billion US$2005/yr.
#' Uses the here package for file path management.
#'
#' @param file_name Name of the file within the data directory (e.g., "econ_gwp.csv")
#' @param scenarios Vector of scenario names to filter (e.g., c("SSP2-Baseline", "SSP5-Baseline")).
#'        If NULL (default), imports all available scenarios.
#' @param regions Vector of regions to filter (default: "world")
#' @param variables Vector of economic variables to import (default: NULL)
#' @return A data frame (tibble) containing filtered economic data in wide format with
#'         columns for Model, Scenario, Region, Variable, Unit, and year columns
#' @examples
#' # Import all economic data for all scenarios
#' all_economic <- import_ssp_economic("econ_gwp.csv")
#'
#' # Import only specific scenarios with specific variable GDP|PPP
#' ssp_economic <- import_ssp_economic(
#'   file_name = "econ_gwp.csv",
#'   scenarios = c("SSP3-Baseline", "SSP5-Baseline"),
#'   variables = "GDP|PPP"
#' )
#'

import_ssp_economic <- function(file_name, scenarios = NULL, regions = "world", variables = NULL) {
  # Construct path using here
  file_path <- here::here("data", file_name)
  
  # Read the CSV file
  data <- readr::read_csv(file_path, show_col_types = FALSE)
  
  # Filter by scenarios if provided
  if (!is.null(scenarios)) {
    data <- data %>% dplyr::filter(Scenario %in% scenarios)
  }
  
  # Filter by regions if provided
  if (regions != "world") {
    data <- data %>% dplyr::filter(Region %in% regions)
  }
  
  # Filter by variables if provided
  if (!is.null(variables)) {
    data <- data %>% dplyr::filter(Variable %in% variables)
  }
  
  # Convert to wide format
  data_wide <- data %>%
    tidyr::pivot_wider(
      id_cols = c("Model", "Scenario", "Region", "Variable", "Unit"),
      names_from = "Year",
      values_from = "Value"
    )
  
  # Return the filtered data frame
  return(data_wide)
}

#' @title Interpolate SSP Emissions Time Series
#' @description
#' Creates a complete emissions time series with specified time step by interpolating
#' between available data points in the wide-format SSP data. Can preserve specific
#' historical values if needed.
#'
#' @param emissions_df Data frame containing emissions data in wide format with year columns
#' @param dt Time step size for interpolation (in years)
#' @param start_year Starting year for the interpolated series
#' @param end_year Ending year for the interpolated series
#' @param preserve_historical Logical indicating whether to preserve historical values
#' @param historical_cutoff Year defining the cutoff between historical and projected data
#' @return A data frame with complete emissions time series at specified time step,
#'         restructured to long format with columns for Scenario, Model, Region, Variable,
#'         Unit, Year, and Value
#' @examples
#' interpolated_emissions <- interpolate_ssp_emissions(
#'   emissions_df = ssp_emissions_data,
#'   dt = 1,
#'   start_year = 2020,
#'   end_year = 2100,
#'   preserve_historical = TRUE,
#'   historical_cutoff = 2024
#' )
interpolate_ssp_emissions <- function(emissions_df, dt = 1, start_year = 2020, end_year = 2100,
                                      preserve_historical = TRUE, historical_cutoff = 2024) {
  # Function implementation goes here
}

#' @title Interpolate SSP Economic Time Series
#' @description
#' Creates a complete economic data time series with specified time step by interpolating
#' between available data points in the wide-format SSP economic data.
#'
#' @param economic_df Data frame containing economic data in wide format with year columns
#' @param dt Time step size for interpolation (in years)
#' @param start_year Starting year for the interpolated series
#' @param end_year Ending year for the interpolated series
#' @return A data frame with complete economic time series at specified time step,
#'         restructured to long format with columns for Scenario, Model, Region, Variable,
#'         Unit, Year, and Value
#' @examples
#' interpolated_economic <- interpolate_ssp_economic(
#'   economic_df = ssp_economic_data,
#'   dt = 1,
#'   start_year = 2020,
#'   end_year = 2100
#' )
interpolate_ssp_economic <- function(economic_df, dt = 1, start_year = 2020, end_year = 2100) {
  # Function implementation goes here
}

#' @title Align SSP Emissions and Economic Datasets
#' @description
#' Aligns and merges emissions and economic datasets to ensure they cover the same time periods
#' with the same time step, allowing for integrated analysis. Both datasets should be in
#' the long format returned by the interpolation functions.
#'
#' @param emissions_df Data frame containing the interpolated emissions data in long format
#' @param economic_df Data frame containing the interpolated economic data in long format
#' @param scenarios Vector of scenario names to include in the aligned dataset
#' @return A list containing aligned emissions and economic data frames, as well as a
#'         combined dataset if variables allow for meaningful integration
#' @examples
#' aligned_data <- align_ssp_datasets(
#'   emissions_df = interpolated_emissions,
#'   economic_df = interpolated_economic,
#'   scenarios = c("SSP3-Baseline", "SSP5-Baseline")
#' )
align_ssp_datasets <- function(emissions_df, economic_df, scenarios = NULL) {
  # Function implementation goes here
}
