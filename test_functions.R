# Test script for data preparation functions
# ------------------------------------

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(here)  

# Source the data preparation functions
source(here::here("src", "data_preparation.R"))

# Test import_ssp_emissions function
# ------------------------------------
cat("Testing import_ssp_emissions function...\n")

# Import emissions data - just pass the filename, path handling is in the import function
emissions_imported <- import_ssp_emissions("emissions.csv")
cat("Imported emissions data dimensions:", dim(emissions_imported)[1], "rows,", 
    dim(emissions_imported)[2], "columns\n")

# Display sample of the imported emissions data
cat("\nSample of imported emissions data:\n")
print(head(emissions_imported, 3))

# Test import_ssp_economic function
# ------------------------------------
cat("\nTesting import_ssp_economic function...\n")

# Import economic data - just pass the filename, path handling is in the import function
economic_imported <- import_ssp_economic("gwp.csv")
cat("Imported economic data dimensions:", dim(economic_imported)[1], "rows,", 
    dim(economic_imported)[2], "columns\n")

# Display sample of the imported economic data
cat("\nSample of imported economic data:\n")
print(head(economic_imported, 3))

# Test interpolate_ssp_emissions function
# ------------------------------------
cat("\nTesting interpolate_ssp_emissions function...\n")

# Test with annual time step
emissions_interpolated <- interpolate_ssp_emissions(
  emissions_df = emissions_imported,
  dt = 1,
  start_year = 2020,
  end_year = 2100
)
cat("Interpolated emissions data dimensions:", dim(emissions_interpolated)[1], "rows,", 
    dim(emissions_interpolated)[2], "columns\n")

# Test interpolate_ssp_economic function
# ------------------------------------
cat("\nTesting interpolate_ssp_economic function...\n")

# Test with annual time step
economic_interpolated <- interpolate_ssp_economic(
  economic_df = economic_imported,
  dt = 1,
  start_year = 2020,
  end_year = 2100
)
cat("Interpolated economic data dimensions:", dim(economic_interpolated)[1], "rows,", 
    dim(economic_interpolated)[2], "columns\n")

cat("\nTest script completed.\n")