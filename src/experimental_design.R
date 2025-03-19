#' @title Experimental Design for Climate Optimal Control
#' @description
#' This script contains functions for generating experimental designs for parameter
#' uncertainty analysis in the optimal control of temperature overshoot model. It includes
#' Latin Hypercube Sampling and other sampling methods for sensitivity analysis.
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
#' - IPCC (2021) Climate Change 2021: The Physical Science Basis. Contribution 
#' of Working Group I to the Sixth Assessment Report of the Intergovernmental 
#' Panel on Climate Change [Masson-Delmotte, V. et al (eds.)]. Cambridge University 
#' Press, Cambridge, United Kingdom and New York, NY, USA, 2391 pp.
#'
#' @dependencies
#' Required packages: lhs, dplyr, here
#' 

# Load required libraries
library(dplyr) # data manipulation
library(here)  # file management
library(lhs)   # Latin hypercube sampling

#' @title Latin Hypercube Sampling for Model Parameters
#' @description
#' Generates parameter sets using Latin Hypercube Sampling for model sensitivity analysis.
#' The function creates a specified number of parameter combinations where each parameter
#' range is divided into equal probability intervals.
#' 
#' @param n_samples    Number of sample points (default: 50)
#' @param seed         Random seed for reproducibility (default: NULL)
#' @param sampling_method LHS method: "random", "optimum", or "genetic" (default: "random")
#' @param return_raw   Return raw 0-1 samples alongside scaled samples (default: FALSE)
#' @return Data frame of parameter combinations, or if return_raw=TRUE, a list
#'         containing raw_samples and scaled_samples
#' @examples
#' # Generate 100 samples using default LHS and specific seed
#' lhs_results <- generate_lhs_samples(n_samples = 100, seed = 42)
#' # Generate samples using optimum sampling method and include raw data in return
#' lhs_results <- generate_lhs_samples(sampling_method = "optimum", return_raw = TRUE) 
#'                               

generate_lhs_samples <- function(n_samples = 50, 
                                 seed = NULL, 
                                 sampling_method = "random",
                                 return_raw = FALSE) {
  # Parameter validation
  if (!is.numeric(n_samples) || n_samples <= 0) {
    stop("n_samples must be a positive number")
  }
  
  valid_methods <- c("random", "optimum", "genetic")
  if (!sampling_method %in% valid_methods) {
    stop("sampling_method must be one of: ", paste(valid_methods, collapse=", "))
  }
  
  if (!is.logical(return_raw)) {
    stop("return_raw must be TRUE or FALSE")
  }
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Define parameter ranges
  parameter_ranges <- list(
    param1 = c(min = 0, max = 1),
    param2 = c(min = 10, max = 100),
    param3 = c(min = -5, max = 5),
    param4 = c(min = 0.01, max = 0.1),
    param5 = c(min = 100, max = 500)
  )
  
  # Continue with rest of function...
}







