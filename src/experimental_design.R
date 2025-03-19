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

