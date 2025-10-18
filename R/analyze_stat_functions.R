#' Enhanced Statistical Analysis with Automatic Disaggregation
#'
#' Collection of enhanced statistical analysis functions with automatic disaggregation handling.
#' Each function automatically loops through disaggregation levels if provided.
#' If no disaggregation variable is provided, performs overall analysis.
#'
#' This file sources the individual statistical analysis functions from separate files
#' for better organization and maintainability.

# Source individual statistical analysis functions
source("R/analyze_mean.R")
source("R/analyze_median.R") 
source("R/analyze_sum.R")
source("R/analyze_quartiles.R")
source("R/analyze_minmax.R")

# Export all functions for package use
#' @export
analyze_mean

#' @export  
analyze_median

#' @export
analyze_sum

#' @export
analyze_first_quartile

#' @export
analyze_third_quartile

#' @export
analyze_min

#' @export
analyze_max