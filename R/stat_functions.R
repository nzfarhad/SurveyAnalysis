#' Statistical Analysis Functions
#'
#' Collection of functions for statistical analysis of numeric survey data.
#' These functions calculate various descriptive statistics for integer-type questions.
#' Each function can be used standalone or as part of the main analysis workflow.

# Mean calculation
#' @rdname stat_functions
#' @param df A data frame containing the survey data
#' @param ques Character string specifying the column name of the question to analyze
#' @param disag Character string specifying the disaggregation variable name
#' @param level Character string specifying the disaggregation level
#' @return A data frame with mean value and metadata
#' @examples
#' # Create sample data
#' survey_data <- data.frame(
#'   age = c(25, 30, 35, 40, 28, 32, 45, 38),
#'   region = c("North", "South", "North", "South", "East", "North", "South", "East")
#' )
#' 
#' # Calculate mean age overall
#' mean_age <- stat_mean(df = survey_data, 
#'                      ques = "age", 
#'                      disag = "all", 
#'                      level = "all")
#' print(mean_age)
#' 
#' # Calculate mean age by region
#' mean_age_north <- stat_mean(df = survey_data, 
#'                            ques = "age", 
#'                            disag = "region", 
#'                            level = "North")
#' print(mean_age_north)
#' @export
stat_mean <- function(df, ques, disag, level, show_view = FALSE){
  st_mean <- round(mean(as.numeric(df[[ques]]), na.rm = T), 1)
  res <- data.frame(row.names = 1)
  res$Var1 <- NA
  res$Freq <- st_mean
  res$aggregation_method <- "mean"
  res$variable <- ques
  res$count <- sum(!is.na(df[[ques]]))
  res$valid <- sum(!is.na(df[[ques]]))
  res$disaggregation <- disag
  res$disagg_level <- level
  
  # Format output according to requirements
  return(format_analysis_output(res, "mean", show_view))
}

# Median calculation
#' @rdname stat_functions
#' @examples
#' # Calculate median age
#' median_age <- stat_median(df = survey_data, 
#'                          ques = "age", 
#'                          disag = "all", 
#'                          level = "all")
#' print(median_age)
#' @export
stat_median <- function(df, ques, disag, level, show_view = FALSE){
  st_median <- round(median(as.numeric(df[[ques]]), na.rm = T), 1)
  res <- data.frame(row.names = 1)
  res$Var1 <- NA
  res$Freq <- st_median
  res$aggregation_method <- "median"
  res$variable <- ques
  res$count <- sum(!is.na(df[[ques]]))
  res$valid <- sum(!is.na(df[[ques]]))
  res$disaggregation <- disag
  res$disagg_level <- level
  
  # Format output according to requirements
  return(format_analysis_output(res, "median", show_view))
}

# Sum calculation
#' @rdname stat_functions
#' @export
stat_sum <- function(df, ques, disag, level, show_view = FALSE){
  st_sum <- sum(as.numeric(df[[ques]]), na.rm = T)
  res <- data.frame(row.names = 1)
  res$Var1 <- NA
  res$Freq <- st_sum
  res$aggregation_method <- "sum"
  res$variable <- ques
  res$count <- sum(!is.na(df[[ques]]))
  res$valid <- sum(!is.na(df[[ques]]))
  res$disaggregation <- disag
  res$disagg_level <- level
  
  # Format output according to requirements
  return(format_analysis_output(res, "sum", show_view))
}

# First quartile calculation
#' @rdname stat_functions
#' @export
stat_1stq <- function(df, ques, disag, level, show_view = FALSE){
  first_quart <- round(quantile(as.numeric(df[[ques]]), na.rm = T)[2], 1)
  res <- data.frame(row.names = 1)
  res$Var1 <- NA
  res$Freq <- first_quart
  res$aggregation_method <- "1stq"
  res$variable <- ques
  res$count <- sum(!is.na(df[[ques]]))
  res$valid <- sum(!is.na(df[[ques]]))
  res$disaggregation <- disag
  res$disagg_level <- level
  
  # Format output according to requirements
  return(format_analysis_output(res, "1stq", show_view))
}

# Third quartile calculation
#' @rdname stat_functions
#' @export
stat_3rdq <- function(df, ques, disag, level, show_view = FALSE){
  third_quart <- round(quantile(as.numeric(df[[ques]]), na.rm = T)[4], 1)
  res <- data.frame(row.names = 1)
  res$Var1 <- NA
  res$Freq <- third_quart
  res$aggregation_method <- "3rdq"
  res$variable <- ques
  res$count <- sum(!is.na(df[[ques]]))
  res$valid <- sum(!is.na(df[[ques]]))
  res$disaggregation <- disag
  res$disagg_level <- level
  
  # Format output according to requirements
  return(format_analysis_output(res, "3rdq", show_view))
}

# Minimum value calculation
#' @rdname stat_functions
#' @export
stat_min <- function(df, ques, disag, level, show_view = FALSE){
  st_min <- min(as.numeric(df[[ques]]), na.rm = T)
  res <- data.frame(row.names = 1)
  res$Var1 <- NA
  res$Freq <- st_min
  res$aggregation_method <- "min"
  res$variable <- ques
  res$count <- sum(!is.na(df[[ques]]))
  res$valid <- sum(!is.na(df[[ques]]))
  res$disaggregation <- disag
  res$disagg_level <- level
  
  # Format output according to requirements
  return(format_analysis_output(res, "min", show_view))
}

# Maximum value calculation
#' @rdname stat_functions
#' @export
stat_max <- function(df, ques, disag, level, show_view = FALSE){
  st_max <- max(as.numeric(df[[ques]]), na.rm = T)
  res <- data.frame(row.names = 1)
  res$Var1 <- NA
  res$Freq <- st_max
  res$aggregation_method <- "max"
  res$variable <- ques
  res$count <- sum(!is.na(df[[ques]]))
  res$valid <- sum(!is.na(df[[ques]]))
  res$disaggregation <- disag
  res$disagg_level <- level
  
  # Format output according to requirements
  return(format_analysis_output(res, "max", show_view))
}
