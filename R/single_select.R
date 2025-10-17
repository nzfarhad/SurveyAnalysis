#' Single Select Question Analysis
#'
#' Analyzes single-select questions from survey data, calculating percentages and counts.
#' This function can be used standalone or as part of the main analysis workflow.
#'
#' @param df A data frame containing the survey data
#' @param ques Character string specifying the column name of the question to analyze
#' @param disag Character string specifying the disaggregation variable name
#' @param level Character string specifying the disaggregation level
#'
#' @return A data frame with analysis results including percentages, counts, and metadata
#'
#' @examples
#' # Create sample data
#' survey_data <- data.frame(
#'   gender = c("Male", "Female", "Male", "Female", "Male"),
#'   region = c("North", "South", "North", "South", "East")
#' )
#' 
#' # Analyze gender overall
#' gender_results <- single_select(df = survey_data, 
#'                                ques = "gender", 
#'                                disag = "all", 
#'                                level = "all")
#' print(gender_results)
#' 
#' # Analyze gender by region
#' gender_by_region <- single_select(df = survey_data, 
#'                                  ques = "gender", 
#'                                  disag = "region", 
#'                                  level = "North")
#' print(gender_by_region)
#'
#' @export
single_select <- function(df, ques, disag, level, show_view = FALSE){
  
  if (all(is.na(df[[ques]]))) {
    prop <- data.frame(row.names = 1)
    prop$Var1 <- NA
    prop$Freq <- NA
    prop$aggregation_method <- "perc"
    prop$variable <- ques
    prop$count <- NA
    prop$valid <- sum(!is.na(df[[ques]]))
    prop$disaggregation <- disag
    prop$disagg_level <- level
    
    # Format output according to requirements
    return(format_analysis_output(prop, "perc", show_view, ques, disag, "single_select"))
  }
  
  else{
    
    cnt <- table(df[[ques]])
    prop <- round(prop.table(table(df[[ques]])) * 100 , 1)
    cnt <- as.data.frame(cnt)
    prop <- as.data.frame(prop)
    
    prop$aggregation_method <- "perc"
    prop$variable <- ques
    prop$count <- cnt$Freq
    prop$valid <- sum(!is.na(df[[ques]]))
    prop$disaggregation <- disag
    prop$disagg_level <- level
    
    # Format output according to requirements
    return(format_analysis_output(prop, "perc", show_view, ques, disag, "single_select"))
  }
}
