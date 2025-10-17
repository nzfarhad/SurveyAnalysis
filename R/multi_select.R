#' Multi-Select Question Analysis
#'
#' Analyzes multi-select questions from survey data, calculating percentages and counts.
#' Handles responses separated by a delimiter (default "; "). This function can be used 
#' standalone or as part of the main analysis workflow.
#'
#' @param df A data frame containing the survey data
#' @param ques Character string specifying the column name of the question to analyze
#' @param disag Character string specifying the disaggregation variable name
#' @param level Character string specifying the disaggregation level
#'
#' @return A data frame with analysis results including percentages, counts, and metadata
#'
#' @examples
#' # Create sample data with multi-select responses
#' survey_data <- data.frame(
#'   services_used = c("Health; Education", "Health", "Education; Transport", 
#'                     "Health; Education; Transport", "Transport"),
#'   region = c("North", "South", "North", "South", "East")
#' )
#' 
#' # Set the separator (this would normally be set globally)
#' multi_response_sep <<- ";"
#' 
#' # Analyze services overall
#' services_results <- multi_select(df = survey_data, 
#'                                 ques = "services_used", 
#'                                 disag = "all", 
#'                                 level = "all")
#' print(services_results)
#' 
#' # Analyze services by region
#' services_by_region <- multi_select(df = survey_data, 
#'                                   ques = "services_used", 
#'                                   disag = "region", 
#'                                   level = "North")
#' print(services_by_region)
#'
#' @export
multi_select <- function(df, ques, disag, level, show_view = FALSE, format_output = TRUE){
  
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
    
    # Format output according to requirements only if requested
    if(format_output) {
      return(format_analysis_output(prop, "perc", show_view, ques, disag, "multi_select"))
    } else {
      return(prop)
    }
  }
  
  else{
    
    # Split multi-response answers by separator
    vec <- stringr::str_split(df[[ques]], pattern = multi_response_sep) %>% reshape2::melt()
    # Calculate percentages based on non-NA responses
    prop <- round((table(vec$value) / nrow(df[!is.na(df[[ques]]), ])) * 100, 1)
    cnt <- table(vec$value)
    cnt <- as.data.frame(cnt)
    prop <- as.data.frame(prop)
    prop$aggregation_method <- "perc"
    prop$variable <- ques
    prop$count <- cnt$Freq
    prop$valid <- sum(!is.na(df[[ques]]))
    prop$disaggregation <- disag
    prop$disagg_level <- level
    
    # Format output according to requirements only if requested
    if(format_output) {
      return(format_analysis_output(prop, "perc", show_view, ques, disag, "multi_select"))
    } else {
      return(prop)
    }
  }
  
}
