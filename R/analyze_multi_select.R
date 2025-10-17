#' Enhanced Multi-Select Analysis with Automatic Disaggregation
#'
#' Analyzes multi-select questions with automatic disaggregation handling.
#' If a disaggregation variable is provided, the function automatically loops through
#' all levels and returns combined results. If not provided, performs overall analysis.
#'
#' @param df A data frame containing the survey data
#' @param ques Character string specifying the column name of the question to analyze
#' @param disag Character string specifying the disaggregation variable name (optional)
#' @param level Character string specifying the disaggregation level name (optional)
#' @param multi_response_sep Character string specifying the separator for multi-response questions (default: "; ")
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
#' # Analyze services overall (no disaggregation)
#' services_results <- analyze_multi_select(df = survey_data, 
#'                                         ques = "services_used",
#'                                         multi_response_sep = ";")
#' print(services_results)
#' 
#' # Analyze services by region (automatic disaggregation)
#' services_by_region <- analyze_multi_select(df = survey_data, 
#'                                           ques = "services_used", 
#'                                           disag = "region",
#'                                           multi_response_sep = ";")
#' print(services_by_region)
#'
#' @export
analyze_multi_select <- function(df, ques, disag = NULL, level = NULL, multi_response_sep = "; ", show_view = FALSE) {
  
  # Set global separator
  multi_response_sep <<- multi_response_sep
  
  # If no disaggregation variable provided, do overall analysis
  if(is.null(disag)) {
    return(multi_select(df, ques, "all", "all", show_view))
  }
  
  # Check if disaggregation variable exists in data
  if(!disag %in% names(df)) {
    stop(paste("Disaggregation variable", disag, "not found in data"))
  }
  
  # Get unique levels of disaggregation variable
  disag_levels <- unique(df[[disag]])
  disag_levels <- disag_levels[!is.na(disag_levels)]  # Remove NA values
  
  # If specific level requested, analyze only that level
  if(!is.null(level)) {
    if(level %in% disag_levels) {
      df_subset <- df[df[[disag]] == level & !is.na(df[[disag]]), ]
      return(multi_select(df_subset, ques, disag, level, show_view))
    } else {
      stop(paste("Level", level, "not found in disaggregation variable", disag))
    }
  }
  
  # Analyze all levels
  results_list <- list()
  
  for(disag_level in disag_levels) {
    df_subset <- df[df[[disag]] == disag_level & !is.na(df[[disag]]), ]
    if(nrow(df_subset) > 0) {  # Only analyze if there are observations
      results_list[[as.character(disag_level)]] <- multi_select(df_subset, ques, disag, disag_level, FALSE)
    }
  }
  
  # Combine results
  if(length(results_list) > 0) {
    combined_result <- do.call(rbind, results_list)
    # Remove row names
    rownames(combined_result) <- NULL
    
    # Sort results from largest to smallest based on Percentage column
    combined_result <- combined_result[order(combined_result$Percentage, decreasing = TRUE), ]
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      if(requireNamespace("knitr", quietly = TRUE) && requireNamespace("htmltools", quietly = TRUE)) {
        # Create HTML table and display in Viewer pane
        html_table <- knitr::kable(combined_result, 
                                  caption = "Multi Select Analysis Results", 
                                  format = "html")
        htmltools::html_print(htmltools::HTML(html_table))
      } else {
        View(combined_result)
      }
    }
    
    return(combined_result)
  } else {
    # Return empty result if no valid observations
    empty_result <- data.frame(
      Response = character(0),
      Percentage = numeric(0),
      Count = integer(0),
      Valid = integer(0),
      stringsAsFactors = FALSE
    )
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      if(requireNamespace("knitr", quietly = TRUE) && requireNamespace("htmltools", quietly = TRUE)) {
        # Create HTML table and display in Viewer pane
        html_table <- knitr::kable(empty_result, 
                                  caption = "Multi Select Analysis Results", 
                                  format = "html")
        htmltools::html_print(htmltools::HTML(html_table))
      } else {
        View(empty_result)
      }
    }
    
    return(empty_result)
  }
}


