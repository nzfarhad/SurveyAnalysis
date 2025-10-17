#' Enhanced Single Select Analysis with Automatic Disaggregation
#'
#' Analyzes single-select questions with automatic disaggregation handling.
#' If a disaggregation variable is provided, the function automatically loops through
#' all levels and returns combined results. If not provided, performs overall analysis.
#'
#' @param df A data frame containing the survey data
#' @param ques Character string specifying the column name of the question to analyze
#' @param disag Character string specifying the disaggregation variable name (optional)
#' @param level Character string specifying the disaggregation level name (optional)
#'
#' @return A data frame with analysis results including percentages, counts, and metadata
#'
#' @examples
#' # Create sample data
#' survey_data <- data.frame(
#'   gender = c("Male", "Female", "Male", "Female", "Male"),
#'   region = c("North", "South", "North", "South", "East"),
#'   facility_type = c("Hospital", "Clinic", "Hospital", "Health Center", "Clinic")
#' )
#' 
#' # Analyze gender overall (no disaggregation)
#' gender_results <- analyze_single_select(df = survey_data, ques = "gender")
#' print(gender_results)
#' 
#' # Analyze gender by region (automatic disaggregation)
#' gender_by_region <- analyze_single_select(df = survey_data, 
#'                                          ques = "gender", 
#'                                          disag = "region")
#' print(gender_by_region)
#'
#' @export
analyze_single_select <- function(df, ques, disag = NULL, level = NULL, show_view = FALSE) {
  
  # If no disaggregation variable provided, do overall analysis
  if(is.null(disag)) {
    return(single_select(df, ques, "all", "all", show_view))
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
      return(single_select(df_subset, ques, disag, level, show_view))
    } else {
      stop(paste("Level", level, "not found in disaggregation variable", disag))
    }
  }
  
  # Analyze all levels
  results_list <- list()
  
  for(disag_level in disag_levels) {
    df_subset <- df[df[[disag]] == disag_level & !is.na(df[[disag]]), ]
    if(nrow(df_subset) > 0) {  # Only analyze if there are observations
      results_list[[as.character(disag_level)]] <- single_select(df_subset, ques, disag, disag_level, FALSE)
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
      if(requireNamespace("htmltools", quietly = TRUE)) {
        # Create descriptive title
        title <- create_analysis_title(ques, disag, "perc", "single_select")
        
        # Create HTML table directly to avoid knitr warnings
        html_content <- htmltools::tags$div(
          htmltools::tags$h3(title),
          htmltools::tags$table(
            htmltools::tags$thead(
              htmltools::tags$tr(
                lapply(names(combined_result), function(col) {
                  htmltools::tags$th(col)
                })
              )
            ),
            htmltools::tags$tbody(
              lapply(1:nrow(combined_result), function(i) {
                htmltools::tags$tr(
                  lapply(combined_result[i, ], function(cell) {
                    htmltools::tags$td(as.character(cell))
                  })
                )
              })
            )
          )
        )
        htmltools::html_print(html_content)
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
      if(requireNamespace("htmltools", quietly = TRUE)) {
        # Create HTML table directly to avoid knitr warnings
        html_content <- htmltools::tags$div(
          htmltools::tags$h3("Single Select Analysis Results"),
          htmltools::tags$p("No data available")
        )
        htmltools::html_print(html_content)
      } else {
        View(empty_result)
      }
    }
    
    return(empty_result)
  }
}


