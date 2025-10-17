#' Format Analysis Output
#'
#' Helper function to format analysis results according to the required output structure.
#' 
#' @param result_df Data frame with analysis results
#' @param aggregation_method Character string indicating the aggregation method
#' @param show_view Logical indicating whether to display results in Viewer pane
#' @param ques Character string specifying the question/variable name
#' @param disag Character string specifying the disaggregation variable name
#' @param analysis_type Character string indicating the type of analysis (single_select, multi_select, stat)
#' @return Formatted data frame with proper column names and structure
format_analysis_output <- function(result_df, aggregation_method, show_view = FALSE, ques = NULL, disag = NULL, analysis_type = NULL) {
  
  if(nrow(result_df) == 0) {
    empty_df <- data.frame(
      Response = character(0),
      stringsAsFactors = FALSE
    )
    if(show_view) {
      if(requireNamespace("htmltools", quietly = TRUE)) {
        # Create HTML table directly to avoid knitr warnings
        html_content <- htmltools::tags$div(
          htmltools::tags$h3("Analysis Results"),
          htmltools::tags$p("No data available")
        )
        htmltools::html_print(html_content)
      } else {
        View(empty_df)
      }
    }
    return(empty_df)
  }
  
  # Create the formatted result
  formatted_result <- data.frame(
    Response = result_df$Var1,
    stringsAsFactors = FALSE
  )
  
  # Add the disaggregation level column with the disaggregation variable name as column name
  if(result_df$disaggregation[1] != "all") {
    # Use the disaggregation variable name as the column name
    disag_col_name <- result_df$disaggregation[1]
    formatted_result[[disag_col_name]] <- result_df$disagg_level
  }
  
  # Add the aggregation method as a column name
  result_column <- NULL
  if(aggregation_method == "mean") {
    formatted_result$Mean <- result_df$Freq
    result_column <- "Mean"
  } else if(aggregation_method == "median") {
    formatted_result$Median <- result_df$Freq
    result_column <- "Median"
  } else if(aggregation_method == "sum") {
    formatted_result$Sum <- result_df$Freq
    result_column <- "Sum"
  } else if(aggregation_method == "1stq") {
    formatted_result$FirstQuartile <- result_df$Freq
    result_column <- "FirstQuartile"
  } else if(aggregation_method == "3rdq") {
    formatted_result$ThirdQuartile <- result_df$Freq
    result_column <- "ThirdQuartile"
  } else if(aggregation_method == "min") {
    formatted_result$Min <- result_df$Freq
    result_column <- "Min"
  } else if(aggregation_method == "max") {
    formatted_result$Max <- result_df$Freq
    result_column <- "Max"
  } else if(aggregation_method == "perc") {
    formatted_result$Percentage <- result_df$Freq
    result_column <- "Percentage"
  } else {
    # Fallback - use the aggregation method as column name
    result_column <- stringr::str_to_title(aggregation_method)
    formatted_result[[result_column]] <- result_df$Freq
  }
  
  # Add Count and Valid columns
  formatted_result$Count <- result_df$count
  formatted_result$Valid <- result_df$valid
  
  # For statistical functions, remove the Response column (it's always NA)
  if(aggregation_method %in% c("mean", "median", "sum", "1stq", "3rdq", "min", "max")) {
    formatted_result$Response <- NULL
  }
  
  # Sort results from largest to smallest based on the result column
  if(!is.null(result_column) && result_column %in% names(formatted_result)) {
    formatted_result <- formatted_result[order(formatted_result[[result_column]], decreasing = TRUE), ]
  }
  
  # Remove row names
  rownames(formatted_result) <- NULL
  
  # Show as HTML table in Viewer pane if requested
  if(show_view) {
    if(requireNamespace("htmltools", quietly = TRUE)) {
      # Create descriptive title based on analysis type and parameters
      title <- create_analysis_title(ques, disag, aggregation_method, analysis_type)
      
      # Create HTML table directly to avoid knitr warnings
      html_content <- htmltools::tags$div(
        htmltools::tags$h3(title),
        htmltools::tags$table(
          htmltools::tags$thead(
            htmltools::tags$tr(
              lapply(names(formatted_result), function(col) {
                htmltools::tags$th(col)
              })
            )
          ),
          htmltools::tags$tbody(
            lapply(1:nrow(formatted_result), function(i) {
              htmltools::tags$tr(
                lapply(formatted_result[i, ], function(cell) {
                  htmltools::tags$td(as.character(cell))
                })
              )
            })
          )
        )
      )
      htmltools::html_print(html_content)
    } else {
      View(formatted_result)
    }
  }
  
  return(formatted_result)
}

#' Create Analysis Title
#'
#' Helper function to create descriptive titles for HTML table output.
#' 
#' @param ques Character string specifying the question/variable name
#' @param disag Character string specifying the disaggregation variable name
#' @param aggregation_method Character string indicating the aggregation method
#' @param analysis_type Character string indicating the type of analysis
#' @return Character string with descriptive title
create_analysis_title <- function(ques, disag, aggregation_method, analysis_type) {
  
  # Start with the question/variable name
  title_parts <- c()
  
  if(!is.null(ques)) {
    title_parts <- c(title_parts, paste("Variable:", ques))
  }
  
  # Add disaggregation information
  if(!is.null(disag) && disag != "all") {
    title_parts <- c(title_parts, paste("Disaggregated by:", disag))
  }
  
  # Add analysis type and aggregation method
  if(!is.null(analysis_type)) {
    if(analysis_type == "single_select") {
      title_parts <- c(title_parts, "Single Select Analysis - Percentages")
    } else if(analysis_type == "multi_select") {
      title_parts <- c(title_parts, "Multi Select Analysis - Percentages")
    } else if(analysis_type == "stat") {
      # For statistical functions, show the specific method
      agg_title <- switch(aggregation_method,
                         "mean" = "Mean",
                         "median" = "Median", 
                         "sum" = "Sum",
                         "1stq" = "First Quartile",
                         "3rdq" = "Third Quartile",
                         "min" = "Minimum",
                         "max" = "Maximum",
                         stringr::str_to_title(aggregation_method))
      title_parts <- c(title_parts, paste("Statistical Analysis -", agg_title))
    }
  } else {
    # Fallback if analysis_type is not provided
    agg_title <- switch(aggregation_method,
                       "perc" = "Percentages",
                       "mean" = "Mean",
                       "median" = "Median", 
                       "sum" = "Sum",
                       "1stq" = "First Quartile",
                       "3rdq" = "Third Quartile",
                       "min" = "Minimum",
                       "max" = "Maximum",
                       stringr::str_to_title(aggregation_method))
    title_parts <- c(title_parts, paste("Analysis -", agg_title))
  }
  
  # Combine all parts
  return(paste(title_parts, collapse = " | "))
}


