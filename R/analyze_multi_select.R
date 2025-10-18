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
analyze_multi_select <- function(df, ques, disag = NULL, level = NULL, multi_response_sep = "; ", show_view = FALSE, wide_format = FALSE, dt_table = FALSE, create_plot = FALSE, max_categories = 10, chart_type = "column", max_label_length = 12, font_sizes = list(plot_title = 12, legend_title = 10, legend_text = 10, geom_text = 3, axis_title = 10)) {
  
  # Set global separator
  multi_response_sep <<- multi_response_sep
  
  # If no disaggregation variable provided, do overall analysis
  if(is.null(disag)) {
    result <- multi_select(df, ques, "all", "all", show_view)
    
    # Sort results from largest to smallest based on Percentage column
    result <- result[order(result$Percentage, decreasing = TRUE), ]
    
    # Reshape to wide format if requested (though not applicable for non-disaggregated)
    if(wide_format) {
      warning("Wide format not applicable for non-disaggregated analysis")
    }
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      # Create descriptive title
      title <- create_analysis_title(ques, "all", "perc", "multi_select")
      
      if(dt_table) {
        # Create DT table with search and download options
        filename <- paste0("multi_select_", ques)
        dt_table_obj <- create_dt_table(result, title, filename)
      } else {
        # Use basic HTML table
        if(requireNamespace("htmltools", quietly = TRUE)) {
          html_content <- htmltools::tags$div(
            htmltools::tags$h3(title),
            htmltools::tags$table(
              htmltools::tags$thead(
                htmltools::tags$tr(
                  lapply(names(result), function(col) {
                    htmltools::tags$th(col)
                  })
                )
              ),
              htmltools::tags$tbody(
                lapply(1:nrow(result), function(i) {
                  htmltools::tags$tr(
                    lapply(result[i, ], function(cell) {
                      htmltools::tags$td(as.character(cell))
                    })
                  )
                })
              )
            )
          )
          htmltools::html_print(html_content)
        } else {
          warning("htmltools package required for HTML table display")
        }
      }
    }
    
    # Create visualization if requested
    if(create_plot) {
      plot_title <- create_analysis_title(ques, "all", "perc", "multi_select")
      plot_obj <- create_visualization(result, "perc", plot_title, max_categories,
                                     color_primary = "#730202", color_secondary = "#f27304",
                                     chart_type = chart_type, max_label_length = max_label_length, font_sizes = font_sizes)
      if(!is.null(plot_obj)) {
        print(plot_obj)
        return(list(table = result, plot = plot_obj))
      }
    }
    
    return(result)
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
    
    # Reshape to wide format if requested
    if(wide_format && !is.null(disag) && disag != "all") {
      combined_result <- reshape_to_wide(combined_result, "perc", disag)
    }
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      # Create descriptive title
      title <- create_analysis_title(ques, disag, "perc", "multi_select")
      
      if(dt_table) {
        # Create DT table with search and download options
        filename <- paste0("multi_select_", ques, ifelse(!is.null(disag) && disag != "all", paste0("_by_", disag), ""))
        dt_table_obj <- create_dt_table(combined_result, title, filename)
      } else {
        # Use basic HTML table
        if(requireNamespace("htmltools", quietly = TRUE)) {
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
    }
    
    # Create visualization if requested
    if(create_plot) {
      plot_title <- create_analysis_title(ques, disag, "perc", "multi_select")
      plot_obj <- create_visualization(combined_result, "perc", plot_title, max_categories,
                                     color_primary = "#730202", color_secondary = "#f27304",
                                     chart_type = chart_type, max_label_length = max_label_length, font_sizes = font_sizes)
      if(!is.null(plot_obj)) {
        print(plot_obj)
        return(list(table = combined_result, plot = plot_obj))
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


