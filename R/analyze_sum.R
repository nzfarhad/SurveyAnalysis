#' Enhanced Sum Analysis with Automatic Disaggregation
#'
#' Enhanced statistical analysis function for calculating sums with automatic disaggregation handling.
#' Automatically loops through disaggregation levels if provided.
#' If no disaggregation variable is provided, performs overall analysis.
#'
#' @param df A data frame containing the survey data
#' @param ques Character string specifying the column name of the question to analyze
#' @param disag Character string specifying the disaggregation variable name (optional)
#' @param level Character string specifying the disaggregation level name (optional)
#' @param show_view Logical, whether to display results in Viewer pane
#' @param wide_format Logical, whether to reshape results to wide format
#' @param dt_table Logical, whether to use DT table for display
#' @param create_plot Logical, whether to create visualization
#' @param max_categories Integer, maximum number of categories to display
#' @param max_label_length Integer, maximum length for labels
#' @param font_sizes List of font sizes for plot elements
#' @return A data frame with sum value and metadata
#' @examples
#' # Create sample data
#' survey_data <- data.frame(
#'   age = c(25, 30, 35, 40, 28, 32, 45, 38),
#'   region = c("North", "South", "North", "South", "East", "North", "South", "East")
#' )
#' 
#' # Calculate sum by region
#' sum_by_region <- analyze_sum(df = survey_data, 
#'                             ques = "age", 
#'                             disag = "region")
#' print(sum_by_region)
#' @export
analyze_sum <- function(df, ques, disag = NULL, level = NULL, show_view = FALSE, wide_format = FALSE, dt_table = FALSE, create_plot = FALSE, max_categories = 10, max_label_length = 12, font_sizes = list(plot_title = 12, legend_title = 10, legend_text = 10, geom_text = 3, axis_title = 10)) {
  
  # If no disaggregation variable provided, do overall analysis
  if(is.null(disag)) {
    result <- stat_sum(df, ques, "all", "all")
    
    # Reshape to wide format if requested (though not applicable for non-disaggregated)
    if(wide_format) {
      warning("Wide format not applicable for non-disaggregated analysis")
    }
    
    # Create DT table object if requested (regardless of show_view)
    dt_table_obj <- NULL
    if(dt_table) {
      title <- create_analysis_title(ques, "all", "sum", "stat")
      filename <- paste0("sum_", ques)
      dt_table_obj <- create_dt_table(result, title, filename, display = show_view)
    }
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      # Create descriptive title
      title <- create_analysis_title(ques, "all", "sum", "stat")
      
      if(dt_table) {
        # DT table already created above, just display it
        if(!is.null(dt_table_obj)) {
          print(dt_table_obj)
        }
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
                lapply(seq_len(nrow(result)), function(i) {
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
    
    # Create visualization object if requested (regardless of show_view)
    plot_obj <- NULL
    if(create_plot) {
      plot_title <- create_analysis_title(ques, "all", "sum", "stat")
      plot_obj <- create_visualization(result, "sum", plot_title, max_categories, max_label_length = max_label_length, font_sizes = font_sizes)
    }
    
    # Display plot if requested and show_view is TRUE
    if(create_plot && show_view && !is.null(plot_obj)) {
      print(plot_obj)
    }
    
    # Return appropriate objects based on what was requested
    if(dt_table || create_plot) {
      return_list <- list(table = result)
      if(dt_table && !is.null(dt_table_obj)) {
        return_list$dt_table <- dt_table_obj
      }
      if(create_plot && !is.null(plot_obj)) {
        return_list$plot <- plot_obj
      }
      return(return_list)
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
      return(stat_sum(df_subset, ques, disag, level))
    } else {
      stop(paste("Level", level, "not found in disaggregation variable", disag))
    }
  }
  
  # Analyze all levels
  results_list <- list()
  
  for(disag_level in disag_levels) {
    df_subset <- df[df[[disag]] == disag_level & !is.na(df[[disag]]), ]
    if(nrow(df_subset) > 0) {  # Only analyze if there are observations
      results_list[[as.character(disag_level)]] <- stat_sum(df_subset, ques, disag, disag_level)
    }
  }
  
  # Combine results
  if(length(results_list) > 0) {
    combined_result <- do.call(rbind, results_list)
    # Remove row names
    rownames(combined_result) <- NULL
    
    # Sort results from largest to smallest based on Sum column
    combined_result <- combined_result[order(combined_result$Sum, decreasing = TRUE), ]
    
    # Reshape to wide format if requested
    if(wide_format && !is.null(disag) && disag != "all") {
      combined_result <- reshape_to_wide(combined_result, "sum", disag)
    }
    
    # Create DT table object if requested (regardless of show_view)
    dt_table_obj <- NULL
    if(dt_table) {
      title <- create_analysis_title(ques, disag, "sum", "stat")
      filename <- paste0("sum_", ques, ifelse(!is.null(disag) && disag != "all", paste0("_by_", disag), ""))
      dt_table_obj <- create_dt_table(combined_result, title, filename, display = show_view)
    }
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      # Create descriptive title
      title <- create_analysis_title(ques, disag, "sum", "stat")
      
      if(dt_table) {
        # DT table already created above, just display it
        if(!is.null(dt_table_obj)) {
          print(dt_table_obj)
        }
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
                lapply(seq_len(nrow(combined_result)), function(i) {
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
    
    # Create visualization object if requested (regardless of show_view)
    plot_obj <- NULL
    if(create_plot) {
      plot_title <- create_analysis_title(ques, disag, "sum", "stat")
      plot_obj <- create_visualization(combined_result, "sum", plot_title, max_categories, max_label_length = max_label_length, font_sizes = font_sizes)
    }
    
    # Display plot if requested and show_view is TRUE
    if(create_plot && show_view && !is.null(plot_obj)) {
      print(plot_obj)
    }
    
    # Return appropriate objects based on what was requested
    if(dt_table || create_plot) {
      return_list <- list(table = combined_result)
      if(dt_table && !is.null(dt_table_obj)) {
        return_list$dt_table <- dt_table_obj
      }
      if(create_plot && !is.null(plot_obj)) {
        return_list$plot <- plot_obj
      }
      return(return_list)
    }
    
    return(combined_result)
  } else {
    # Return empty result if no valid observations
    empty_result <- data.frame(
      Response = character(0),
      Sum = numeric(0),
      Count = integer(0),
      Valid = integer(0),
      stringsAsFactors = FALSE
    )
    
    # Create DT table object if requested (regardless of show_view)
    dt_table_obj <- NULL
    if(dt_table) {
      title <- create_analysis_title(ques, disag, "sum", "stat")
      filename <- paste0("sum_", ques, ifelse(!is.null(disag) && disag != "all", paste0("_by_", disag), ""))
      dt_table_obj <- create_dt_table(empty_result, title, filename, display = show_view)
    }
    
    # Create visualization object if requested (regardless of show_view)
    plot_obj <- NULL
    if(create_plot) {
      plot_title <- create_analysis_title(ques, disag, "sum", "stat")
      plot_obj <- create_visualization(empty_result, "sum", plot_title, max_categories, max_label_length = max_label_length, font_sizes = font_sizes)
    }
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      if(requireNamespace("htmltools", quietly = TRUE)) {
        # Create descriptive title
        title <- create_analysis_title(ques, disag, "sum", "stat")
        
        # Create HTML table directly to avoid knitr warnings
        html_content <- htmltools::tags$div(
          htmltools::tags$h3(title),
          htmltools::tags$p("No data available")
        )
        htmltools::html_print(html_content)
      } else {
        View(empty_result)
      }
    }
    
    # Display plot if requested and show_view is TRUE
    if(create_plot && show_view && !is.null(plot_obj)) {
      print(plot_obj)
    }
    
    # Return appropriate objects based on what was requested
    if(dt_table || create_plot) {
      return_list <- list(table = empty_result)
      if(dt_table && !is.null(dt_table_obj)) {
        return_list$dt_table <- dt_table_obj
      }
      if(create_plot && !is.null(plot_obj)) {
        return_list$plot <- plot_obj
      }
      return(return_list)
    }
    
    return(empty_result)
  }
}
