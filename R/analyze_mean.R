#' Enhanced Mean Analysis with Automatic Disaggregation
#'
#' Enhanced statistical analysis function for calculating means with automatic disaggregation handling.
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
#' @return A data frame with mean value and metadata
#' @examples
#' # Create sample data
#' survey_data <- data.frame(
#'   age = c(25, 30, 35, 40, 28, 32, 45, 38),
#'   region = c("North", "South", "North", "South", "East", "North", "South", "East")
#' )
#' 
#' # Calculate mean age overall (no disaggregation)
#' mean_age <- analyze_mean(df = survey_data, ques = "age")
#' print(mean_age)
#' 
#' # Calculate mean age by region (automatic disaggregation)
#' mean_age_by_region <- analyze_mean(df = survey_data, 
#'                                   ques = "age", 
#'                                   disag = "region")
#' print(mean_age_by_region)
#' @export
analyze_mean <- function(df, ques, disag = NULL, level = NULL, show_view = FALSE, wide_format = FALSE, dt_table = FALSE, create_plot = FALSE, max_categories = 10, max_label_length = 12, font_sizes = list(plot_title = 12, legend_title = 10, legend_text = 10, geom_text = 3, axis_title = 10)) {
  
  # If no disaggregation variable provided, do overall analysis
  if(is.null(disag)) {
    result <- stat_mean(df, ques, "all", "all", show_view)
    
    # Reshape to wide format if requested (though not applicable for non-disaggregated)
    if(wide_format) {
      warning("Wide format not applicable for non-disaggregated analysis")
    }
    
    # Create DT table object if requested (regardless of show_view)
    dt_table_obj <- NULL
    if(dt_table) {
      title <- create_analysis_title(ques, "all", "mean", "stat")
      filename <- paste0("mean_", ques)
      dt_table_obj <- create_dt_table(result, title, filename, display = show_view)
    }
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      # Create descriptive title
      title <- create_analysis_title(ques, "all", "mean", "stat")
      
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
      plot_title <- create_analysis_title(ques, "all", "mean", "stat")
      plot_obj <- create_visualization(result, "mean", plot_title, max_categories,
                                     original_data = df, ques = ques, disag = "all", max_label_length = max_label_length, font_sizes = font_sizes)
    }
    
    # Display plot if requested and show_view is TRUE
    if(create_plot && show_view && !is.null(plot_obj)) {
      print(plot_obj)
    }
    
    # Return appropriate objects based on what was requested
    if(dt_table || create_plot || wide_format) {
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
      return(stat_mean(df_subset, ques, disag, level, show_view))
    } else {
      stop(paste("Level", level, "not found in disaggregation variable", disag))
    }
  }
  
  # Analyze all levels
  results_list <- list()
  
  for(disag_level in disag_levels) {
    df_subset <- df[df[[disag]] == disag_level & !is.na(df[[disag]]), ]
    if(nrow(df_subset) > 0) {  # Only analyze if there are observations
      results_list[[as.character(disag_level)]] <- stat_mean(df_subset, ques, disag, disag_level, FALSE)
    }
  }
  
  # Combine results
  if(length(results_list) > 0) {
    combined_result <- do.call(rbind, results_list)
    # Remove row names
    rownames(combined_result) <- NULL
    
    # Sort results from largest to smallest based on Mean column
    combined_result <- combined_result[order(combined_result$Mean, decreasing = TRUE), ]
    
    # Store original result for plotting before reshaping
    original_result_for_plot <- combined_result
    
    # Reshape to wide format if requested
    if(wide_format && !is.null(disag) && disag != "all") {
      combined_result <- reshape_to_wide(combined_result, "mean", disag)
    }
    
    # Create DT table object if requested (regardless of show_view)
    dt_table_obj <- NULL
    if(dt_table) {
      title <- create_analysis_title(ques, disag, "mean", "stat")
      filename <- paste0("mean_", ques, ifelse(!is.null(disag) && disag != "all", paste0("_by_", disag), ""))
      dt_table_obj <- create_dt_table(combined_result, title, filename, display = show_view)
    }
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      # Create descriptive title
      title <- create_analysis_title(ques, disag, "mean", "stat")
      
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
      plot_title <- create_analysis_title(ques, disag, "mean", "stat")
      # Filter original data to only include the levels that were actually analyzed
      # Use the original combined_result before wide format reshaping for plotting
      analyzed_levels <- unique(original_result_for_plot[[disag]])
      filtered_data <- df[df[[disag]] %in% analyzed_levels & !is.na(df[[disag]]), ]
      
      # For plotting, always use the original data structure, not the wide format
      plot_obj <- create_visualization(original_result_for_plot, "mean", plot_title, max_categories,
                                     original_data = filtered_data, ques = ques, disag = disag, max_label_length = max_label_length, font_sizes = font_sizes)
    }
    
    # Display plot if requested and show_view is TRUE
    if(create_plot && show_view && !is.null(plot_obj)) {
      print(plot_obj)
    }
    
    # Return appropriate objects based on what was requested
    if(dt_table || create_plot || wide_format) {
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
      Mean = numeric(0),
      Count = integer(0),
      Valid = integer(0),
      stringsAsFactors = FALSE
    )
    
    # Create DT table object if requested (regardless of show_view)
    dt_table_obj <- NULL
    if(dt_table) {
      title <- create_analysis_title(ques, disag, "mean", "stat")
      filename <- paste0("mean_", ques, ifelse(!is.null(disag) && disag != "all", paste0("_by_", disag), ""))
      dt_table_obj <- create_dt_table(empty_result, title, filename, display = show_view)
    }
    
    # Create visualization object if requested (regardless of show_view)
    plot_obj <- NULL
    if(create_plot) {
      plot_title <- create_analysis_title(ques, disag, "mean", "stat")
      plot_obj <- create_visualization(empty_result, "mean", plot_title, max_categories,
                                     original_data = df, ques = ques, disag = disag, max_label_length = max_label_length, font_sizes = font_sizes)
    }
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      if(requireNamespace("knitr", quietly = TRUE) && requireNamespace("htmltools", quietly = TRUE)) {
        # Create HTML table and display in Viewer pane
        html_table <- knitr::kable(empty_result, 
                                  caption = "Mean Analysis Results", 
                                  format = "html")
        htmltools::html_print(htmltools::HTML(html_table))
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
