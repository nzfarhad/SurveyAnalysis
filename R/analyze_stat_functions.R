#' Enhanced Statistical Analysis with Automatic Disaggregation
#'
#' Collection of enhanced statistical analysis functions with automatic disaggregation handling.
#' Each function automatically loops through disaggregation levels if provided.
#' If no disaggregation variable is provided, performs overall analysis.

# Enhanced Mean calculation
#' @rdname analyze_stat_functions
#' @param df A data frame containing the survey data
#' @param ques Character string specifying the column name of the question to analyze
#' @param disag Character string specifying the disaggregation variable name (optional)
#' @param level Character string specifying the disaggregation level name (optional)
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
analyze_mean <- function(df, ques, disag = NULL, level = NULL, show_view = FALSE, wide_format = FALSE, dt_table = FALSE, create_plot = FALSE, max_categories = 10, max_label_length = 12) {
  
  # If no disaggregation variable provided, do overall analysis
  if(is.null(disag)) {
    result <- stat_mean(df, ques, "all", "all", show_view)
    
    # Create visualization if requested
    if(create_plot) {
      plot_title <- create_analysis_title(ques, "all", "mean", "stat")
      plot_obj <- create_visualization(result, "mean", plot_title, max_categories,
                                     original_data = df, ques = ques, disag = "all", max_label_length = max_label_length)
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
    
    # Reshape to wide format if requested
    if(wide_format && !is.null(disag) && disag != "all") {
      combined_result <- reshape_to_wide(combined_result, "mean", disag)
    }
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      # Create descriptive title
      title <- create_analysis_title(ques, disag, "mean", "stat")
      
      if(dt_table) {
        # Create DT table with search and download options
        filename <- paste0("mean_", ques, ifelse(!is.null(disag) && disag != "all", paste0("_by_", disag), ""))
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
      plot_title <- create_analysis_title(ques, disag, "mean", "stat")
      plot_obj <- create_visualization(combined_result, "mean", plot_title, max_categories,
                                     original_data = df, ques = ques, disag = disag, max_label_length = max_label_length)
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
      Mean = numeric(0),
      Count = integer(0),
      Valid = integer(0),
      stringsAsFactors = FALSE
    )
    
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
    
    return(empty_result)
  }
}

# Enhanced Median calculation
#' @rdname analyze_stat_functions
#' @examples
#' # Calculate median age by region
#' median_age_by_region <- analyze_median(df = survey_data, 
#'                                       ques = "age", 
#'                                       disag = "region")
#' print(median_age_by_region)
#' @export
analyze_median <- function(df, ques, disag = NULL, level = NULL, show_view = FALSE, wide_format = FALSE, dt_table = FALSE, create_plot = FALSE, max_categories = 10, max_label_length = 12) {
  
  # If no disaggregation variable provided, do overall analysis
  if(is.null(disag)) {
    result <- stat_median(df, ques, "all", "all")
    
    # Create visualization if requested
    if(create_plot) {
      plot_title <- create_analysis_title(ques, "all", "median", "stat")
      plot_obj <- create_visualization(result, "median", plot_title, max_categories,
                                     original_data = df, ques = ques, disag = "all", max_label_length = max_label_length)
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
      return(stat_median(df_subset, ques, disag, level))
    } else {
      stop(paste("Level", level, "not found in disaggregation variable", disag))
    }
  }
  
  # Analyze all levels
  results_list <- list()
  
  for(disag_level in disag_levels) {
    df_subset <- df[df[[disag]] == disag_level & !is.na(df[[disag]]), ]
    if(nrow(df_subset) > 0) {  # Only analyze if there are observations
      results_list[[as.character(disag_level)]] <- stat_median(df_subset, ques, disag, disag_level)
    }
  }
  
  # Combine results
  if(length(results_list) > 0) {
    combined_result <- do.call(rbind, results_list)
    # Remove row names
    rownames(combined_result) <- NULL
    
    # Sort results from largest to smallest based on Median column
    combined_result <- combined_result[order(combined_result$Median, decreasing = TRUE), ]
    
    # Reshape to wide format if requested
    if(wide_format && !is.null(disag) && disag != "all") {
      combined_result <- reshape_to_wide(combined_result, "median", disag)
    }
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      # Create descriptive title
      title <- create_analysis_title(ques, disag, "median", "stat")
      
      if(dt_table) {
        # Create DT table with search and download options
        filename <- paste0("median_", ques, ifelse(!is.null(disag) && disag != "all", paste0("_by_", disag), ""))
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
      plot_title <- create_analysis_title(ques, disag, "median", "stat")
      plot_obj <- create_visualization(combined_result, "median", plot_title, max_categories,
                                     original_data = df, ques = ques, disag = disag, max_label_length = max_label_length)
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
      Median = numeric(0),
      Count = integer(0),
      Valid = integer(0),
      stringsAsFactors = FALSE
    )
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      if(requireNamespace("htmltools", quietly = TRUE)) {
        # Create descriptive title
        title <- create_analysis_title(ques, disag, "median", "stat")
        
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
    
    return(empty_result)
  }
}

# Enhanced Sum calculation
#' @rdname analyze_stat_functions
#' @examples
#' # Calculate sum by region
#' sum_by_region <- analyze_sum(df = survey_data, 
#'                             ques = "age", 
#'                             disag = "region")
#' print(sum_by_region)
#' @export
analyze_sum <- function(df, ques, disag = NULL, level = NULL, show_view = FALSE, wide_format = FALSE, dt_table = FALSE, create_plot = FALSE, max_categories = 10, max_label_length = 12) {
  
  # If no disaggregation variable provided, do overall analysis
  if(is.null(disag)) {
    return(stat_sum(df, ques, "all", "all"))
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
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      # Create descriptive title
      title <- create_analysis_title(ques, disag, "sum", "stat")
      
      if(dt_table) {
        # Create DT table with search and download options
        filename <- paste0("sum_", ques, ifelse(!is.null(disag) && disag != "all", paste0("_by_", disag), ""))
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
      plot_title <- create_analysis_title(ques, disag, "sum", "stat")
      plot_obj <- create_visualization(combined_result, "sum", plot_title, max_categories, max_label_length = max_label_length)
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
      Sum = numeric(0),
      Count = integer(0),
      Valid = integer(0),
      stringsAsFactors = FALSE
    )
    
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
    
    return(empty_result)
  }
}

# Enhanced Quartile calculations
#' @rdname analyze_stat_functions
#' @examples
#' # Calculate first quartile by region
#' q1_by_region <- analyze_first_quartile(df = survey_data, 
#'                                        ques = "age", 
#'                                        disag = "region")
#' print(q1_by_region)
#' @export
analyze_first_quartile <- function(df, ques, disag = NULL, level = NULL, show_view = FALSE, wide_format = FALSE, dt_table = FALSE, create_plot = FALSE, max_categories = 10, max_label_length = 12) {
  
  # If no disaggregation variable provided, do overall analysis
  if(is.null(disag)) {
    return(stat_1stq(df, ques, "all", "all"))
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
      return(stat_1stq(df_subset, ques, disag, level))
    } else {
      stop(paste("Level", level, "not found in disaggregation variable", disag))
    }
  }
  
  # Analyze all levels
  results_list <- list()
  
  for(disag_level in disag_levels) {
    df_subset <- df[df[[disag]] == disag_level & !is.na(df[[disag]]), ]
    if(nrow(df_subset) > 0) {  # Only analyze if there are observations
      results_list[[as.character(disag_level)]] <- stat_1stq(df_subset, ques, disag, disag_level)
    }
  }
  
  # Combine results
  if(length(results_list) > 0) {
    combined_result <- do.call(rbind, results_list)
    # Remove row names
    rownames(combined_result) <- NULL
    
    # Sort results from largest to smallest based on FirstQuartile column
    combined_result <- combined_result[order(combined_result$FirstQuartile, decreasing = TRUE), ]
    
    # Reshape to wide format if requested
    if(wide_format && !is.null(disag) && disag != "all") {
      combined_result <- reshape_to_wide(combined_result, "1stq", disag)
    }
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      # Create descriptive title
      title <- create_analysis_title(ques, disag, "1stq", "stat")
      
      if(dt_table) {
        # Create DT table with search and download options
        filename <- paste0("first_quartile_", ques, ifelse(!is.null(disag) && disag != "all", paste0("_by_", disag), ""))
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
      plot_title <- create_analysis_title(ques, disag, "1stq", "stat")
      plot_obj <- create_visualization(combined_result, "1stq", plot_title, max_categories, max_label_length = max_label_length)
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
      FirstQuartile = numeric(0),
      Count = integer(0),
      Valid = integer(0),
      stringsAsFactors = FALSE
    )
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      if(requireNamespace("htmltools", quietly = TRUE)) {
        # Create descriptive title
        title <- create_analysis_title(ques, disag, "1stq", "stat")
        
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
    
    return(empty_result)
  }
}

#' @rdname analyze_stat_functions
#' @examples
#' # Calculate third quartile by region
#' q3_by_region <- analyze_third_quartile(df = survey_data, 
#'                                        ques = "age", 
#'                                        disag = "region")
#' print(q3_by_region)
#' @export
analyze_third_quartile <- function(df, ques, disag = NULL, level = NULL, show_view = FALSE, wide_format = FALSE, dt_table = FALSE, create_plot = FALSE, max_categories = 10, max_label_length = 12) {
  
  # If no disaggregation variable provided, do overall analysis
  if(is.null(disag)) {
    return(stat_3rdq(df, ques, "all", "all"))
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
      return(stat_3rdq(df_subset, ques, disag, level))
    } else {
      stop(paste("Level", level, "not found in disaggregation variable", disag))
    }
  }
  
  # Analyze all levels
  results_list <- list()
  
  for(disag_level in disag_levels) {
    df_subset <- df[df[[disag]] == disag_level & !is.na(df[[disag]]), ]
    if(nrow(df_subset) > 0) {  # Only analyze if there are observations
      results_list[[as.character(disag_level)]] <- stat_3rdq(df_subset, ques, disag, disag_level)
    }
  }
  
  # Combine results
  if(length(results_list) > 0) {
    combined_result <- do.call(rbind, results_list)
    # Remove row names
    rownames(combined_result) <- NULL
    
    # Sort results from largest to smallest based on ThirdQuartile column
    combined_result <- combined_result[order(combined_result$ThirdQuartile, decreasing = TRUE), ]
    
    # Reshape to wide format if requested
    if(wide_format && !is.null(disag) && disag != "all") {
      combined_result <- reshape_to_wide(combined_result, "3rdq", disag)
    }
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      # Create descriptive title
      title <- create_analysis_title(ques, disag, "3rdq", "stat")
      
      if(dt_table) {
        # Create DT table with search and download options
        filename <- paste0("third_quartile_", ques, ifelse(!is.null(disag) && disag != "all", paste0("_by_", disag), ""))
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
      plot_title <- create_analysis_title(ques, disag, "3rdq", "stat")
      plot_obj <- create_visualization(combined_result, "3rdq", plot_title, max_categories, max_label_length = max_label_length)
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
      ThirdQuartile = numeric(0),
      Count = integer(0),
      Valid = integer(0),
      stringsAsFactors = FALSE
    )
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      if(requireNamespace("htmltools", quietly = TRUE)) {
        # Create descriptive title
        title <- create_analysis_title(ques, disag, "3rdq", "stat")
        
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
    
    return(empty_result)
  }
}

# Enhanced Min/Max calculations
#' @rdname analyze_stat_functions
#' @examples
#' # Calculate minimum by region
#' min_by_region <- analyze_min(df = survey_data, 
#'                             ques = "age", 
#'                             disag = "region")
#' print(min_by_region)
#' @export
analyze_min <- function(df, ques, disag = NULL, level = NULL, show_view = FALSE, wide_format = FALSE, dt_table = FALSE, create_plot = FALSE, max_categories = 10, max_label_length = 12) {
  
  # If no disaggregation variable provided, do overall analysis
  if(is.null(disag)) {
    return(stat_min(df, ques, "all", "all"))
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
      return(stat_min(df_subset, ques, disag, level))
    } else {
      stop(paste("Level", level, "not found in disaggregation variable", disag))
    }
  }
  
  # Analyze all levels
  results_list <- list()
  
  for(disag_level in disag_levels) {
    df_subset <- df[df[[disag]] == disag_level & !is.na(df[[disag]]), ]
    if(nrow(df_subset) > 0) {  # Only analyze if there are observations
      results_list[[as.character(disag_level)]] <- stat_min(df_subset, ques, disag, disag_level)
    }
  }
  
  # Combine results
  if(length(results_list) > 0) {
    combined_result <- do.call(rbind, results_list)
    # Remove row names
    rownames(combined_result) <- NULL
    
    # Sort results from largest to smallest based on Min column
    combined_result <- combined_result[order(combined_result$Min, decreasing = TRUE), ]
    
    # Reshape to wide format if requested
    if(wide_format && !is.null(disag) && disag != "all") {
      combined_result <- reshape_to_wide(combined_result, "min", disag)
    }
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      # Create descriptive title
      title <- create_analysis_title(ques, disag, "min", "stat")
      
      if(dt_table) {
        # Create DT table with search and download options
        filename <- paste0("min_", ques, ifelse(!is.null(disag) && disag != "all", paste0("_by_", disag), ""))
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
      plot_title <- create_analysis_title(ques, disag, "min", "stat")
      plot_obj <- create_visualization(combined_result, "min", plot_title, max_categories, max_label_length = max_label_length)
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
      Min = numeric(0),
      Count = integer(0),
      Valid = integer(0),
      stringsAsFactors = FALSE
    )
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      if(requireNamespace("htmltools", quietly = TRUE)) {
        # Create descriptive title
        title <- create_analysis_title(ques, disag, "min", "stat")
        
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
    
    return(empty_result)
  }
}

#' @rdname analyze_stat_functions
#' @examples
#' # Calculate maximum by region
#' max_by_region <- analyze_max(df = survey_data, 
#'                             ques = "age", 
#'                             disag = "region")
#' print(max_by_region)
#' @export
analyze_max <- function(df, ques, disag = NULL, level = NULL, show_view = FALSE, wide_format = FALSE, dt_table = FALSE, create_plot = FALSE, max_categories = 10, max_label_length = 12) {
  
  # If no disaggregation variable provided, do overall analysis
  if(is.null(disag)) {
    return(stat_max(df, ques, "all", "all"))
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
      return(stat_max(df_subset, ques, disag, level))
    } else {
      stop(paste("Level", level, "not found in disaggregation variable", disag))
    }
  }
  
  # Analyze all levels
  results_list <- list()
  
  for(disag_level in disag_levels) {
    df_subset <- df[df[[disag]] == disag_level & !is.na(df[[disag]]), ]
    if(nrow(df_subset) > 0) {  # Only analyze if there are observations
      results_list[[as.character(disag_level)]] <- stat_max(df_subset, ques, disag, disag_level)
    }
  }
  
  # Combine results
  if(length(results_list) > 0) {
    combined_result <- do.call(rbind, results_list)
    # Remove row names
    rownames(combined_result) <- NULL
    
    # Sort results from largest to smallest based on Max column
    combined_result <- combined_result[order(combined_result$Max, decreasing = TRUE), ]
    
    # Reshape to wide format if requested
    if(wide_format && !is.null(disag) && disag != "all") {
      combined_result <- reshape_to_wide(combined_result, "max", disag)
    }
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      # Create descriptive title
      title <- create_analysis_title(ques, disag, "max", "stat")
      
      if(dt_table) {
        # Create DT table with search and download options
        filename <- paste0("max_", ques, ifelse(!is.null(disag) && disag != "all", paste0("_by_", disag), ""))
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
      plot_title <- create_analysis_title(ques, disag, "max", "stat")
      plot_obj <- create_visualization(combined_result, "max", plot_title, max_categories, max_label_length = max_label_length)
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
      Max = numeric(0),
      Count = integer(0),
      Valid = integer(0),
      stringsAsFactors = FALSE
    )
    
    # Show as HTML table in Viewer pane if requested
    if(show_view) {
      if(requireNamespace("htmltools", quietly = TRUE)) {
        # Create descriptive title
        title <- create_analysis_title(ques, disag, "max", "stat")
        
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
    
    return(empty_result)
  }
}


