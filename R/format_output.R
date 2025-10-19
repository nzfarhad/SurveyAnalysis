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
  
  # Add percentage signs if this is percentage data
  formatted_result <- add_percentage_signs(formatted_result, aggregation_method)
  
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
create_analysis_title <- function(ques, disag, aggregation_method, analysis_type, max_line_length = 80) {
  
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
      title_parts <- c(title_parts, "Question type - Single Select")
    } else if(analysis_type == "multi_select") {
      title_parts <- c(title_parts, "Question type - Multi Select")
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
  
  # Fallback
  if (length(title_parts) == 0) return("Analysis Summary")
  
  # Combine first (for character count check)
  full_title <- paste(title_parts, collapse = " | ")
  
  # ✅ Smart split only if title is too long
  if (nchar(full_title) > max_line_length && length(title_parts) > 1) {
    
    # Calculate cumulative length of each part (including separators)
    cum_lengths <- cumsum(nchar(title_parts) + 3)  # +3 for " | "
    
    # Find the last part that keeps total length under max_line_length
    break_index <- max(which(cum_lengths <= max_line_length))
    
    # Build the two-line title cleanly
    full_title <- paste(
      paste(title_parts[1:break_index], collapse = " | "),
      paste(title_parts[(break_index + 1):length(title_parts)], collapse = " | "),
      sep = "\n"
    )
  }
  
  return(full_title)
}

#' Reshape Analysis Results to Wide Format
#'
#' Helper function to reshape analysis results from long to wide format,
#' with disaggregation levels as columns.
#' 
#' @param result_df Data frame with analysis results in long format
#' @param aggregation_method Character string indicating the aggregation method
#' @param disag Character string specifying the disaggregation variable name
#' @return Data frame in wide format with disaggregation levels as columns
reshape_to_wide <- function(result_df, aggregation_method, disag) {
  
  if(nrow(result_df) == 0) {
    return(data.frame())
  }
  
  # Determine the result column name
  result_column <- switch(aggregation_method,
                         "perc" = "Percentage",
                         "mean" = "Mean",
                         "median" = "Median", 
                         "sum" = "Sum",
                         "1stq" = "FirstQuartile",
                         "3rdq" = "ThirdQuartile",
                         "min" = "Min",
                         "max" = "Max",
                         stringr::str_to_title(aggregation_method))
  
  # Create wide format data
  if(disag != "all" && disag %in% names(result_df)) {
    
    # For statistical functions, use a different approach since there's no Response column
    if(aggregation_method %in% c("mean", "median", "sum", "1stq", "3rdq", "min", "max")) {
      # For statistical functions, we need to create a single row with disaggregation levels as columns
      disag_levels <- unique(result_df[[disag]])
      wide_data <- data.frame()
      
      # Create columns for each disaggregation level
      for(level in disag_levels) {
        level_data <- result_df[result_df[[disag]] == level, ]
        if(nrow(level_data) > 0) {
          wide_data[1, paste0(result_column, "_", level)] <- level_data[[result_column]][1]
          wide_data[1, paste0("Count_", level)] <- level_data$Count[1]
          wide_data[1, paste0("Valid_", level)] <- level_data$Valid[1]
        }
      }
      
    } else {
      # For percentage-based functions (single/multi select)
      # Reshape to wide format
      wide_data <- reshape(result_df, 
                          idvar = "Response", 
                          timevar = disag, 
                          direction = "wide")
      
      # Clean up column names and reorder
      names(wide_data) <- gsub(paste0(result_column, "."), paste0(result_column, "_"), names(wide_data))
      names(wide_data) <- gsub(paste0("Count."), "Count_", names(wide_data))
      names(wide_data) <- gsub(paste0("Valid."), "Valid_", names(wide_data))
      
      # Reorder columns: Response, then result columns, then count columns, then valid columns
      response_col <- "Response"
      result_cols <- names(wide_data)[grepl(paste0("^", result_column, "_"), names(wide_data))]
      count_cols <- names(wide_data)[grepl("^Count_", names(wide_data))]
      valid_cols <- names(wide_data)[grepl("^Valid_", names(wide_data))]
      
      # Create ordered column names: Response, then alternating result/count/valid for each level
      ordered_cols <- c(response_col)
      disag_levels <- unique(result_df[[disag]])
      
      for(level in disag_levels) {
        if(paste0(result_column, "_", level) %in% names(wide_data)) {
          ordered_cols <- c(ordered_cols, paste0(result_column, "_", level))
        }
        if(paste0("Count_", level) %in% names(wide_data)) {
          ordered_cols <- c(ordered_cols, paste0("Count_", level))
        }
        if(paste0("Valid_", level) %in% names(wide_data)) {
          ordered_cols <- c(ordered_cols, paste0("Valid_", level))
        }
      }
      
      # Reorder the data frame - only keep the ordered columns
      wide_data <- wide_data[, ordered_cols[ordered_cols %in% names(wide_data)]]
      
      # Sort by the first result column (descending)
      if(length(result_cols) > 0) {
        wide_data <- wide_data[order(wide_data[[result_cols[1]]], decreasing = TRUE, na.last = TRUE), ]
      }
    }
    
  } else {
    # No disaggregation, return as is but ensure proper column names
    wide_data <- result_df
    if("Freq" %in% names(wide_data)) {
      names(wide_data)[names(wide_data) == "Freq"] <- result_column
    }
  }
  
  # Remove row names
  rownames(wide_data) <- NULL
  
  # Add percentage signs if this is percentage data
  wide_data <- add_percentage_signs(wide_data, aggregation_method)
  
  return(wide_data)
}

#' Create DT Table with Search and Download Options
#'
#' Helper function to create an interactive DT table with search and Excel download.
#' 
#' @param data Data frame to display
#' @param title Character string for table title
#' @param filename Character string for download filename
#' @param display Logical, whether to display the table immediately (default: TRUE)
#' @return DT table object
create_dt_table <- function(data, title, filename = "analysis_results", display = TRUE) {
  
  if(requireNamespace("DT", quietly = TRUE)) {
    # Check if this is wide format data (has columns with _ in them)
    is_wide_format <- any(grepl("_", names(data)))
    
    if(is_wide_format) {
      # Create a proper wide format table with merged headers
      # First, let's create a custom HTML table structure
      
      if(requireNamespace("htmltools", quietly = TRUE)) {
        # Extract column information
        col_names <- names(data)
        
        # Find disaggregation levels
        disag_cols <- col_names[grepl("_", col_names)]
        disag_levels <- unique(gsub(".*_(.*)$", "\\1", disag_cols))
        
        # Determine if this is a statistical function (no Response column)
        is_stat_function <- !any(grepl("^Response$", col_names))
        
        # Extract aggregation method from column names for statistical functions
        if(is_stat_function) {
          # Find the aggregation method from the first column name
          first_col <- col_names[1]
          if(grepl("^Mean_", first_col)) {
            agg_method <- "Mean"
          } else if(grepl("^Median_", first_col)) {
            agg_method <- "Median"
          } else if(grepl("^Sum_", first_col)) {
            agg_method <- "Sum"
          } else if(grepl("^FirstQuartile_", first_col)) {
            agg_method <- "First Quartile"
          } else if(grepl("^ThirdQuartile_", first_col)) {
            agg_method <- "Third Quartile"
          } else if(grepl("^Min_", first_col)) {
            agg_method <- "Min"
          } else if(grepl("^Max_", first_col)) {
            agg_method <- "Max"
          } else {
            agg_method <- "Value"
          }
        }
        
        if(is_stat_function) {
          # For statistical functions, create a single row table
          header_row1 <- htmltools::tags$tr(
            lapply(disag_levels, function(level) {
              htmltools::tags$th(level, colspan = 3, style = "text-align: center; background-color: #f8f9fa; border: 1px solid #dee2e6; font-weight: bold; padding: 8px;")
            })
          )
          
          header_row2 <- htmltools::tags$tr(
            lapply(disag_levels, function(level) {
              list(
                htmltools::tags$th(agg_method, style = "text-align: center; background-color: #f8f9fa; border: 1px solid #dee2e6; font-weight: bold; padding: 8px;"),
                htmltools::tags$th("Count", style = "text-align: center; background-color: #f8f9fa; border: 1px solid #dee2e6; font-weight: bold; padding: 8px;"),
                htmltools::tags$th("Valid", style = "text-align: center; background-color: #f8f9fa; border: 1px solid #dee2e6; font-weight: bold; padding: 8px;")
              )
            })
          )
          
          # Create single data row for statistical functions
          data_rows <- list(
            htmltools::tags$tr(
              lapply(disag_levels, function(level) {
                # Find the correct column name for this aggregation method
                agg_col <- paste0(agg_method, "_", level)
                if(agg_method == "First Quartile") {
                  agg_col <- paste0("FirstQuartile_", level)
                } else if(agg_method == "Third Quartile") {
                  agg_col <- paste0("ThirdQuartile_", level)
                }
                
                count_col <- paste0("Count_", level)
                valid_col <- paste0("Valid_", level)
                
                list(
                  htmltools::tags$td(if(agg_col %in% names(data)) data[1, agg_col] else "", 
                                   style = "text-align: center; border: 1px solid #dee2e6; padding: 8px;"),
                  htmltools::tags$td(if(count_col %in% names(data)) data[1, count_col] else "", 
                                   style = "text-align: center; border: 1px solid #dee2e6; padding: 8px;"),
                  htmltools::tags$td(if(valid_col %in% names(data)) data[1, valid_col] else "", 
                                   style = "text-align: center; border: 1px solid #dee2e6; padding: 8px;")
                )
              })
            )
          )
          
        } else {
          # For percentage-based functions (single/multi select)
          response_col <- col_names[1]
          
          header_row1 <- htmltools::tags$tr(
            htmltools::tags$th(response_col, rowspan = 2, style = "text-align: left; vertical-align: middle; background-color: #f8f9fa; border: 1px solid #dee2e6; font-weight: bold; padding: 8px;"),
            lapply(disag_levels, function(level) {
              htmltools::tags$th(level, colspan = 3, style = "text-align: center; background-color: #f8f9fa; border: 1px solid #dee2e6; font-weight: bold; padding: 8px;")
            })
          )
          
          header_row2 <- htmltools::tags$tr(
            lapply(disag_levels, function(level) {
              list(
                htmltools::tags$th("Percentage", style = "text-align: center; background-color: #f8f9fa; border: 1px solid #dee2e6; font-weight: bold; padding: 8px;"),
                htmltools::tags$th("Count", style = "text-align: center; background-color: #f8f9fa; border: 1px solid #dee2e6; font-weight: bold; padding: 8px;"),
                htmltools::tags$th("Valid", style = "text-align: center; background-color: #f8f9fa; border: 1px solid #dee2e6; font-weight: bold; padding: 8px;")
              )
            })
          )
          
          # Create data rows for percentage-based functions
          data_rows <- lapply(1:nrow(data), function(i) {
            htmltools::tags$tr(
              htmltools::tags$td(data[i, 1], style = "text-align: left; border: 1px solid #dee2e6; padding: 8px;"),
              lapply(disag_levels, function(level) {
                perc_col <- paste0("Percentage_", level)
                count_col <- paste0("Count_", level)
                valid_col <- paste0("Valid_", level)
                
                list(
                  htmltools::tags$td(if(perc_col %in% names(data)) data[i, perc_col] else "", 
                                   style = "text-align: center; border: 1px solid #dee2e6; padding: 8px;"),
                  htmltools::tags$td(if(count_col %in% names(data)) data[i, count_col] else "", 
                                   style = "text-align: center; border: 1px solid #dee2e6; padding: 8px;"),
                  htmltools::tags$td(if(valid_col %in% names(data)) data[i, valid_col] else "", 
                                   style = "text-align: center; border: 1px solid #dee2e6; padding: 8px;")
                )
              })
            )
          })
        }
        
        # Create the complete table
        html_table <- htmltools::tags$div(
          htmltools::tags$h3(title, style = "margin-bottom: 20px;"),
          htmltools::tags$table(
            style = "border-collapse: collapse; width: 100%; margin-bottom: 20px;",
            htmltools::tags$thead(
              header_row1,
              header_row2
            ),
            htmltools::tags$tbody(data_rows)
          )
        )
        
        # Display the HTML table only if requested
        if(display) {
          htmltools::html_print(html_table)
        }
        return(html_table)
        
      } else {
        # Fallback to regular DT table
        dt_table <- DT::datatable(
          data,
          caption = title,
          extensions = c('Buttons'),
          options = list(
            dom = 'Bfrtip',
            buttons = list(
              list(extend = 'excel', filename = filename),
              list(extend = 'csv', filename = filename),
              list(extend = 'pdf', filename = filename),
              'copy', 'print'
            ),
            pageLength = 25,
            lengthMenu = c(10, 25, 50, 100),
            searchHighlight = TRUE,
            language = list(
              search = "Search:",
              lengthMenu = "Show _MENU_ entries",
              info = "Showing _START_ to _END_ of _TOTAL_ entries",
              paginate = list(
                first = "First",
                last = "Last", 
                `next` = "Next",
                previous = "Previous"
              )
            ),
            columnDefs = list(
              list(className = 'dt-center', targets = '_all'),
              list(className = 'dt-left', targets = 0)
            )
          ),
          class = 'display nowrap compact',
          rownames = FALSE,
          filter = 'top'
        )
        # Display the table only if requested
        if(display) {
          print(dt_table)
        }
        return(dt_table)
      }
      
    } else {
      # Regular DT table for long format
      dt_table <- DT::datatable(
        data,
        caption = title,
        extensions = c('Buttons'),
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            list(extend = 'excel', filename = filename),
            list(extend = 'csv', filename = filename),
            list(extend = 'pdf', filename = filename),
            'copy', 'print'
          ),
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100),
          searchHighlight = TRUE,
          language = list(
            search = "Search:",
            lengthMenu = "Show _MENU_ entries",
            info = "Showing _START_ to _END_ of _TOTAL_ entries",
            paginate = list(
              first = "First",
              last = "Last", 
              `next` = "Next",
              previous = "Previous"
            )
          )
        ),
        class = 'display nowrap compact',
        rownames = FALSE,
        filter = 'top'
      )
    }
    
    # Display the table in Viewer pane only if requested
    if(display) {
      print(dt_table)
    }
    return(dt_table)
  } else {
    # Fallback to basic HTML table
    if(requireNamespace("htmltools", quietly = TRUE)) {
      html_content <- htmltools::tags$div(
        htmltools::tags$h3(title),
        htmltools::tags$p("DT package not available. Install DT package for interactive tables."),
        htmltools::tags$table(
          htmltools::tags$thead(
            htmltools::tags$tr(
              lapply(names(data), function(col) {
                htmltools::tags$th(col)
              })
            )
          ),
          htmltools::tags$tbody(
            lapply(1:nrow(data), function(i) {
              htmltools::tags$tr(
                lapply(data[i, ], function(cell) {
                  htmltools::tags$td(as.character(cell))
                })
              )
            })
          )
        )
      )
      htmltools::html_print(html_content)
    } else {
      View(data)
    }
    return(NULL)
  }
}

#' Create Wide Format Header for DT Table
#'
#' Helper function to create merged headers for wide format DT tables.
#' 
#' @param data Data frame in wide format
#' @return HTML table container with merged headers
create_wide_format_header <- function(data) {
  
  if(requireNamespace("htmltools", quietly = TRUE)) {
    # Extract column names
    col_names <- names(data)
    
    # Find Response column (first column)
    response_col <- col_names[1]
    
    # Find disaggregation levels by looking for columns with _
    disag_cols <- col_names[grepl("_", col_names)]
    
    # Extract unique disaggregation levels
    disag_levels <- unique(gsub(".*_(.*)$", "\\1", disag_cols))
    
    # Create header rows
    header_row1 <- htmltools::tags$tr(
      htmltools::tags$th(response_col, rowspan = 2, style = "text-align: center; vertical-align: middle;"),
      lapply(disag_levels, function(level) {
        htmltools::tags$th(level, colspan = 2, style = "text-align: center;")
      })
    )
    
    header_row2 <- htmltools::tags$tr(
      lapply(disag_levels, function(level) {
        list(
          htmltools::tags$th("Percentage", style = "text-align: center;"),
          htmltools::tags$th("Count", style = "text-align: center;")
        )
      })
    )
    
    # Create table container
    container <- htmltools::tags$table(
      htmltools::tags$thead(
        header_row1,
        header_row2
      ),
      htmltools::tags$tbody()
    )
    
    return(container)
  } else {
    return(NULL)
  }
}

#' Add Percentage Signs to Data
#'
#' Helper function to add "%" signs to percentage columns.
#' 
#' @param data Data frame
#' @param aggregation_method Character string indicating the aggregation method
#' @return Data frame with percentage signs added
add_percentage_signs <- function(data, aggregation_method) {
  
  if(aggregation_method == "perc") {
    # Find percentage columns
    perc_cols <- names(data)[grepl("Percentage", names(data))]
    
    for(col in perc_cols) {
      if(is.numeric(data[[col]])) {
        data[[col]] <- paste0(data[[col]], "%")
      }
    }
  }
  
  return(data)
}



