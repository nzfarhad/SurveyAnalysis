#' Visualization Functions for Survey Analysis
#'
#' Collection of functions to create appropriate visualizations based on analysis type
#' and disaggregation. Handles cases with many categories and uses a consistent color theme.
#'
#' @param data A data frame containing analysis results
#' @param analysis_type Character string specifying the type of analysis ("perc", "mean", "median", "sum", etc.)
#' @param title Character string for the chart title
#' @param max_categories Maximum number of categories to display (default: 10)
#' @param color_primary Primary color (default: "#730202")
#' @param color_secondary Secondary color (default: "#f27304")
#' @param original_data Optional original survey data for box plots (mean/median only)
#' @param ques Optional question column name for box plots
#' @param disag Optional disaggregation variable name for box plots
#' @param chart_type Chart type ("auto", "column", "bar", "pie") (default: "auto")
#' @param max_label_length Maximum length for truncated labels (default: 12)
#' @param font_sizes List containing font sizes for plot elements (default: list(plot_title = 12, legend_title = 10, legend_text = 10, geom_text = 3, axis_title = 10))
#'
#' @return A ggplot2 object or NULL if no visualization is appropriate
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' sample_data <- data.frame(
#'   Response = c("Option A", "Option B", "Option C"),
#'   Percentage = c(45, 35, 20),
#'   Count = c(45, 35, 20)
#' )
#' 
#' # Create percentage visualization
#' viz <- create_visualization(sample_data, "perc", "Survey Results")
#' print(viz)
#' }
#'
#' @export
create_visualization <- function(data, analysis_type, title, max_categories = 10, 
                                color_primary = "#730202", color_secondary = "#f27304",
                                original_data = NULL, ques = NULL, disag = NULL, chart_type = "auto", max_label_length = 12,
                                font_sizes = list(plot_title = 12, legend_title = 10, legend_text = 10, geom_text = 3, axis_title = 10)) {
  
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 package is required for visualizations. Install ggplot2 to use this feature.")
    return(NULL)
  }
  
  # Check if data is empty or has no rows
  if (is.null(data) || nrow(data) == 0) {
    warning("No data available for visualization")
    return(NULL)
  }
  
  # Determine if this is disaggregated data
  # Check for wide format (columns with underscores) or explicit disaggregation columns
  is_disaggregated <- any(grepl("_[A-Za-z]", names(data))) || 
                     any(grepl("DisaggLevel|Level|Region|Province", names(data), ignore.case = TRUE)) ||
                     (any(grepl("Percentage_|Mean_|Median_|Sum_|Min_|Max_", names(data))))
  
  # Handle different analysis types
  if (analysis_type %in% c("perc", "proportion", "percentage")) {
    return(create_percentage_visualization(data, title, max_categories, 
                                         color_primary, color_secondary, is_disaggregated, chart_type, max_label_length, font_sizes))
  } else if (analysis_type %in% c("mean", "median")) {
    # For mean and median, try to create box plots if original data is available
    if (!is.null(original_data) && !is.null(ques)) {
      return(create_box_plot(original_data, ques, disag, analysis_type, title, 
                           color_primary, color_secondary, font_sizes))
    } else {
      # Fall back to regular statistical charts
      return(create_statistical_visualization(data, analysis_type, title, max_categories,
                                            color_primary, color_secondary, is_disaggregated, font_sizes))
    }
  } else if (analysis_type %in% c("sum", "min", "max", "1stq", "3rdq")) {
    return(create_statistical_visualization(data, analysis_type, title, max_categories,
                                          color_primary, color_secondary, is_disaggregated, font_sizes))
  } else {
    warning(paste("Visualization not implemented for analysis type:", analysis_type))
    return(NULL)
  }
}

#' Create Percentage-Based Visualizations
#'
#' Creates appropriate visualizations for percentage-based analysis (single-select, multi-select)
#'
#' @param data A data frame containing percentage analysis results
#' @param title Character string for the chart title
#' @param max_categories Maximum number of categories to display
#' @param color_primary Primary color
#' @param color_secondary Secondary color
#' @param is_disaggregated Whether the data is disaggregated
#'
#' @return A ggplot2 object
#'
#' @export
create_percentage_visualization <- function(data, title, max_categories, 
                                          color_primary, color_secondary, is_disaggregated, chart_type = "auto", max_label_length = 12, font_sizes = list(plot_title = 12, legend_title = 10, legend_text = 10, geom_text = 3, axis_title = 10)) {
  
  # Check if we have percentage data
  perc_cols <- names(data)[grepl("Percentage|perc", names(data), ignore.case = TRUE)]
  if (length(perc_cols) == 0) {
    warning("No percentage columns found in data")
    return(NULL)
  }
  
  if (is_disaggregated) {
    return(create_disaggregated_percentage_chart(data, title, max_categories, 
                                               color_primary, color_secondary, chart_type, max_label_length, font_sizes))
  } else {
    return(create_simple_percentage_chart(data, title, max_categories, 
                                        color_primary, color_secondary, chart_type, max_label_length, font_sizes))
  }
}

#' Create Simple Percentage Chart (No Disaggregation)
#'
#' @param data A data frame containing percentage analysis results
#' @param title Character string for the chart title
#' @param max_categories Maximum number of categories to display
#' @param color_primary Primary color
#' @param color_secondary Secondary color
#'
#' @return A ggplot2 object
#'
#' @export
create_simple_percentage_chart <- function(data, title, max_categories, 
                                         color_primary, color_secondary, chart_type = "auto", max_label_length = 12, font_sizes = list(plot_title = 12, legend_title = 10, legend_text = 10, geom_text = 3, axis_title = 10)) {
  
  # Find the percentage column
  perc_col <- names(data)[grepl("Percentage|perc", names(data), ignore.case = TRUE)][1]
  response_col <- names(data)[grepl("Response|Var1", names(data), ignore.case = TRUE)][1]
  
  if (is.null(perc_col) || is.null(response_col)) {
    warning("Required columns not found for percentage visualization")
    return(NULL)
  }
  
  # Convert percentage strings to numeric for visualization
  data <- convert_percentage_to_numeric(data, perc_col)
  
  # Remove percentage signs for numeric operations
  data_for_plot <- data
  
  # Sort data by percentage (largest to smallest) for better visualization
  data_for_plot <- data_for_plot[order(data_for_plot[[perc_col]], decreasing = TRUE), ]
  
  # Limit categories if too many (AFTER sorting to get top categories by percentage)
  if (nrow(data_for_plot) > max_categories) {
    data_for_plot <- data_for_plot[1:max_categories, ]
    title <- paste0(title, " (Top ", max_categories, " Categories)")
  }
  
  # Truncate long labels more aggressively for better spacing (BEFORE factor ordering)
  data_for_plot[[response_col]] <- truncate_labels(data_for_plot[[response_col]], max_label_length)
  
  # Fix factor ordering - explicitly set factor levels to maintain sort order
  # Ensure unique levels to avoid duplicates after truncation
  unique_levels <- unique(data_for_plot[[response_col]])
  data_for_plot[[response_col]] <- factor(
    data_for_plot[[response_col]],
    levels = unique_levels
  )
  
  # Create color palette
  n_categories <- nrow(data_for_plot)
  colors <- create_color_palette(n_categories, color_primary, color_secondary)
  
  # Determine chart type based on user preference or automatic selection
  if (chart_type == "pie" || (chart_type == "auto" && n_categories < 5)) {
    # Create pie chart
    p <- ggplot2::ggplot(data_for_plot, ggplot2::aes(x = 1, y = !!ggplot2::sym(perc_col), fill = !!ggplot2::sym(response_col))) +
      ggplot2::geom_col(width = 1) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::geom_text(ggplot2::aes(label = paste0(!!ggplot2::sym(perc_col), "%", "\n", !!ggplot2::sym(response_col))), 
                        position = ggplot2::position_stack(vjust = 0.5), 
                        size = font_sizes$geom_text, fontface = "bold") +
      ggplot2::labs(
        title = title,
        fill = "Response"
      ) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = font_sizes$plot_title, face = "bold", hjust = 0.5),
        legend.title = ggplot2::element_text(size = font_sizes$legend_title, face = "bold"),
        legend.text = ggplot2::element_text(size = font_sizes$legend_text),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1))
    
    return(p)
  } else if (chart_type == "bar" || (chart_type == "auto" && n_categories > 10)) {
    # Create horizontal bar chart
    p <- ggplot2::ggplot(data_for_plot, ggplot2::aes_string(x = perc_col, y = response_col)) +
      ggplot2::geom_col(fill = colors, color = "white", size = 0.5) +
      ggplot2::geom_text(ggplot2::aes_string(label = paste0("paste(", perc_col, ", '%')")), 
                        hjust = -0.1, size = font_sizes$geom_text, fontface = "bold") +
      ggplot2::labs(
        title = title,
        x = "Percentage (%)",
        y = "Response"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = font_sizes$plot_title, face = "bold", hjust = 0.5),
        axis.text.x = ggplot2::element_text(size = 10),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = font_sizes$axis_title),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      ) +
      ggplot2::scale_x_continuous(limits = c(0, max(data_for_plot[[perc_col]], na.rm = TRUE) * 1.1)) +
      ggplot2::scale_y_discrete(limits = rev)
  } else {
    # Create vertical column chart (default)
    p <- ggplot2::ggplot(data_for_plot, ggplot2::aes_string(x = response_col, y = perc_col)) +
      ggplot2::geom_col(fill = colors, color = "white", size = 0.5) +
      ggplot2::geom_text(ggplot2::aes_string(label = paste0("paste(", perc_col, ", '%')")), 
                        vjust = -0.5, size = font_sizes$geom_text, fontface = "bold") +
      ggplot2::labs(
        title = title,
        x = "Response",
        y = "Percentage (%)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = font_sizes$plot_title, face = "bold", hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size = 9, vjust = 1),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = font_sizes$axis_title),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      ) +
      ggplot2::scale_y_continuous(limits = c(0, max(data_for_plot[[perc_col]], na.rm = TRUE) * 1.1))
  }
  
  return(p)
}

#' Create Disaggregated Percentage Chart
#'
#' @param data A data frame containing disaggregated percentage analysis results
#' @param title Character string for the chart title
#' @param max_categories Maximum number of categories to display
#' @param color_primary Primary color
#' @param color_secondary Secondary color
#'
#' @return A ggplot2 object
#'
#' @export
create_disaggregated_percentage_chart <- function(data, title, max_categories, 
                                                color_primary, color_secondary, chart_type = "auto", max_label_length = 12, font_sizes = list(plot_title = 12, legend_title = 10, legend_text = 10, geom_text = 3, axis_title = 10)) {
  
  # Check if this is wide format data
  is_wide_format <- any(grepl("_", names(data)))
  
  if (is_wide_format) {
    return(create_wide_format_percentage_chart(data, title, max_categories, 
                                             color_primary, color_secondary, chart_type, max_label_length, font_sizes))
  } else {
    return(create_long_format_percentage_chart(data, title, max_categories, 
                                             color_primary, color_secondary, chart_type, max_label_length, font_sizes))
  }
}

#' Create Wide Format Percentage Chart
#'
#' @param data A data frame containing wide format percentage analysis results
#' @param title Character string for the chart title
#' @param max_categories Maximum number of categories to display
#' @param color_primary Primary color
#' @param color_secondary Secondary color
#'
#' @return A ggplot2 object
#'
#' @export
create_wide_format_percentage_chart <- function(data, title, max_categories, 
                                              color_primary, color_secondary, chart_type = "auto", max_label_length = 12, font_sizes = list(plot_title = 12, legend_title = 10, legend_text = 10, geom_text = 3, axis_title = 10)) {
  
  # Extract percentage columns
  perc_cols <- names(data)[grepl("Percentage_", names(data))]
  response_col <- names(data)[grepl("Response|Var1", names(data), ignore.case = TRUE)][1]
  
  if (length(perc_cols) == 0 || is.null(response_col)) {
    warning("Required columns not found for wide format percentage visualization")
    return(NULL)
  }
  
  # Convert percentage strings to numeric for visualization
  for (col in perc_cols) {
    data <- convert_percentage_to_numeric(data, col)
  }
  
  # Truncate long labels for better visualization
  data[[response_col]] <- truncate_labels(data[[response_col]], max_label_length)
  
  # Remove percentage signs for numeric operations
  plot_data <- data.frame()
  
  for (col in perc_cols) {
    level_name <- gsub("Percentage_", "", col)
    temp_data <- data.frame(
      Response = data[[response_col]],
      Percentage = data[[col]],  # Already converted to numeric by helper function
      Level = level_name,
      stringsAsFactors = FALSE
    )
    plot_data <- rbind(plot_data, temp_data)
  }
  
  # Remove rows with NA percentages
  plot_data <- plot_data[!is.na(plot_data$Percentage), ]
  
  # Sort data by percentage (largest to smallest) for better visualization
  # For grouped data, sort by the maximum percentage within each response category
  plot_data$max_percentage <- ave(plot_data$Percentage, plot_data$Response, FUN = max)
  plot_data <- plot_data[order(plot_data$max_percentage, decreasing = TRUE), ]
  plot_data$max_percentage <- NULL  # Remove temporary column
  
  # Limit categories if too many (AFTER sorting to get top categories by percentage)
  unique_responses <- unique(plot_data$Response)
  if (length(unique_responses) > max_categories) {
    top_responses <- unique_responses[1:max_categories]
    plot_data <- plot_data[plot_data$Response %in% top_responses, ]
    title <- paste0(title, " (Top ", max_categories, " Categories)")
  }
  
  # Fix factor ordering - explicitly set factor levels to maintain sort order
  # Ensure unique levels to avoid duplicates after truncation
  unique_levels <- unique(plot_data$Response)
  plot_data$Response <- factor(
    plot_data$Response,
    levels = unique_levels
  )
  
  # Create color palette
  n_levels <- length(unique(plot_data$Level))
  n_responses <- length(unique(plot_data$Response))
  
  # Determine chart type based on user preference or automatic selection
  if (chart_type == "pie" || (chart_type == "auto" && n_responses < 5)) {
    # For pie charts with disaggregated data, we'll create a grouped pie chart
    # This is complex, so we'll fall back to grouped bar chart
    chart_type <- "column"
  }
  
  if (chart_type == "bar" || (chart_type == "auto" && n_responses > 10)) {
    # Create horizontal grouped bar chart
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Percentage, y = Response, fill = Level)) +
      ggplot2::geom_col(position = "dodge", color = "white", size = 0.5) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(Percentage, "%")), 
                        position = ggplot2::position_dodge(width = 0.9), 
                        hjust = -0.1, size = font_sizes$geom_text, fontface = "bold") +
      ggplot2::labs(
        title = title,
        x = "Percentage (%)",
        y = "Response",
        fill = "Disaggregation Level"
      ) +
      ggplot2::scale_fill_manual(values = create_color_palette(n_levels, color_primary, color_secondary)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = font_sizes$plot_title, face = "bold", hjust = 0.5),
        axis.text.x = ggplot2::element_text(size = 10),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = font_sizes$axis_title),
        legend.title = ggplot2::element_text(size = font_sizes$legend_title, face = "bold"),
        legend.text = ggplot2::element_text(size = font_sizes$legend_text),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      ) +
      ggplot2::scale_x_continuous(limits = c(0, max(plot_data$Percentage, na.rm = TRUE) * 1.1)) +
      ggplot2::scale_y_discrete(limits = rev)
  } else {
    # Create vertical grouped column chart (default)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Response, y = Percentage, fill = Level)) +
      ggplot2::geom_col(position = "dodge", color = "white", size = 0.5) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(Percentage, "%")), 
                        position = ggplot2::position_dodge(width = 0.9), 
                        vjust = -0.5, size = font_sizes$geom_text, fontface = "bold") +
      ggplot2::labs(
        title = title,
        x = "Response",
        y = "Percentage (%)",
        fill = "Disaggregation Level"
      ) +
      ggplot2::scale_fill_manual(values = create_color_palette(n_levels, color_primary, color_secondary)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = font_sizes$plot_title, face = "bold", hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size = 9, vjust = 1),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = font_sizes$axis_title),
        legend.title = ggplot2::element_text(size = font_sizes$legend_title, face = "bold"),
        legend.text = ggplot2::element_text(size = font_sizes$legend_text),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      ) +
      ggplot2::scale_y_continuous(limits = c(0, max(plot_data$Percentage, na.rm = TRUE) * 1.1))
  }
  
  return(p)
}

#' Create Long Format Percentage Chart
#'
#' @param data A data frame containing long format percentage analysis results
#' @param title Character string for the chart title
#' @param max_categories Maximum number of categories to display
#' @param color_primary Primary color
#' @param color_secondary Secondary color
#'
#' @return A ggplot2 object
#'
#' @export
create_long_format_percentage_chart <- function(data, title, max_categories, 
                                               color_primary, color_secondary, chart_type = "auto", max_label_length = 12, font_sizes = list(plot_title = 12, legend_title = 10, legend_text = 10, geom_text = 3, axis_title = 10)) {
  
  # Find relevant columns
  perc_col <- names(data)[grepl("Percentage|perc", names(data), ignore.case = TRUE)][1]
  response_col <- names(data)[grepl("Response|Var1", names(data), ignore.case = TRUE)][1]
  level_col <- names(data)[grepl("Level|Disagg|Region|Province", names(data), ignore.case = TRUE)][1]
  
  if (is.null(perc_col) || is.null(response_col) || is.null(level_col)) {
    warning("Required columns not found for long format percentage visualization")
    return(NULL)
  }
  
  # Convert percentage strings to numeric for visualization
  data <- convert_percentage_to_numeric(data, perc_col)
  
  # Truncate long labels for better visualization
  data[[response_col]] <- truncate_labels(data[[response_col]], max_label_length)
  data[[level_col]] <- truncate_labels(data[[level_col]], max_label_length)
  
  # Remove percentage signs for numeric operations
  data_for_plot <- data
  
  # Sort data by percentage (largest to smallest) for better visualization
  # For grouped data, sort by the maximum percentage within each response category
  data_for_plot$max_percentage <- ave(data_for_plot[[perc_col]], data_for_plot[[response_col]], FUN = max)
  data_for_plot <- data_for_plot[order(data_for_plot$max_percentage, decreasing = TRUE), ]
  data_for_plot$max_percentage <- NULL  # Remove temporary column
  
  # Limit categories if too many (AFTER sorting to get top categories by percentage)
  unique_responses <- unique(data_for_plot[[response_col]])
  if (length(unique_responses) > max_categories) {
    top_responses <- unique_responses[1:max_categories]
    data_for_plot <- data_for_plot[data_for_plot[[response_col]] %in% top_responses, ]
    title <- paste0(title, " (Top ", max_categories, " Categories)")
  }
  
  # Fix factor ordering - explicitly set factor levels to maintain sort order
  # Ensure unique levels to avoid duplicates after truncation
  unique_levels <- unique(data_for_plot[[response_col]])
  data_for_plot[[response_col]] <- factor(
    data_for_plot[[response_col]],
    levels = unique_levels
  )
  
  # Create color palette
  n_levels <- length(unique(data_for_plot[[level_col]]))
  n_responses <- length(unique(data_for_plot[[response_col]]))
  
  # Determine chart type based on user preference or automatic selection
  if (chart_type == "pie" || (chart_type == "auto" && n_responses < 5)) {
    # For pie charts with disaggregated data, we'll create a grouped pie chart
    # This is complex, so we'll fall back to grouped bar chart
    chart_type <- "column"
  }
  
  if (chart_type == "bar" || (chart_type == "auto" && n_responses > 10)) {
    # Create horizontal grouped bar chart
    p <- ggplot2::ggplot(data_for_plot, ggplot2::aes_string(x = perc_col, y = response_col, fill = level_col)) +
      ggplot2::geom_col(position = "dodge", color = "white", size = 0.5) +
      ggplot2::geom_text(ggplot2::aes_string(label = paste0("paste(", perc_col, ", '%')")), 
                        position = ggplot2::position_dodge(width = 0.9), 
                        hjust = -0.1, size = font_sizes$geom_text, fontface = "bold") +
      ggplot2::labs(
        title = title,
        x = "Percentage (%)",
        y = "Response",
        fill = stringr::str_to_title(level_col)
      ) +
      ggplot2::scale_fill_manual(values = create_color_palette(n_levels, color_primary, color_secondary)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = font_sizes$plot_title, face = "bold", hjust = 0.5),
        axis.text.x = ggplot2::element_text(size = 10),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = font_sizes$axis_title),
        legend.title = ggplot2::element_text(size = font_sizes$legend_title, face = "bold"),
        legend.text = ggplot2::element_text(size = font_sizes$legend_text),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      ) +
      ggplot2::scale_x_continuous(limits = c(0, max(data_for_plot[[perc_col]], na.rm = TRUE) * 1.1)) +
      ggplot2::scale_y_discrete(limits = rev)
  } else {
    # Create vertical grouped column chart (default)
    p <- ggplot2::ggplot(data_for_plot, ggplot2::aes_string(x = response_col, y = perc_col, fill = level_col)) +
      ggplot2::geom_col(position = "dodge", color = "white", size = 0.5) +
      ggplot2::geom_text(ggplot2::aes_string(label = paste0("paste(", perc_col, ", '%')")), 
                        position = ggplot2::position_dodge(width = 0.9), 
                        vjust = -0.5, size = font_sizes$geom_text, fontface = "bold") +
      ggplot2::labs(
        title = title,
        x = "Response",
        y = "Percentage (%)",
        fill = stringr::str_to_title(level_col)
      ) +
      ggplot2::scale_fill_manual(values = create_color_palette(n_levels, color_primary, color_secondary)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = font_sizes$plot_title, face = "bold", hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size = 9, vjust = 1),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = font_sizes$axis_title),
        legend.title = ggplot2::element_text(size = font_sizes$legend_title, face = "bold"),
        legend.text = ggplot2::element_text(size = font_sizes$legend_text),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      ) +
      ggplot2::scale_y_continuous(limits = c(0, max(data_for_plot[[perc_col]], na.rm = TRUE) * 1.1))
  }
  
  return(p)
}

#' Create Box Plot for Statistical Analysis
#'
#' @param data A data frame containing the original survey data
#' @param ques Character string specifying the column name of the question to analyze
#' @param disag Character string specifying the disaggregation variable name
#' @param analysis_type Character string specifying the statistical method
#' @param title Character string for the chart title
#' @param color_primary Primary color
#' @param color_secondary Secondary color
#'
#' @return A ggplot2 object
#'
#' @export
create_box_plot <- function(data, ques, disag, analysis_type, title, color_primary, color_secondary, font_sizes = list(plot_title = 12, legend_title = 10, legend_text = 10, geom_text = 3, axis_title = 10)) {
  
  # Validate inputs
  if (is.null(data) || nrow(data) == 0) {
    warning("No data provided for box plot")
    return(NULL)
  }
  
  if (!ques %in% names(data)) {
    warning(paste("Question column", ques, "not found in data"))
    return(NULL)
  }
  
  # Check if disaggregation variable exists
  if (!disag %in% names(data) || disag == "all") {
    # Single box plot for overall data
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "1", y = ques)) +
      ggplot2::geom_boxplot(fill = color_primary, color = "black", size = 0.5, alpha = 0.7) +
      ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red", color = "red") +
      ggplot2::stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "blue") +
      ggplot2::labs(
        title = title,
        x = "",
        y = ques,
        subtitle = "Red diamond = Mean, Blue circle = Median"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = font_sizes$plot_title, face = "bold", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = font_sizes$axis_title),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      )
    
    return(p)
  }
  
  # Multiple box plots for disaggregated data
  # Clean data - remove NA values
  clean_data <- data[!is.na(data[[ques]]) & !is.na(data[[disag]]), ]
  
  if (nrow(clean_data) == 0) {
    warning("No valid data remaining after removing NA values")
    return(NULL)
  }
  
  # Check unique values in disaggregation variable
  unique_levels <- unique(clean_data[[disag]])
  if (length(unique_levels) == 0) {
    warning("No unique levels found in disaggregation variable")
    return(NULL)
  }
  
  # Limit categories if too many
  if (length(unique_levels) > 20) {
    warning(paste("Too many categories (", length(unique_levels), ") in disaggregation variable '", disag, "'. Limiting to top 20 by frequency."))
    freq_table <- table(clean_data[[disag]])
    top_categories <- names(sort(freq_table, decreasing = TRUE)[1:20])
    clean_data <- clean_data[clean_data[[disag]] %in% top_categories, ]
    unique_levels <- unique(clean_data[[disag]])
    title <- paste0(title, " (Top 20 Categories)")
  }
  
  # Convert disaggregation variable to factor for proper ordering
  clean_data[[disag]] <- as.factor(clean_data[[disag]])
  
  # Create the boxplot using a simple, direct approach
  p <- ggplot2::ggplot(clean_data, ggplot2::aes_string(x = disag, y = ques)) +
    ggplot2::geom_boxplot(fill = color_primary, color = "black", size = 0.5, alpha = 0.7) +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red", color = "red") +
    ggplot2::stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "blue") +
    ggplot2::labs(
      title = title,
      x = disag,
      y = ques,
      subtitle = "Red diamond = Mean, Blue circle = Median"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = font_sizes$plot_title, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = font_sizes$axis_title),
      legend.position = "none",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )
  
  return(p)
}

#' Create Statistical Visualizations
#'
#' Creates appropriate visualizations for statistical analysis (mean, median, sum, etc.)
#'
#' @param data A data frame containing statistical analysis results
#' @param analysis_type Character string specifying the statistical method
#' @param title Character string for the chart title
#' @param max_categories Maximum number of categories to display
#' @param color_primary Primary color
#' @param color_secondary Secondary color
#' @param is_disaggregated Whether the data is disaggregated
#'
#' @return A ggplot2 object
#'
#' @export
create_statistical_visualization <- function(data, analysis_type, title, max_categories, 
                                           color_primary, color_secondary, is_disaggregated, font_sizes = list(plot_title = 12, legend_title = 10, legend_text = 10, geom_text = 3, axis_title = 10)) {
  
  # For mean and median, create box plots if we have access to original data
  if (analysis_type %in% c("mean", "median")) {
    # Note: This would require access to original data, which we don't have in this context
    # For now, we'll create regular bar charts but this could be enhanced to accept original data
    warning("Box plots for mean/median require original data. Creating bar chart instead.")
  }
  
  if (is_disaggregated) {
    return(create_disaggregated_statistical_chart(data, analysis_type, title, max_categories,
                                                color_primary, color_secondary))
  } else {
    return(create_simple_statistical_chart(data, analysis_type, title, max_categories,
                                         color_primary, color_secondary))
  }
}

#' Create Simple Statistical Chart (No Disaggregation)
#'
#' @param data A data frame containing statistical analysis results
#' @param analysis_type Character string specifying the statistical method
#' @param title Character string for the chart title
#' @param max_categories Maximum number of categories to display
#' @param color_primary Primary color
#' @param color_secondary Secondary color
#'
#' @return A ggplot2 object
#'
#' @export
create_simple_statistical_chart <- function(data, analysis_type, title, max_categories,
                                          color_primary, color_secondary, font_sizes = list(plot_title = 12, legend_title = 10, legend_text = 10, geom_text = 3, axis_title = 10)) {
  
  # Find the result column based on analysis type
  result_col <- names(data)[grepl(paste0("^", stringr::str_to_title(analysis_type), "$"), names(data))]
  if (length(result_col) == 0) {
    # Fallback to any column that might contain the result
    result_col <- names(data)[grepl("Mean|Median|Sum|Min|Max|First|Third", names(data))][1]
  }
  
  if (is.null(result_col)) {
    warning("Result column not found for statistical visualization")
    return(NULL)
  }
  
  # For single value results, create a simple bar chart
  if (nrow(data) == 1) {
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "1", y = result_col)) +
      ggplot2::geom_col(fill = color_primary, color = "white", size = 0.5) +
      ggplot2::geom_text(ggplot2::aes_string(label = result_col), 
                        vjust = -0.5, size = font_sizes$geom_text, fontface = "bold") +
      ggplot2::labs(
        title = title,
        x = "",
        y = stringr::str_to_title(analysis_type)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = font_sizes$plot_title, face = "bold", hjust = 0.5),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = font_sizes$axis_title),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      )
    
    return(p)
  }
  
  # For multiple values, create a bar chart
  response_col <- names(data)[grepl("Response|Var1", names(data), ignore.case = TRUE)][1]
  
  if (!is.null(response_col)) {
    # Limit categories if too many
    if (nrow(data) > max_categories) {
      data <- data[1:max_categories, ]
      title <- paste0(title, " (Top ", max_categories, " Categories)")
    }
    
    # Create color palette
    n_categories <- nrow(data)
    if (n_categories == 1) {
      colors <- color_primary
    } else if (n_categories == 2) {
      colors <- c(color_primary, color_secondary)
    } else {
      colors <- colorRampPalette(c(color_primary, color_secondary))(n_categories)
    }
    
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = response_col, y = result_col)) +
      ggplot2::geom_col(fill = colors, color = "white", size = 0.5) +
      ggplot2::geom_text(ggplot2::aes_string(label = result_col), 
                        vjust = -0.5, size = font_sizes$geom_text, fontface = "bold") +
      ggplot2::labs(
        title = title,
        x = "Response",
        y = stringr::str_to_title(analysis_type)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = font_sizes$plot_title, face = "bold", hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size = 9, vjust = 1),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = font_sizes$axis_title),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      )
    
    return(p)
  }
  
  return(NULL)
}

#' Create Disaggregated Statistical Chart
#'
#' @param data A data frame containing disaggregated statistical analysis results
#' @param analysis_type Character string specifying the statistical method
#' @param title Character string for the chart title
#' @param max_categories Maximum number of categories to display
#' @param color_primary Primary color
#' @param color_secondary Secondary color
#'
#' @return A ggplot2 object
#'
#' @export
create_disaggregated_statistical_chart <- function(data, analysis_type, title, max_categories,
                                                 color_primary, color_secondary) {
  
  # Check if this is wide format data
  is_wide_format <- any(grepl("_", names(data)))
  
  if (is_wide_format) {
    return(create_wide_format_statistical_chart(data, analysis_type, title, max_categories,
                                              color_primary, color_secondary))
  } else {
    return(create_long_format_statistical_chart(data, analysis_type, title, max_categories,
                                              color_primary, color_secondary))
  }
}

#' Create Wide Format Statistical Chart
#'
#' @param data A data frame containing wide format statistical analysis results
#' @param analysis_type Character string specifying the statistical method
#' @param title Character string for the chart title
#' @param max_categories Maximum number of categories to display
#' @param color_primary Primary color
#' @param color_secondary Secondary color
#'
#' @return A ggplot2 object
#'
#' @export
create_wide_format_statistical_chart <- function(data, analysis_type, title, max_categories,
                                               color_primary, color_secondary) {
  
  # Find result columns based on analysis type
  analysis_title <- stringr::str_to_title(analysis_type)
  if (analysis_type == "1stq") analysis_title <- "FirstQuartile"
  if (analysis_type == "3rdq") analysis_title <- "ThirdQuartile"
  
  result_cols <- names(data)[grepl(paste0("^", analysis_title, "_"), names(data))]
  
  if (length(result_cols) == 0) {
    warning("Result columns not found for wide format statistical visualization")
    return(NULL)
  }
  
  # Reshape data for plotting
  plot_data <- data.frame()
  
  for (col in result_cols) {
    level_name <- gsub(paste0("^", analysis_title, "_"), "", col)
    temp_data <- data.frame(
      Level = level_name,
      Value = data[[col]],
      stringsAsFactors = FALSE
    )
    plot_data <- rbind(plot_data, temp_data)
  }
  
  # Remove rows with NA values
  plot_data <- plot_data[!is.na(plot_data$Value), ]
  
  # Create color palette
  n_levels <- nrow(plot_data)
  if (n_levels == 1) {
    colors <- color_primary
  } else if (n_levels == 2) {
    colors <- c(color_primary, color_secondary)
  } else {
    colors <- colorRampPalette(c(color_primary, color_secondary))(n_levels)
  }
  
  # Create bar chart
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Level, y = Value, fill = Level)) +
    ggplot2::geom_col(color = "white", size = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = Value), 
                      vjust = -0.5, size = 4, fontface = "bold") +
    ggplot2::labs(
      title = title,
      x = "Disaggregation Level",
      y = analysis_title
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.position = "none",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, max(plot_data$Value, na.rm = TRUE) * 1.1))
  
  return(p)
}

#' Create Long Format Statistical Chart
#'
#' @param data A data frame containing long format statistical analysis results
#' @param analysis_type Character string specifying the statistical method
#' @param title Character string for the chart title
#' @param max_categories Maximum number of categories to display
#' @param color_primary Primary color
#' @param color_secondary Secondary color
#'
#' @return A ggplot2 object
#'
#' @export
create_long_format_statistical_chart <- function(data, analysis_type, title, max_categories,
                                               color_primary, color_secondary) {
  
  # Find relevant columns
  analysis_title <- stringr::str_to_title(analysis_type)
  if (analysis_type == "1stq") analysis_title <- "FirstQuartile"
  if (analysis_type == "3rdq") analysis_title <- "ThirdQuartile"
  
  result_col <- names(data)[grepl(paste0("^", analysis_title, "$"), names(data))]
  if (length(result_col) == 0) {
    result_col <- names(data)[grepl("Mean|Median|Sum|Min|Max|First|Third", names(data))][1]
  }
  
  level_col <- names(data)[grepl("Level|Disagg|Region|Province", names(data), ignore.case = TRUE)][1]
  
  if (is.null(result_col) || is.null(level_col)) {
    warning("Required columns not found for long format statistical visualization")
    return(NULL)
  }
  
  # Limit categories if too many
  if (nrow(data) > max_categories) {
    data <- data[1:max_categories, ]
    title <- paste0(title, " (Top ", max_categories, " Categories)")
  }
  
  # Create color palette
  n_levels <- length(unique(data[[level_col]]))
  if (n_levels == 1) {
    colors <- color_primary
  } else if (n_levels == 2) {
    colors <- c(color_primary, color_secondary)
  } else {
    colors <- colorRampPalette(c(color_primary, color_secondary))(n_levels)
  }
  
  # Create bar chart
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = level_col, y = result_col, fill = level_col)) +
    ggplot2::geom_col(color = "white", size = 0.5) +
    ggplot2::geom_text(ggplot2::aes_string(label = result_col), 
                      vjust = -0.5, size = 4, fontface = "bold") +
    ggplot2::labs(
      title = title,
      x = "Disaggregation Level",
      y = analysis_title
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.position = "none",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, max(data[[result_col]], na.rm = TRUE) * 1.1))
  
  return(p)
}

#' Helper function to convert percentage strings to numeric values
#'
#' @param data Data frame
#' @param perc_col Column name containing percentage values
#' @return Data frame with numeric percentage values
convert_percentage_to_numeric <- function(data, perc_col) {
  if (perc_col %in% names(data)) {
    # Convert percentage strings to numeric
    data[[perc_col]] <- as.numeric(gsub("%", "", data[[perc_col]]))
  }
  return(data)
}

#' Helper function to truncate long labels
#'
#' @param labels Vector of labels to truncate
#' @param max_length Maximum length for each label (default: 20)
#'
#' @return Vector of truncated labels
#'
#' @export
truncate_labels <- function(labels, max_length = 15) {
  # Validate inputs
  if (is.null(labels) || length(labels) == 0) {
    return(character(0))
  }
  
  # Convert to character if not already
  labels <- as.character(labels)
  
  # Handle NA values
  labels[is.na(labels)] <- "NA"
  
  result <- sapply(labels, function(label) {
    if (is.character(label) && nchar(label) > max_length) {
      paste0(substr(label, 1, max_length - 3), "...")
    } else {
      as.character(label)
    }
  })
  
  # Handle duplicates by adding numbers if needed
  if (any(duplicated(result))) {
    # Find duplicates and make them unique
    unique_result <- make.unique(result, sep = "_")
    return(unique_result)
  }
  
  return(result)
}

#' Helper function to create color palette
#'
#' @param n Number of colors needed
#' @param color_primary Primary color
#' @param color_secondary Secondary color
#'
#' @return Vector of colors
#'
#' @export
create_color_palette <- function(n, color_primary = "#730202", color_secondary = "#f27304") {
  # Extended color palette with highly distinct colors
  extended_colors <- c(
    "#730202",  # Primary: Dark red
    "#f27304",  # Secondary: Orange
    "#2E8B57",  # Sea green
    "#4169E1",  # Royal blue
    "#8B008B",  # Dark magenta
    "#FF6347",  # Tomato
    "#32CD32",  # Lime green
    "#FFD700",  # Gold
    "#DC143C",  # Crimson
    "#00CED1",  # Dark turquoise
    "#FF8C00",  # Dark orange
    "#9370DB",  # Medium purple
    "#228B22",  # Forest green
    "#FF1493",  # Deep pink
    "#1E90FF",  # Dodger blue
    "#FFA500",  # Orange
    "#8B4513",  # Saddle brown
    "#20B2AA",  # Light sea green
    "#B22222",  # Fire brick
    "#6A5ACD",  # Slate blue
    "#CD853F",  # Peru
    "#4682B4",  # Steel blue
    "#DAA520",  # Goldenrod
    "#2F4F4F",  # Dark slate gray
    "#8FBC8F"   # Dark sea green
  )
  
  if (n == 1) {
    return(color_primary)
  } else if (n == 2) {
    return(c(color_primary, color_secondary))
  } else if (n <= length(extended_colors)) {
    return(extended_colors[1:n])
  } else {
    # For more than 25 categories, create a gradient
    return(colorRampPalette(c(color_primary, color_secondary))(n))
  }
}
