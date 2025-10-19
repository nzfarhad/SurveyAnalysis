#' Generate R Markdown Template for Survey Reports
#'
#' Creates a professional R Markdown template for surveyCTO/Kobo survey analysis reports.
#' Supports two modes: placeholder template for manual coding or auto-generated code based on analysis plan.
#'
#' @param output_file Character string specifying the output file path (e.g., "survey_report.Rmd")
#' @param mode Character string specifying the template mode: "placeholder" or "analysis_plan"
#' @param analysis_plan Data frame containing analysis plan (required for "analysis_plan" mode)
#' @param org_name Character string for organization name
#' @param survey_name Character string for survey name
#' @param author Character string for report author
#' @param data_collection_start Character string for data collection start date
#' @param data_collection_end Character string for data collection end date
#' @param project_summary Character string for project summary
#' @param primary_color Character string for primary color (hex code, e.g., "#003366")
#' @param secondary_color Character string for secondary color (hex code, e.g., "#FF6B35")
#' @param multi_response_sep Character string for multi-response separator (default: "; ")
#' @param show_code Logical indicating whether to show code chunks by default (default: FALSE)
#'
#' @return Creates an R Markdown file at the specified output path
#'
#' @examples
#' \dontrun{
#' # Create placeholder template (code hidden by default)
#' generate_rmd_template(
#'   output_file = "survey_report.Rmd",
#'   mode = "placeholder",
#'   org_name = "My Organization",
#'   survey_name = "Health Facility Assessment",
#'   author = "John Doe",
#'   project_summary = "Assessment of health facilities in the region"
#' )
#' 
#' # Create template with code visible
#' generate_rmd_template(
#'   output_file = "survey_report.Rmd",
#'   mode = "placeholder",
#'   org_name = "My Organization",
#'   survey_name = "Health Facility Assessment",
#'   author = "John Doe",
#'   show_code = TRUE
#' )
#' 
#' # Create template from analysis plan
#' generate_rmd_template(
#'   output_file = "survey_report.Rmd",
#'   mode = "analysis_plan",
#'   analysis_plan = my_analysis_plan,
#'   org_name = "My Organization",
#'   survey_name = "Health Facility Assessment",
#'   author = "John Doe"
#' )
#' }
#'
#' @export
generate_rmd_template <- function(
  output_file,
  mode = c("placeholder", "analysis_plan"),
  analysis_plan = NULL,
  org_name = "Organization Name",
  survey_name = "Survey Name",
  author = "Author Name",
  data_collection_start = NULL,
  data_collection_end = NULL,
  project_summary = "Project summary goes here",
  primary_color = "#003366",
  secondary_color = "#FF6B35",
  multi_response_sep = "; ",
  show_code = FALSE
) {
  
  # Validate inputs
  mode <- match.arg(mode)
  
  if (mode == "analysis_plan" && is.null(analysis_plan)) {
    stop("analysis_plan is required when mode = 'analysis_plan'")
  }
  
  if (!is.null(analysis_plan)) {
    required_cols <- c("variable", "label", "kobo_type", "aggregation_method", "disaggregation", "sheet")
    missing_cols <- setdiff(required_cols, names(analysis_plan))
    if (length(missing_cols) > 0) {
      stop(paste("Missing required columns in analysis_plan:", paste(missing_cols, collapse = ", ")))
    }
  }
  
  # Validate color codes
  if (!grepl("^#[0-9A-Fa-f]{6}$", primary_color)) {
    stop("primary_color must be a valid hex color code (e.g., '#003366')")
  }
  if (!grepl("^#[0-9A-Fa-f]{6}$", secondary_color)) {
    stop("secondary_color must be a valid hex color code (e.g., '#FF6B35')")
  }
  
  # Generate CSS
  css_content <- generate_custom_css(primary_color, secondary_color)
  
  # Generate YAML header
  yaml_header <- generate_yaml_header(survey_name, author, css_content, show_code)
  
  # Generate document content based on mode
  if (mode == "placeholder") {
    document_content <- generate_placeholder_content(org_name, survey_name, author, 
                                                   data_collection_start, data_collection_end, 
                                                   project_summary, primary_color, secondary_color, 
                                                   multi_response_sep, show_code)
  } else {
    document_content <- generate_analysis_plan_content(analysis_plan, org_name, survey_name, author,
                                                      data_collection_start, data_collection_end,
                                                      project_summary, primary_color, secondary_color,
                                                      multi_response_sep, show_code)
  }
  
  # Combine and write to file (CSS goes after YAML header)
  full_content <- paste(yaml_header, css_content, document_content, sep = "\n")
  
  # Write to file
  cat(full_content, file = output_file)
  
  message(paste("R Markdown template created:", output_file))
  message(paste("Mode:", mode))
  if (mode == "analysis_plan") {
    message(paste("Variables to analyze:", nrow(analysis_plan)))
    message(paste("Data sheets:", length(unique(analysis_plan$sheet))))
  }
}

#' Generate Custom CSS for Theme Integration
#'
#' @param primary_color Primary color hex code
#' @param secondary_color Secondary color hex code
#' @return Character string containing CSS
generate_custom_css <- function(primary_color, secondary_color) {
  css <- paste0('
<style>
/* Custom CSS for Survey Report Template */
.main-container {
  max-width: 1400px;
  margin: 0 auto;
  padding: 20px;
}

/* Header styling */
h1, h2, h3 {
  color: ', primary_color, ';
  border-bottom: 2px solid ', primary_color, ';
  padding-bottom: 10px;
}

/* Table of Contents styling */
#TOC {
  background-color: #f8f9fa;
  border: 1px solid #dee2e6;
  border-radius: 5px;
  padding: 15px;
  margin-bottom: 20px;
}

#TOC .toc-active {
  color: ', primary_color, ';
  font-weight: bold;
}

/* Links */
a {
  color: ', secondary_color, ';
}

a:hover {
  color: ', primary_color, ';
}

/* Table styling */
table {
  border-collapse: collapse;
  width: 100%;
  margin: 20px 0;
}

th {
  background-color: ', primary_color, ';
  color: white;
  padding: 12px;
  text-align: left;
  font-weight: bold;
}

td {
  padding: 10px;
  border-bottom: 1px solid #ddd;
}

tr:nth-child(even) {
  background-color: #f2f2f2;
}

tr:hover {
  background-color: #e6f3ff;
}

/* Code chunk styling */
pre {
  background-color: #f8f9fa;
  border: 1px solid #e9ecef;
  border-radius: 5px;
  padding: 15px;
  overflow-x: auto;
}

/* Project info box */
.project-info {
  background-color: #f8f9fa;
  border-left: 4px solid ', primary_color, ';
  padding: 20px;
  margin: 20px 0;
  border-radius: 5px;
}

.project-info h3 {
  color: ', primary_color, ';
  margin-top: 0;
  border-bottom: none;
}

/* Responsive design */
@media (max-width: 768px) {
  .main-container {
    padding: 10px;
  }
  
  table {
    font-size: 14px;
  }
  
  th, td {
    padding: 8px;
  }
}

/* Plot styling */
.plot-container {
  margin: 20px 0;
  text-align: center;
}

/* Section dividers */
.section-divider {
  border-top: 3px solid ', secondary_color, ';
  margin: 30px 0;
}
</style>
')
  return(css)
}

#' Generate YAML Header
#'
#' @param survey_name Survey name
#' @param author Author name
#' @param css_content CSS content
#' @return Character string containing YAML header
generate_yaml_header <- function(survey_name, author, css_content, show_code) {
  yaml <- paste0('---
title: "', survey_name, ' - Survey Analysis Report"
author: "', author, '"
date: "`r Sys.Date()`"
output: 
  html_document:
    toc: true
    toc_float: true
    toc_depth: 3
    theme: flatly
    highlight: tango')
  
  # Only add code_folding if show_code is TRUE
  if (show_code) {
    yaml <- paste0(yaml, '
    code_folding: show')
  }
  
  yaml <- paste0(yaml, '
---
')
  return(yaml)
}

#' Generate Placeholder Content
#'
#' @param org_name Organization name
#' @param survey_name Survey name
#' @param author Author name
#' @param data_collection_start Data collection start date
#' @param data_collection_end Data collection end date
#' @param project_summary Project summary
#' @param primary_color Primary color
#' @param secondary_color Secondary color
#' @param multi_response_sep Multi-response separator
#' @return Character string containing placeholder content
generate_placeholder_content <- function(org_name, survey_name, author, 
                                        data_collection_start, data_collection_end,
                                        project_summary, primary_color, secondary_color,
                                        multi_response_sep, show_code) {
  
  content <- paste0('
# Project Information

<div class="project-info">

**Organization:** ', org_name, '  
**Survey:** ', survey_name, '  
**Author:** ', author, '  
**Report Date:** `r Sys.Date()`')

  if (!is.null(data_collection_start)) {
    content <- paste0(content, '  
**Data Collection Start:** ', data_collection_start)
  }
  
  if (!is.null(data_collection_end)) {
    content <- paste0(content, '  
**Data Collection End:** ', data_collection_end)
  }
  
  content <- paste0(content, '

**Project Summary:**  
', project_summary, '

</div>

---

# Setup')

  # Add setup code chunk only if show_code is TRUE
  if (show_code) {
    content <- paste0(content, '

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 10,
  fig.height = 6,
  fig.align = "center"
)

# Load required packages
library(surveyAnalysis)
library(dplyr)
library(DT)
library(ggplot2)

# Set color theme
color_primary <- "', primary_color, '"
color_secondary <- "', secondary_color, '"

# Set multi-response separator
multi_response_sep <- "', multi_response_sep, '"

# Load your data here
# data <- read.csv("your_data_file.csv")
# data_sheet1 <- data  # or load specific sheet
# data_sheet2 <- read.csv("sheet2.csv")  # if multiple sheets
```')
  }

  # Add example sections only if show_code is TRUE
  if (show_code) {
    content <- paste0(content, '

---

# Demographics

## Example: Single Select Analysis

```{r demographics-example}
# Example analysis - replace with your actual variables
# result_gender <- analyze_single_select(
#   df = data_sheet1,
#   ques = "gender",
#   disag = "all",
#   show_view = FALSE,
#   dt_table = TRUE,
#   create_plot = TRUE
# )
# 
# # Display table
# result_gender$dt_table
# 
# # Display plot
# result_gender$plot
```

## Example: Multi Select Analysis

```{r multi-select-example}
# Example analysis - replace with your actual variables
# result_services <- analyze_multi_select(
#   df = data_sheet1,
#   ques = "services_used",
#   disag = "all",
#   multi_response_sep = multi_response_sep,
#   show_view = FALSE,
#   dt_table = TRUE,
#   create_plot = TRUE
# )
# 
# # Display table
# result_services$dt_table
# 
# # Display plot
# result_services$plot
```

---

# Key Indicators

## Example: Statistical Analysis

```{r statistical-example}
# Example analysis - replace with your actual variables
# result_age <- analyze_mean(
#   df = data_sheet1,
#   ques = "age",
#   disag = "all",
#   show_view = FALSE,
#   dt_table = TRUE,
#   create_plot = TRUE
# )
# 
# # Display table
# result_age$dt_table
# 
# # Display plot
# result_age$plot
```

## Example: Disaggregated Analysis

```{r disaggregated-example}
# Example analysis by region - replace with your actual variables
# result_gender_by_region <- analyze_single_select(
#   df = data_sheet1,
#   ques = "gender",
#   disag = "region",
#   show_view = FALSE,
#   dt_table = TRUE,
#   create_plot = TRUE
# )
# 
# # Display table
# result_gender_by_region$dt_table
# 
# # Display plot
# result_gender_by_region$plot
```

---

# Statistical Analysis

## Example: Multiple Statistical Measures

```{r multiple-stats-example}
# Example analysis - replace with your actual variables
# 
# # Mean analysis
# result_mean <- analyze_mean(
#   df = data_sheet1,
#   ques = "income",
#   disag = "region",
#   show_view = FALSE,
#   dt_table = TRUE,
#   create_plot = TRUE
# )
# 
# # Median analysis
# result_median <- analyze_median(
#   df = data_sheet1,
#   ques = "income",
#   disag = "region",
#   show_view = FALSE,
#   dt_table = TRUE,
#   create_plot = TRUE
# )
# 
# # Sum analysis
# result_sum <- analyze_sum(
#   df = data_sheet1,
#   ques = "income",
#   disag = "region",
#   show_view = FALSE,
#   dt_table = TRUE,
#   create_plot = TRUE
# )
```

---

# Additional Analysis

Add your custom analysis sections here. Use the examples above as templates for:

- Single select questions: `analyze_single_select()`
- Multi select questions: `analyze_multi_select()`
- Mean calculations: `analyze_mean()`
- Median calculations: `analyze_median()`
- Sum calculations: `analyze_sum()`
- Quartile calculations: `analyze_first_quartile()`, `analyze_third_quartile()`
- Min/Max calculations: `analyze_min()`, `analyze_max()`

Remember to:
1. Set `show_view = FALSE` to prevent automatic display
2. Set `dt_table = TRUE` and `create_plot = TRUE` to get both outputs
3. Access results using `$dt_table` and `$plot` components
4. Use appropriate disaggregation variables for subgroup analysis')
  } else {
    # When show_code is FALSE, provide guidance without code examples
    content <- paste0(content, '

---

# Analysis Sections

This template provides a structure for your survey analysis report. Add your analysis sections below:

## Demographics
Add demographic analysis here.

## Key Indicators  
Add key indicator analysis here.

## Statistical Analysis
Add statistical analysis here.

## Additional Analysis
Add any additional analysis sections as needed.

---

# Analysis Guidelines

Use the surveyAnalysis package functions for your analysis:

- **Single select questions**: `analyze_single_select()`
- **Multi select questions**: `analyze_multi_select()`  
- **Mean calculations**: `analyze_mean()`
- **Median calculations**: `analyze_median()`
- **Sum calculations**: `analyze_sum()`
- **Quartile calculations**: `analyze_first_quartile()`, `analyze_third_quartile()`
- **Min/Max calculations**: `analyze_min()`, `analyze_max()`

**Important parameters:**
- Set `show_view = FALSE` to prevent automatic display
- Set `dt_table = TRUE` and `create_plot = TRUE` to get both outputs
- Access results using `$dt_table` and `$plot` components
- Use appropriate disaggregation variables for subgroup analysis')
  }

  content <- paste0(content, '

---

# Conclusion

Add your conclusions and recommendations here.

')
  
  return(content)
}

#' Generate Analysis Plan Content
#'
#' @param analysis_plan Analysis plan data frame
#' @param org_name Organization name
#' @param survey_name Survey name
#' @param author Author name
#' @param data_collection_start Data collection start date
#' @param data_collection_end Data collection end date
#' @param project_summary Project summary
#' @param primary_color Primary color
#' @param secondary_color Secondary color
#' @param multi_response_sep Multi-response separator
#' @return Character string containing analysis plan content
generate_analysis_plan_content <- function(analysis_plan, org_name, survey_name, author,
                                          data_collection_start, data_collection_end,
                                          project_summary, primary_color, secondary_color,
                                          multi_response_sep, show_code) {
  
  content <- paste0('
# Project Information

<div class="project-info">

**Organization:** ', org_name, '  
**Survey:** ', survey_name, '  
**Author:** ', author, '  
**Report Date:** `r Sys.Date()`')

  if (!is.null(data_collection_start)) {
    content <- paste0(content, '  
**Data Collection Start:** ', data_collection_start)
  }
  
  if (!is.null(data_collection_end)) {
    content <- paste0(content, '  
**Data Collection End:** ', data_collection_end)
  }
  
  content <- paste0(content, '

**Project Summary:**  
', project_summary, '

</div>

---

# Setup')

  # Add setup code chunk only if show_code is TRUE
  if (show_code) {
    content <- paste0(content, '

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 10,
  fig.height = 6,
  fig.align = "center"
)

# Load required packages
library(surveyAnalysis)
library(dplyr)
library(DT)
library(ggplot2)

# Set color theme
color_primary <- "', primary_color, '"
color_secondary <- "', secondary_color, '"

# Set multi-response separator
multi_response_sep <- "', multi_response_sep, '"

# Load your data here
# data_sheet1 <- read.csv("sheet1.csv")
# data_sheet2 <- read.csv("sheet2.csv")
# Add more data sheets as needed
```')
  }

  content <- paste0(content, '

')

  # Group analysis plan by sheet
  sheets <- unique(analysis_plan$sheet)
  sheets <- sheets[!is.na(sheets)]
  
  for (sheet in sheets) {
    sheet_plan <- analysis_plan[analysis_plan$sheet == sheet & !is.na(analysis_plan$sheet), ]
    
    if (nrow(sheet_plan) > 0) {
      content <- paste0(content, '
---

# ', stringr::str_to_title(sheet), ' Analysis

')
      
      # Generate code for each variable in the sheet (only if show_code is TRUE)
      if (show_code) {
        for (i in seq_len(nrow(sheet_plan))) {
          var_info <- sheet_plan[i, ]
          
          # Determine analysis function
          analysis_function <- determine_analysis_function(var_info$kobo_type, var_info$aggregation_method)
          
          if (!is.null(analysis_function)) {
            # Generate code chunk
            code_chunk <- generate_analysis_code_chunk(var_info, analysis_function, sheet, multi_response_sep)
            content <- paste0(content, code_chunk, '\n')
          }
        }
      } else {
        # When show_code is FALSE, just list the variables that would be analyzed
        content <- paste0(content, '
## Variables to Analyze

The following variables will be analyzed in this section:

')
        for (i in seq_len(nrow(sheet_plan))) {
          var_info <- sheet_plan[i, ]
          content <- paste0(content, '- **', var_info$label, '** (', var_info$variable, ') - ', 
                           var_info$kobo_type, ' - ', var_info$aggregation_method, '\n')
        }
        content <- paste0(content, '

Add your analysis code here using the appropriate `analyze_*` functions.

')
      }
    }
  }
  
  content <- paste0(content, '
---

# Conclusion

Add your conclusions and recommendations here based on the analysis results above.

')
  
  return(content)
}

#' Determine Analysis Function Based on Question Type and Aggregation Method
#'
#' @param kobo_type Question type from Kobo
#' @param aggregation_method Aggregation method
#' @return Character string with function name or NULL
determine_analysis_function <- function(kobo_type, aggregation_method) {
  
  if (kobo_type == "select_one" && aggregation_method %in% c("proportion", "perc")) {
    return("analyze_single_select")
  } else if (kobo_type == "select_multiple" && aggregation_method %in% c("proportion", "perc")) {
    return("analyze_multi_select")
  } else if (kobo_type == "integer" && aggregation_method == "mean") {
    return("analyze_mean")
  } else if (kobo_type == "integer" && aggregation_method == "median") {
    return("analyze_median")
  } else if (kobo_type == "integer" && aggregation_method == "sum") {
    return("analyze_sum")
  } else if (kobo_type == "integer" && aggregation_method == "1stq") {
    return("analyze_first_quartile")
  } else if (kobo_type == "integer" && aggregation_method == "3rdq") {
    return("analyze_third_quartile")
  } else if (kobo_type == "integer" && aggregation_method == "min") {
    return("analyze_min")
  } else if (kobo_type == "integer" && aggregation_method == "max") {
    return("analyze_max")
  }
  
  return(NULL)
}

#' Generate Analysis Code Chunk
#'
#' @param var_info Variable information from analysis plan
#' @param analysis_function Analysis function name
#' @param sheet Data sheet name
#' @param multi_response_sep Multi-response separator
#' @return Character string containing code chunk
generate_analysis_code_chunk <- function(var_info, analysis_function, sheet, multi_response_sep) {
  
  # Clean variable name for R object naming
  clean_var_name <- gsub("[^A-Za-z0-9_]", "_", var_info$variable)
  
  # Handle disaggregation
  disag_var <- if (!is.na(var_info$disaggregation) && var_info$disaggregation != "all") {
    var_info$disaggregation
  } else {
    "all"
  }
  
  # Generate code chunk
  code_chunk <- paste0('
## ', var_info$label, '

```{r ', clean_var_name, '}
# ', var_info$label, '
result_', clean_var_name, ' <- ', analysis_function, '(
  df = data_', sheet, ',
  ques = "', var_info$variable, '",
  disag = "', disag_var, '",')
  
  # Add multi_response_sep for multi-select questions
  if (analysis_function == "analyze_multi_select") {
    code_chunk <- paste0(code_chunk, '
  multi_response_sep = multi_response_sep,')
  }
  
  code_chunk <- paste0(code_chunk, '
  show_view = FALSE,
  dt_table = TRUE,
  create_plot = TRUE
)

# Display table
result_', clean_var_name, '$dt_table

# Display plot
result_', clean_var_name, '$plot
```

')
  
  return(code_chunk)
}
