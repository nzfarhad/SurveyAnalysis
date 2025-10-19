#' Generate R Markdown Template for Survey Reports
#'
#' Creates a professional R Markdown template for surveyCTO/Kobo survey analysis reports.
#' Generates a placeholder template with example code for manual customization.
#'
#' @param output_file Character string specifying the output file path (e.g., "survey_report.Rmd")
#' @param mode Character string specifying the template mode: "placeholder"
#' @param analysis_plan Data frame containing analysis plan (not used, kept for compatibility)
#' @param org_name Character string for organization name
#' @param survey_name Character string for survey name
#' @param author Character string for report author
#' @param data_collection_start Character string for data collection start date
#' @param data_collection_end Character string for data collection end date
#' @param project_summary Character string for project summary
#' @param primary_color Character string for primary color (hex code, e.g., "#730202")
#' @param secondary_color Character string for secondary color (hex code, e.g., "#f27304")
#' @param multi_response_sep Character string for multi-response separator (default: "; ")
#' @param code_folding Logical indicating whether to enable code folding in YAML (default: FALSE)
#' @param output_format Character string specifying output format: "html" or "word" (default: "html")
#'
#' @return Creates an R Markdown file at the specified output path
#'
#' @examples
#' \dontrun{
#' # Create placeholder template (code hidden by default)
#' generate_rmd_template(
#'   output_file = "survey_report.Rmd",
#'   org_name = "My Organization",
#'   survey_name = "Health Facility Assessment",
#'   author = "John Doe",
#'   project_summary = "Assessment of health facilities in the region"
#' )
#' 
#' # Create template with code folding enabled
#' generate_rmd_template(
#'   output_file = "survey_report.Rmd",
#'   org_name = "My Organization",
#'   survey_name = "Health Facility Assessment",
#'   author = "John Doe",
#'   code_folding = TRUE
#' )
#' 
#' # Create Word document template
#' generate_rmd_template(
#'   output_file = "survey_report.Rmd",
#'   org_name = "My Organization",
#'   survey_name = "Health Facility Assessment",
#'   author = "John Doe",
#'   output_format = "word"
#' )
#' }
#'
#' @export
generate_rmd_template <- function(
  output_file,
  mode = "placeholder",
  analysis_plan = NULL,
  org_name = "Organization Name",
  survey_name = "Survey Name",
  author = "Author Name",
  data_collection_start = NULL,
  data_collection_end = NULL,
  project_summary = "Project summary goes here",
  primary_color = "#730202",
  secondary_color = "#f27304",
  multi_response_sep = "; ",
  code_folding = FALSE,
  output_format = "html"
) {
  
  # Validate inputs
  mode <- match.arg(mode, choices = "placeholder")
  output_format <- match.arg(output_format, choices = c("html", "word"))
  
  # Validate color codes
  if (!grepl("^#[0-9A-Fa-f]{6}$", primary_color)) {
    stop("primary_color must be a valid hex color code (e.g., '#003366')")
  }
  if (!grepl("^#[0-9A-Fa-f]{6}$", secondary_color)) {
    stop("secondary_color must be a valid hex color code (e.g., '#FF6B35')")
  }
  
  # Generate CSS (only for HTML output)
  if (output_format == "html") {
    css_content <- generate_custom_css(primary_color, secondary_color)
  } else {
    css_content <- ""
  }
  
  # Generate YAML header
  yaml_header <- generate_yaml_header(survey_name, author, css_content, code_folding, output_format)
  
  # Generate placeholder content (always include example code)
  document_content <- generate_placeholder_content(org_name, survey_name, author, 
                                                 data_collection_start, data_collection_end, 
                                                 project_summary, primary_color, secondary_color, 
                                                 multi_response_sep, output_format)
  
  # Combine and write to file (CSS goes after YAML header)
  full_content <- paste(yaml_header, css_content, document_content, sep = "\n")
  
  # Write to file
  cat(full_content, file = output_file)
  
  message(paste("R Markdown template created:", output_file))
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
#' @param code_folding Whether to enable code folding
#' @param output_format Output format ("html" or "word")
#' @return Character string containing YAML header
generate_yaml_header <- function(survey_name, author, css_content, code_folding, output_format) {
  yaml <- paste0('---
title: "', survey_name, ' - Survey Analysis Report"
author: "', author, '"
date: "`r Sys.Date()`"
output: ')
  
  if (output_format == "html") {
    yaml <- paste0(yaml, '
  html_document:
    toc: true
    toc_float: true
    toc_depth: 3
    theme: flatly
    highlight: tango')
    
    # Only add code_folding if code_folding is TRUE
    if (code_folding) {
      yaml <- paste0(yaml, '
    code_folding: show')
    }
  } else if (output_format == "word") {
    yaml <- paste0(yaml, '
  word_document:
    toc: true
    toc_depth: 3
    reference_docx: default')
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
                                        multi_response_sep, output_format) {
  
  # Generate project information section based on output format
  if (output_format == "html") {
    content <- paste0('
# Project Information

<div class="project-info">

**Organization:** ', org_name, '  
**Survey:** ', survey_name, '  
**Author:** ', author, '  
**Report Date:** `r Sys.Date()`')
  } else {
    content <- paste0('
# Project Information

**Organization:** ', org_name, '  
**Survey:** ', survey_name, '  
**Author:** ', author, '  
**Report Date:** `r Sys.Date()`')
  }

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

')

  # Close HTML div only for HTML output
  if (output_format == "html") {
    content <- paste0(content, '</div>')
  }

  content <- paste0(content, '

---

# Setup

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
```

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

  content <- paste0(content, '

---

# Conclusion

Add your conclusions and recommendations here.

')
  
  return(content)
}

