# SurveyAnalysis Package

A comprehensive R package for analyzing survey data collected with Kobo Toolbox or SurveyCTO. This package provides feature-rich descriptive analysis functions with automatic disaggregation, interactive visualizations, and flexible output formats, including DT tables and plots.

## Installation

```r
# Install from GitHub (if available)
devtools::install_github("nzfarhad/surveyAnalysis")

# Or install locally
devtools::install("path/to/surveyAnalysis")
```

## Features

- **Analysis Functions**: Modern descriptive analysis functions with disaggregation support
- **Visualizations**: Built-in plotting with customizable themes and chart types
- **DT Tables**: Interactive HTML tables with search, filtering, and export capabilities
- **Flexible Output**: Choose between data frames, DT tables, plots, or combinations
- **Automatic Disaggregation**: Seamlessly analyze by subgroups without manual looping
- **R Markdown Templates**: Generate professional report templates
- **Repeat Group Support**: Handle complex survey structures with nested data
- **Batch Analysis**: Analyze multiple variables simultaneously using a predefined analysis plan

## Quick Start

### Analysis Functions

The package provides analysis functions that automatically handle disaggregation and offer flexible output options:

```r
library(surveyAnalysis)

# Load your data
survey_data <- read.csv("your_survey_data.csv")

# Single-select analysis with automatic disaggregation
gender_results <- analyze_single_select(
  df = survey_data,
  ques = "gender",
  disag = "region",  # Automatically analyzes all regions
  dt_table = TRUE,   # Creates interactive DT table
  create_plot = TRUE  # Creates visualization
)

# Access results
print(gender_results$table)    # Data frame
print(gender_results$dt_table) # Interactive table
print(gender_results$plot)     # Visualization

# Multi-select analysis
services_results <- analyze_multi_select(
  df = survey_data,
  ques = "services_used",
  disag = "region",
  multi_response_sep = ";",
  chart_type = "column",
  dt_table = TRUE,
  create_plot = TRUE
)

# Statistical analysis
age_results <- analyze_mean(
  df = survey_data,
  ques = "age",
  disag = "region",
  dt_table = TRUE,
  create_plot = TRUE
)
```

### Batch Analysis with Analysis Plans

For comprehensive analysis across multiple variables:

```r
# Create analysis plan
analysis_plan <- data.frame(
  variable = c("gender", "age", "services_used"),
  label = c("Gender", "Age", "Services Used"),
  kobo_type = c("select_one", "integer", "select_multiple"),
  aggregation_method = c("proportion", "mean", "proportion"),
  disaggregation = c("region", "region", "region"),
  disagg_label = c("Geographic Region", "Geographic Region", "Geographic Region")
)

# Run comprehensive analysis
results <- analysis_func(df = survey_data, ap = analysis_plan)
```


## Analysis Functions

### Single-Select Analysis

```r
# Create sample data
survey_data <- data.frame(
  gender = c("Male", "Female", "Male", "Female", "Male"),
  region = c("North", "South", "North", "South", "East")
)

# Analyze gender overall (no disaggregation)
gender_results <- analyze_single_select(
  df = survey_data, 
  ques = "gender",
  show_view = FALSE,
  dt_table = TRUE,
  create_plot = TRUE,
  chart_type = "column"
)

# Analyze gender by region (automatic disaggregation)
gender_by_region <- analyze_single_select(
  df = survey_data, 
  ques = "gender", 
  disag = "region",
  dt_table = TRUE,
  create_plot = TRUE
)
```

### Multi-Select Analysis

```r
# Create sample data with multi-select responses
survey_data <- data.frame(
  services_used = c("Health; Education", "Health", "Education; Transport", 
                    "Health; Education; Transport", "Transport"),
  region = c("North", "South", "North", "South", "East")
)

# Analyze services with automatic disaggregation
services_results <- analyze_multi_select(
  df = survey_data, 
  ques = "services_used",
  disag = "region",
  multi_response_sep = ";",
  dt_table = TRUE,
  create_plot = TRUE,
  chart_type = "column"
)
```

### Statistical Analysis

```r
# Create sample numeric data
survey_data <- data.frame(
  age = c(25, 30, 35, 40, 28, 32, 45, 38),
  income = c(1000, 1500, 2000, 2500, 1200, 1800, 3000, 2200),
  region = c("North", "South", "North", "South", "East", "North", "South", "East")
)

# Mean analysis
mean_age <- analyze_mean(
  df = survey_data, 
  ques = "age",
  disag = "region",
  dt_table = TRUE,
  create_plot = TRUE
)

# Median analysis
median_income <- analyze_median(
  df = survey_data, 
  ques = "income", 
  disag = "region",
  dt_table = TRUE,
  create_plot = TRUE
)

# Sum analysis
total_income <- analyze_sum(
  df = survey_data, 
  ques = "income", 
  disag = "region",
  dt_table = TRUE,
  create_plot = TRUE
)

# Min/Max analysis
min_age <- analyze_min(df = survey_data, ques = "age", disag = "region")
max_age <- analyze_max(df = survey_data, ques = "age", disag = "region")
```

### R Markdown Template Generation

Generate professional report templates:

```r
# Create HTML report template
generate_rmd_template(
  output_file = "survey_report.Rmd",
  org_name = "My Organization",
  survey_name = "Health Facility Assessment",
  author = "John Doe",
  data_collection_start = "2024-01-01",
  data_collection_end = "2024-03-31",
  project_summary = "Assessment of health facilities in the region",
  primary_color = "#730202",
  secondary_color = "#f27304",
  multi_response_sep = ";",
  code_folding = FALSE,
  output_format = "html"
)

# Create Word document template
generate_rmd_template(
  output_file = "survey_report_word.Rmd",
  org_name = "My Organization",
  survey_name = "Health Facility Assessment",
  author = "John Doe",
  output_format = "word"
)
```

## Function Reference

### Analysis Functions

- `analyze_single_select()`: Single-select analysis with automatic disaggregation
- `analyze_multi_select()`: Multi-select analysis with automatic disaggregation
- `analyze_mean()`: Mean calculation with automatic disaggregation
- `analyze_median()`: Median calculation with automatic disaggregation
- `analyze_sum()`: Sum calculation with automatic disaggregation
- `analyze_min()`: Minimum calculation with automatic disaggregation
- `analyze_max()`: Maximum calculation with automatic disaggregation
- `generate_rmd_template()`: Generate professional R Markdown report templates

### Batch Analysis Functions

- `analysis_func()`: Main wrapper function for comprehensive analysis with repeat group support

### Function Parameters

All analyze_ functions support these common parameters:

- `df`: Data frame containing survey data
- `ques`: Column name of the question to analyze
- `disag`: Disaggregation variable (optional, analyzes all levels automatically)
- `level`: Specific disaggregation level (optional)
- `show_view`: Display results in Viewer pane (logical)
- `wide_format`: Reshape results to wide format (logical)
- `dt_table`: Create interactive DT table (logical)
- `create_plot`: Create visualization (logical)
- `max_categories`: Maximum number of categories to display
- `max_label_length`: Maximum length for labels
- `font_sizes`: List of font sizes for plot elements

### Return Values

Analysis functions return:
- **Data frame only**: When `dt_table = FALSE` and `create_plot = FALSE`
- **List with components**: When `dt_table = TRUE` or `create_plot = TRUE`
  - `$table`: Data frame with results
  - `$dt_table`: Interactive DT table (if requested)
  - `$plot`: ggplot2 visualization (if requested)

## Examples

### Example 1: Single-Select Analysis

```r
# Sample data
set.seed(123)
survey_data <- data.frame(
  gender = sample(c("Male", "Female"), 100, replace = TRUE),
  region = sample(c("North", "South", "East", "West"), 100, replace = TRUE),
  facility_type = sample(c("Hospital", "Clinic", "Health Center"), 100, replace = TRUE)
)

# Analyze gender by region with interactive outputs
gender_results <- analyze_single_select(
  df = survey_data,
  ques = "gender",
  disag = "region",
  dt_table = TRUE,
  create_plot = TRUE,
  chart_type = "column",
  max_categories = 10
)

# Display results
print(gender_results$table)    # Data frame
print(gender_results$dt_table) # Interactive table
print(gender_results$plot)     # Visualization
```

### Example 2: Multi-Select Analysis

```r
# Sample multi-select data
services_data <- data.frame(
  services_used = c("Health; Education", "Health", "Education; Transport", 
                    "Health; Education; Transport", "Transport"),
  region = c("North", "South", "North", "South", "East"),
  facility_type = c("Hospital", "Clinic", "Hospital", "Health Center", "Clinic")
)

# Analyze services with multiple outputs
services_results <- analyze_multi_select(
  df = services_data,
  ques = "services_used",
  disag = "region",
  multi_response_sep = ";",
  dt_table = TRUE,
  create_plot = TRUE,
  chart_type = "column",
  max_categories = 5
)
```

### Example 3: Statistical Analysis

```r
# Sample numeric data
numeric_data <- data.frame(
  age = sample(18:65, 100, replace = TRUE),
  income = sample(1000:5000, 100, replace = TRUE),
  satisfaction = sample(1:5, 100, replace = TRUE),
  region = sample(c("North", "South", "East", "West"), 100, replace = TRUE)
)

# Multiple statistical analyses
age_mean <- analyze_mean(df = numeric_data, ques = "age", disag = "region", 
                        dt_table = TRUE, create_plot = TRUE)
income_median <- analyze_median(df = numeric_data, ques = "income", disag = "region",
                               dt_table = TRUE, create_plot = TRUE)
satisfaction_sum <- analyze_sum(df = numeric_data, ques = "satisfaction", disag = "region",
                               dt_table = TRUE, create_plot = TRUE)
```

### Example 4: R Markdown Report Generation

```r
# Generate comprehensive report template
generate_rmd_template(
  output_file = "comprehensive_survey_report.Rmd",
  org_name = "Health Research Organization",
  survey_name = "Health Facility Assessment 2024",
  author = "Dr. Jane Smith",
  data_collection_start = "2024-01-15",
  data_collection_end = "2024-03-15",
  project_summary = "Comprehensive assessment of health facilities including infrastructure, services, and patient satisfaction across multiple regions.",
  primary_color = "#2E86AB",
  secondary_color = "#A23B72",
  multi_response_sep = ";",
  code_folding = TRUE,
  output_format = "html"
)
```

### Example 5: Batch Analysis with Analysis Functions

```r
# Analysis plan for comprehensive analysis
analysis_plan <- data.frame(
  variable = c("gender", "age", "services_used", "satisfaction"),
  label = c("Gender Distribution", "Average Age", "Services Used", "Satisfaction Score"),
  kobo_type = c("select_one", "integer", "select_multiple", "integer"),
  aggregation_method = c("proportion", "mean", "proportion", "mean"),
  disaggregation = c("region", "region", "region", "region"),
  disagg_label = c("Geographic Region", "Geographic Region", "Geographic Region", "Geographic Region")
)

# Run comprehensive analysis
results <- analysis_func(df = survey_data, ap = analysis_plan, multi_response_sep = ";")
```

## Dependencies

- `dplyr`: Data manipulation
- `ggplot2`: Data visualization
- `DT`: Interactive tables
- `htmltools`: HTML table generation
- `stringr`: String operations
- `reshape2`: Data reshaping
- `knitr`: R Markdown processing

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## License

This package is licensed under the GPL License.
