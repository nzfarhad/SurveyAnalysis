# SurveyAnalysis Package

A comprehensive R package for analyzing survey data collected with Kobo Toolbox or SurveyCTO. This package provides functions for descriptive analysis including single-select, multi-select, and statistical aggregations with support for disaggregation and repeat groups.

## Installation

```r
# Install from GitHub (if available)
devtools::install_github("nzfarhad/surveyAnalysis")

# Or install locally
devtools::install("path/to/surveyAnalysis")
```

## Features

- **Single-select analysis**: Calculate percentages and counts for categorical questions
- **Multi-select analysis**: Handle multiple response questions with customizable separators
- **Statistical analysis**: Mean, median, sum, quartiles, min/max for numeric data
- **Disaggregation support**: Analyze data by subgroups (e.g., by region, gender)
- **Repeat group analysis**: Handle complex survey structures with repeat groups
- **Flexible analysis plans**: Define analysis specifications in data frames

## Quick Start

### Basic Usage

```r
library(surveyAnalysis)

# Load your data
survey_data <- read.csv("your_survey_data.csv")

# Create analysis plan
analysis_plan <- data.frame(
  variable = c("gender", "age", "services_used"),
  kobo_type = c("select_one", "integer", "select_multiple"),
  aggregation_method = c("proportion", "mean", "proportion"),
  disaggregation = c("region", "all", "region"),
  label = c("Gender", "Age", "Services Used"),
  disagg_label = c("Region", NA, "Region")
)

# Run analysis
results <- analysis_func(df = survey_data, ap = analysis_plan)
```

### Advanced Usage with Repeat Groups

```r
# Analysis plan with repeat groups
analysis_plan <- data.frame(
  variable = c("consultation_type", "duration"),
  kobo_type = c("select_one", "integer"),
  aggregation_method = c("perc", "mean"),
  disaggregation = c("all", "all"),
  repeat_for = c("patient_id", "patient_id"),
  label = c("Consultation Type", "Duration (minutes)")
)

# Run analysis with repeat group support
results <- analysis_func(df = consultation_data, 
                        ap = analysis_plan,
                        multi_response_sep = ";")
```


## Standalone Function Usage

Each analysis function can be used independently without requiring the main wrapper functions. This is useful for quick analyses or when you need more control over the analysis process.

### Single-Select Analysis

```r
# Create sample data
survey_data <- data.frame(
  gender = c("Male", "Female", "Male", "Female", "Male"),
  region = c("North", "South", "North", "South", "East")
)

# Analyze gender overall
gender_results <- single_select(df = survey_data, 
                               ques = "gender", 
                               disag = "all", 
                               level = "all")
print(gender_results)

# Analyze gender by region
gender_by_region <- single_select(df = survey_data, 
                                 ques = "gender", 
                                 disag = "region", 
                                 level = "North")
print(gender_by_region)
```

### Multi-Select Analysis

```r
# Create sample data with multi-select responses
survey_data <- data.frame(
  services_used = c("Health; Education", "Health", "Education; Transport", 
                    "Health; Education; Transport", "Transport"),
  region = c("North", "South", "North", "South", "East")
)

# Set the separator
multi_response_sep <<- ";"

# Analyze services overall
services_results <- multi_select(df = survey_data, 
                                ques = "services_used", 
                                disag = "all", 
                                level = "all")
print(services_results)
```

### Statistical Analysis

```r
# Create sample numeric data
survey_data <- data.frame(
  age = c(25, 30, 35, 40, 28, 32, 45, 38),
  income = c(1000, 1500, 2000, 2500, 1200, 1800, 3000, 2200),
  region = c("North", "South", "North", "South", "East", "North", "South", "East")
)

# Calculate mean age
mean_age <- stat_mean(df = survey_data, 
                     ques = "age", 
                     disag = "all", 
                     level = "all")
print(mean_age)

# Calculate median income by region
median_income_north <- stat_median(df = survey_data, 
                                  ques = "income", 
                                  disag = "region", 
                                  level = "North")
print(median_income_north)

# Calculate sum of income
total_income <- stat_sum(df = survey_data, 
                        ques = "income", 
                        disag = "all", 
                        level = "all")
print(total_income)
```

### Batch Analysis with Individual Functions

```r
# Analyze multiple variables using individual functions
variables_to_analyze <- c("gender", "age", "income")
results_list <- list()

for(var in variables_to_analyze) {
  if(var == "gender") {
    results_list[[var]] <- single_select(df = survey_data, 
                                        ques = var, 
                                        disag = "all", 
                                        level = "all")
  } else if(var %in% c("age", "income")) {
    results_list[[var]] <- stat_mean(df = survey_data, 
                                    ques = var, 
                                    disag = "all", 
                                    level = "all")
  }
}

# Combine results
combined_results <- do.call(rbind, results_list)
print(combined_results)
```

## Function Reference

### Standalone Functions

- `analyze_single_select()`: Enhanced single-select analysis with automatic disaggregation
- `analyze_multi_select()`: Enhanced multi-select analysis with automatic disaggregation
- `analyze_mean()`: Enhanced mean calculation with automatic disaggregation
- `analyze_median()`: Enhanced median calculation with automatic disaggregation
- `analyze_sum()`: Enhanced sum calculation with automatic disaggregation
- `analyze_first_quartile()`: Enhanced first quartile calculation with automatic disaggregation
- `analyze_third_quartile()`: Enhanced third quartile calculation with automatic disaggregation
- `analyze_min()`: Enhanced minimum calculation with automatic disaggregation
- `analyze_max()`: Enhanced maximum calculation with automatic disaggregation

### Batch Analysis Functions

- `analysis_func()`: Main wrapper function for comprehensive analysis


## Analysis Plan Structure

The analysis plan is a data frame with the following columns:

| Column | Description | Required | Example |
|--------|-------------|----------|---------|
| `variable` | Column name in dataset | Yes | "gender" |
| `label` | Question label | No | "<b>Gender:</b>" |
| `kobo_type` | Question type | Yes | "select_one", "select_multiple", "integer" |
| `aggregation_method` | Analysis method | Yes | "proportion", "mean", "median", "sum", etc. |
| `disaggregation` | Disaggregation variable | Yes | "region" or "all" |
| `disagg_label` | Disaggregation label | No | "Geographic region" |
| `sheet` | Data sheet name (for multi-sheet analysis) | No | "data", "patient_consultation" |
| `Remarks` | Optional notes about the analysis | No | "Calculated in the script" |
| `repeat_for` | Repeat group column | No | "patient_id" |

## Question Types

### select_one
Categorical questions with single response. Use `aggregation_method = "proportion"`.

### select_multiple
Questions allowing multiple responses. Responses are separated by a delimiter (default: "; "). Use `aggregation_method = "proportion"`.

### integer
Numeric questions. Supports various aggregation methods:
- `"mean"`: Calculate mean
- `"median"`: Calculate median
- `"sum"`: Calculate sum
- `"firstq"`: First quartile
- `"thirdq"`: Third quartile
- `"min"`: Minimum value
- `"max"`: Maximum value

## Disaggregation

Disaggregation allows you to analyze data by subgroups:

```r
# Analyze by region
analysis_plan <- data.frame(
  variable = "satisfaction",
  kobo_type = "integer",
  aggregation_method = "mean",
  disaggregation = "region"  # Will analyze by each region
)

# No disaggregation
analysis_plan <- data.frame(
  variable = "satisfaction",
  kobo_type = "integer", 
  aggregation_method = "mean",
  disaggregation = "all"  # Overall analysis only
)
```

## Output Format

The analysis results include:

- `Disaggregation`: Disaggregation variable name
- `Disaggregation_level`: Specific level of disaggregation
- `Question`: Question variable name
- `Label`: Question label (if provided)
- `Response`: Response category (for categorical questions)
- `Aggregation_method`: Method used for analysis
- `Result`: Calculated result (percentage, mean, etc.)
- `Count`: Number of responses
- `Denominator`: Total valid responses
- `repeat_for`: Repeat group identifier (if applicable)

## Real-World Example: Health Facility Assessment

This example is based on a real health facility assessment survey (HER_RMU) that includes multiple data sheets and complex disaggregation patterns.

### Multi-Sheet Analysis

```r
# Load analysis plan from CSV
analysis_plan <- read.csv("example_analysis_plan.csv", stringsAsFactors = FALSE)

# Load data from multiple sheets
data_sheets <- list(
  data = read.csv("facility_data.csv"),
  opd_consultant = read.csv("opd_consultant_data.csv"), 
  patient_consultation = read.csv("patient_consultation_data.csv")
)

# Run analysis for each sheet
results <- list()
for(sheet_name in unique(analysis_plan$sheet)) {
  sheet_plan <- analysis_plan[analysis_plan$sheet == sheet_name, ]
  sheet_data <- data_sheets[[sheet_name]]
  
  results[[sheet_name]] <- analysis_func(
    df = sheet_data, 
    ap = sheet_plan,
    multi_response_sep = ";"
  )
}

# Combine results
final_results <- do.call(rbind, results)
```

### Complex Disaggregation Patterns

The analysis plan supports complex disaggregation patterns:

```r
# Example analysis plan with multiple disaggregation levels
complex_plan <- data.frame(
  variable = c("patient_received_antibiotic", "number_of_medicine_type_prescribed_to_the_patient"),
  label = c("Was the patient prescribed antibiotic?", "Number of medicine type prescribed to the patient:"),
  kobo_type = c("select_one", "integer"),
  aggregation_method = c("proportion", "mean"),
  disaggregation = c("HF_Type > Provider_Type", "diagnosis"),
  disagg_label = c("HF_Type > Provider_Type", "Diagnosis:"),
  sheet = c("patient_consultation", "patient_consultation"),
  Remarks = c("Calculated in the script", ""),
  repeat_for = c("", ""),
  stringsAsFactors = FALSE
)
```

### Analysis Plan Features

- **HTML Labels**: Support for HTML formatting in labels (e.g., `<b>Bold text</b>`)
- **Multi-sheet Support**: Analyze data from different sheets (facility data, patient data, etc.)
- **Complex Disaggregation**: Support for nested disaggregation (e.g., "HF_Type > Provider_Type")
- **Calculated Variables**: Mark variables that are calculated in preprocessing scripts
- **Repeat Groups**: Handle repeat group structures in survey data

## Examples

### Example 1: Basic Survey Analysis

```r
# Sample data
set.seed(123)
survey_data <- data.frame(
  gender = sample(c("Male", "Female"), 100, replace = TRUE),
  age = sample(18:65, 100, replace = TRUE),
  region = sample(c("North", "South", "East", "West"), 100, replace = TRUE),
  satisfaction = sample(1:5, 100, replace = TRUE)
)

# Analysis plan
ap <- data.frame(
  variable = c("gender", "age", "satisfaction"),
  kobo_type = c("select_one", "integer", "integer"),
  aggregation_method = c("proportion", "mean", "mean"),
  disaggregation = c("region", "all", "region"),
  label = c("Gender", "Age", "Satisfaction Score")
)

# Run analysis
results <- analysis_func(df = survey_data, ap = ap)
print(results)
```

### Example 2: Multi-select Analysis

```r
# Sample multi-select data
services_data <- data.frame(
  services_used = c("Health; Education", "Health", "Education; Transport", "Health; Education; Transport"),
  region = c("North", "South", "North", "South")
)

# Analysis plan
ap <- data.frame(
  variable = "services_used",
  kobo_type = "select_multiple",
  aggregation_method = "proportion",
  disaggregation = "region",
  label = "Services Used"
)

# Run analysis
results <- analysis_func(df = services_data, ap = ap, multi_response_sep = ";")
print(results)
```

## Dependencies

- `dplyr`: Data manipulation
- `stringr`: String operations
- `reshape2`: Data reshaping

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## License

This package is licensed under the MIT License.
