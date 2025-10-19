#' SurveyAnalysis: Survey Data Analysis Package
#'
#' A comprehensive R package for analyzing survey data collected with Kobo Toolbox 
#' or SurveyCTO. Provides functions for descriptive analysis including single-select, 
#' multi-select, and statistical aggregations with support for disaggregation and 
#' repeat groups.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{analysis_func}}}{Main wrapper function for comprehensive analysis}
#'   \item{\code{\link{analyze}}}{Core analysis function based on analysis plan}
#' }
#'
#' @section Enhanced Analysis Functions (with automatic disaggregation):
#' \describe{
#'   \item{\code{\link{analyze_single_select}}}{Enhanced single-select analysis with automatic disaggregation}
#'   \item{\code{\link{analyze_multi_select}}}{Enhanced multi-select analysis with automatic disaggregation}
#'   \item{\code{\link{analyze_mean}}}{Enhanced mean calculation with automatic disaggregation}
#'   \item{\code{\link{analyze_median}}}{Enhanced median calculation with automatic disaggregation}
#'   \item{\code{\link{analyze_sum}}}{Enhanced sum calculation with automatic disaggregation}
#'   \item{\code{\link{analyze_first_quartile}}}{Enhanced first quartile calculation with automatic disaggregation}
#'   \item{\code{\link{analyze_third_quartile}}}{Enhanced third quartile calculation with automatic disaggregation}
#'   \item{\code{\link{analyze_min}}}{Enhanced minimum calculation with automatic disaggregation}
#'   \item{\code{\link{analyze_max}}}{Enhanced maximum calculation with automatic disaggregation}
#' }
#'
#' @section Basic Analysis Functions:
#' \describe{
#'   \item{\code{\link{single_select}}}{Analyze single-select questions}
#'   \item{\code{\link{multi_select}}}{Analyze multi-select questions}
#'   \item{\code{\link{stat_mean}}}{Calculate mean values}
#'   \item{\code{\link{stat_median}}}{Calculate median values}
#'   \item{\code{\link{stat_sum}}}{Calculate sum values}
#'   \item{\code{\link{stat_1stq}}}{Calculate first quartile}
#'   \item{\code{\link{stat_3rdq}}}{Calculate third quartile}
#'   \item{\code{\link{stat_min}}}{Calculate minimum values}
#'   \item{\code{\link{stat_max}}}{Calculate maximum values}
#' }
#'
#' @section Visualization Functions:
#' \describe{
#'   \item{\code{\link{create_visualization}}}{Create appropriate visualizations based on analysis type}
#'   \item{\code{\link{create_percentage_visualization}}}{Create percentage-based charts}
#'   \item{\code{\link{create_statistical_visualization}}}{Create statistical charts}
#'   \item{\code{\link{create_box_plot}}}{Create box plots for statistical analysis}
#'   \item{\code{\link{create_color_palette}}}{Generate color palettes for visualizations}
#'   \item{\code{\link{truncate_labels}}}{Truncate long labels for better visualization}
#' }
#'
#' @section Report Generation:
#' \describe{
#'   \item{\code{\link{generate_rmd_template}}}{Generate R Markdown placeholder templates for survey reports}
#' }
#'
#' @section Question Types:
#' \describe{
#'   \item{select_one}{Categorical questions with single response}
#'   \item{select_multiple}{Questions allowing multiple responses}
#'   \item{integer}{Numeric questions supporting various statistical measures}
#' }
#'
#' @section Features:
#' \itemize{
#'   \item Single-select and multi-select question analysis
#'   \item Statistical analysis (mean, median, quartiles, min/max)
#'   \item Disaggregation support for subgroup analysis
#'   \item Repeat group analysis for complex survey structures
#'   \item Flexible analysis plans defined in data frames
#'   \item Customizable multi-response separators
#'   \item Professional visualizations with customizable color themes
#'   \item R Markdown placeholder template generation for report creation
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' library(surveyAnalysis)
#' 
#' # Create analysis plan
#' analysis_plan <- data.frame(
#'   variable = c("gender", "age"),
#'   kobo_type = c("select_one", "integer"),
#'   aggregation_method = c("perc", "mean"),
#'   disaggregation = c("region", "all"),
#'   label = c("Gender", "Age")
#' )
#' 
#' # Run analysis
#' results <- analysis_func(df = survey_data, ap = analysis_plan)
#' }
#'
#' @docType package
#' @name surveyAnalysis
#' @aliases surveyAnalysis-package
NULL
