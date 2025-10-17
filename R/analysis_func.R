#' Main Analysis Function with Double Disaggregation Support
#'
#' Main wrapper function that handles analysis plans with support for repeat groups
#' and double disaggregation. This is the primary function users should call.
#'
#' @param df A data frame containing the survey data
#' @param ap A data frame specifying the analysis plan with columns:
#'   \itemize{
#'     \item variable: Column names to analyze
#'     \item label: Question labels (can include HTML tags like <b>)
#'     \item kobo_type: Type of question ("select_one", "select_multiple", "integer")
#'     \item aggregation_method: Method for aggregation ("proportion", "mean", "median", "sum", etc.)
#'     \item disaggregation: Variable for disaggregation ("all" for no disaggregation)
#'     \item disagg_label: Labels for disaggregation variables
#'     \item sheet: Data sheet name (for multi-sheet analysis)
#'     \item Remarks: Optional notes about the analysis
#'     \item repeat_for: Optional column for repeat group analysis
#'   }
#' @param multi_response_sep Character string specifying the separator for multi-response questions (default: "; ")
#'
#' @return A data frame with analysis results including labels and repeat group information
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' results <- analysis_func(df = survey_data, ap = analysis_plan)
#' 
#' # With custom multi-response separator
#' results <- analysis_func(df = survey_data, 
#'                         ap = analysis_plan, 
#'                         multi_response_sep = " & ")
#' }
#'
#' @export
analysis_func <- function(df, ap, multi_response_sep = "; "){
  
  multi_response_sep <<- multi_response_sep
  
  # Initialize variables
  restuls_dbl_disg <- NULL
  result_no_dbl_disag <- NULL
  
  if("repeat_for" %in% names(ap) & any(!is.na(ap$repeat_for))) {
    ap_dle_disg <- filter(ap, !is.na(repeat_for))
    ap_no_dbl_disg <- filter(ap, is.na(repeat_for))
    
    # Process repeat group analysis
    if(nrow(ap_dle_disg) > 0) {
      list_analysis <- list()
      for (col_i in unique(ap_dle_disg$repeat_for)) {
        for (j in unique(df[[col_i]])) {
          df_i <- df %>% filter(df[[col_i]] %in% j) # Updated == with %in%
          
          res <- analyze(df_i, ap_dle_disg)
          # res$repeat_for_col <- col_i
          res$repeat_for <- j
          
          list_analysis[[length(list_analysis)+1]] <- res
        }
      }
      
      if(length(list_analysis) > 0) {
        restuls_dbl_disg <- do.call(rbind, list_analysis)
      }
    }
    
    # Process non-repeat group analysis
    if(nrow(ap_no_dbl_disg) > 0) {
      result_no_dbl_disag <- analyze(df, ap_no_dbl_disg)
      result_no_dbl_disag$repeat_for <- NA
    }
  } else {
    # No repeat groups, process all analysis plan
    ap_no_dbl_disg <- ap
    if(nrow(ap_no_dbl_disg) > 0) {
      result_no_dbl_disag <- analyze(df, ap_no_dbl_disg)
      result_no_dbl_disag$repeat_for <- NA
    }
  }
  
  # Combine results
  if(!is.null(restuls_dbl_disg) & !is.null(result_no_dbl_disag)){
    results_merged <- rbind(result_no_dbl_disag, restuls_dbl_disg)
  } else if(!is.null(restuls_dbl_disg) & is.null(result_no_dbl_disag)){
    results_merged <- restuls_dbl_disg
  } else if(is.null(restuls_dbl_disg) & !is.null(result_no_dbl_disag)){
    results_merged <- result_no_dbl_disag
  } else {
    # Return empty result if no analysis performed
    results_merged <- data.frame(
      Disaggregation = character(0),
      Disaggregation_level = character(0),
      Question = character(0),
      Response = character(0),
      Aggregation_method = character(0),
      Result = numeric(0),
      Count = numeric(0),
      Denominator = numeric(0),
      repeat_for = character(0)
    )
  }
  
  # Join Label column
  results_merged <- results_merged %>% 
    left_join(ap %>% 
                select(Question=variable, Label=label) %>% unique()) %>% 
    left_join(ap %>% 
                select(Disaggregation=disaggregation, Disagg_Label=disagg_label) %>% unique()) %>% 
    relocate(Label, .after = Question) %>% 
    relocate(Disagg_Label, .after=Disaggregation)
  #
  
  return(results_merged)
}
