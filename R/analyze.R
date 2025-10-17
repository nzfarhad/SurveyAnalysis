#' Main Analysis Function
#'
#' Performs analysis based on an analysis plan, handling different question types
#' and aggregation methods with support for disaggregation.
#'
#' @param df A data frame containing the survey data
#' @param analysis_plan A data frame specifying the analysis plan with columns:
#'   \itemize{
#'     \item variable: Column names to analyze
#'     \item kobo_type: Type of question ("select_one", "select_multiple", "integer")
#'     \item aggregation_method: Method for aggregation ("proportion", "mean", "median", "sum", etc.)
#'     \item disaggregation: Variable for disaggregation ("all" for no disaggregation)
#'   }
#'
#' @return A data frame with analysis results
#'
#' @examples
#' \dontrun{
#' # Create analysis plan
#' analysis_plan <- data.frame(
#'   variable = c("gender", "age"),
#'   kobo_type = c("select_one", "integer"),
#'   aggregation_method = c("perc", "mean"),
#'   disaggregation = c("region", "all")
#' )
#' 
#' # Run analysis
#' results <- analyze(df = survey_data, analysis_plan = analysis_plan)
#' }
#'
#' @export
analyze <- function(df, analysis_plan){
  
  res_list <- list()
  ap_len <- nrow(analysis_plan)
  prog_counter <- 1/ap_len
  
  for (i in 1:ap_len) {
    ap_var <- analysis_plan$variable[i]
    ap_kobo_type <- analysis_plan$kobo_type[i]
    ap_agr_type <- analysis_plan$aggregation_method[i]
    ap_disagg <- analysis_plan$disaggregation[i]
    
    cat(paste(ap_var,  "-", "Done\n"))
    
    if(ap_disagg == "all" | is.na(ap_disagg) ){
      
      if(ap_kobo_type == "select_one" & (ap_agr_type == "proportion" | ap_agr_type == "perc")){
        temp <-  single_select(df, ap_var, ap_disagg, ap_disagg, format_output = FALSE)
        res_list[[i]] <- temp
        
      }
      
      if (ap_kobo_type == "select_multiple" & (ap_agr_type == "proportion" | ap_agr_type == "perc")) {
        multi_temp <- multi_select(df, ap_var, ap_disagg, ap_disagg, format_output = FALSE)
        res_list[[i]] <- multi_temp
      }
      
      if(ap_kobo_type == "integer" & ap_agr_type == "mean"){
        mean_temp <- stat_mean(df, ap_var, ap_disagg, ap_disagg, format_output = FALSE)
        res_list[[i]] <- mean_temp
      }
      
      if(ap_kobo_type == "integer" & ap_agr_type == "median"){
        med_temp <- stat_median(df, ap_var, ap_disagg, ap_disagg, format_output = FALSE)
        res_list[[i]] <- med_temp
      }
      
      if(ap_kobo_type == "integer" & ap_agr_type == "sum"){
        sum_temp <- stat_sum(df, ap_var, ap_disagg, ap_disagg, format_output = FALSE)
        res_list[[i]] <- sum_temp
      }
      
      if(ap_kobo_type == "integer" & ap_agr_type == "firstq"){
        first_temp <- stat_1stq(df, ap_var, ap_disagg, ap_disagg, format_output = FALSE)
        res_list[[i]] <- first_temp
      }
      
      if(ap_kobo_type == "integer" & ap_agr_type == "thirdq"){
        third_temp <- stat_3rdq(df, ap_var, ap_disagg, ap_disagg, format_output = FALSE)
        res_list[[i]] <- third_temp
      }
      
      if(ap_kobo_type == "integer" & ap_agr_type == "min"){
        min_temp <- stat_min(df, ap_var, ap_disagg, ap_disagg, format_output = FALSE)
        res_list[[i]] <- min_temp
      }
      
      if(ap_kobo_type == "integer" & ap_agr_type == "max"){
        max_temp <- stat_max(df, ap_var, ap_disagg, ap_disagg, format_output = FALSE)
        res_list[[i]] <- max_temp
      }
      
    }
    
    if(ap_disagg != "all"){
      
      
      for (var in unique(df[[ap_disagg]])) {
        
        df_sub <- df %>% filter(df[[ap_disagg]] == var)
        cat(paste(ap_var, "-", var, "-", "Done\n"))
        
        if(ap_kobo_type == "select_one" & (ap_agr_type == "proportion" | ap_agr_type == "perc")){
          temp <-  single_select(df_sub, ap_var, ap_disagg, var, format_output = FALSE)
          res_list[[length(res_list)+1]] <- temp
          
        }
        
        if (ap_kobo_type == "select_multiple" & (ap_agr_type == "proportion" | ap_agr_type == "perc")) {
          multi_temp <- multi_select(df_sub, ap_var, ap_disagg, var, format_output = FALSE)
          res_list[[length(res_list)+1]] <- multi_temp
        }
        
        if(ap_kobo_type == "integer" & ap_agr_type == "mean"){
          mean_temp <- stat_mean(df_sub, ap_var, ap_disagg, var, format_output = FALSE)
          res_list[[length(res_list)+1]] <- mean_temp
        }
        
        if(ap_kobo_type == "integer" & ap_agr_type == "median"){
          med_temp <- stat_median(df_sub, ap_var, ap_disagg, var, format_output = FALSE)
          res_list[[length(res_list)+1]] <- med_temp
        }
        
        if(ap_kobo_type == "integer" & ap_agr_type == "sum"){
          sum_temp <- stat_sum(df_sub, ap_var, ap_disagg, var, format_output = FALSE)
          res_list[[length(res_list)+1]] <- sum_temp
        }
        
        if(ap_kobo_type == "integer" & ap_agr_type == "firstq"){
          first_temp <- stat_1stq(df_sub, ap_var, ap_disagg, var, format_output = FALSE)
          res_list[[length(res_list)+1]] <- first_temp
        }
        
        if(ap_kobo_type == "integer" & ap_agr_type == "thirdq"){
          third_temp <- stat_3rdq(df_sub, ap_var, ap_disagg, var, format_output = FALSE)
          res_list[[length(res_list)+1]] <- third_temp
        }
        
        if(ap_kobo_type == "integer" & ap_agr_type == "min"){
          min_temp <- stat_min(df_sub, ap_var, ap_disagg, var, format_output = FALSE)
          res_list[[length(res_list)+1]] <- min_temp
        }
        
        if(ap_kobo_type == "integer" & ap_agr_type == "max"){
          max_temp <- stat_max(df_sub, ap_var, ap_disagg, var, format_output = FALSE)
          res_list[[length(res_list)+1]] <- max_temp
        }
        
        
      }
      
      
    }
    
    
  }
  
  
  result <- do.call(rbind, res_list) %>%
    select( Disaggregation = disaggregation,
            Disaggregation_level = disagg_level,
            Question = variable,
            Response = Var1,
            Aggregation_method = aggregation_method,
            Result = Freq,
            Count = count,
            Denominator = valid,)
  
  
  return(result)
  
}
