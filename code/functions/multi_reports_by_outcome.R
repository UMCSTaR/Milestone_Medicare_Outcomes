#' model summary reports based on which outcomes
#'
#' @param results Model results data
#' @param outcome name of the outcome from the model
#' @param outcome_name name showing on reports title
#' @param n_month number of months in practice (only 12, 24, and 30)

model_reports_by_outcome <- function(results, 
                                     outcome = "severe_cmp", 
                                     outcome_name = "Severe Complication (exclude POA)",
                                     n_month = 24) {
  outcome_all = names(results) %>% 
    as_tibble() %>% 
    filter(str_detect(value, outcome)) %>% 
    pull
  
  outcome_all_model = map(outcome_all, ~results[[.]])
  
  names(outcome_all_model) = str_remove(outcome_all, paste0(outcome, "_"))
  
  rmarkdown::render(
    "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/code/model_summary/model_summary_report.Rmd",
    params = list(data = outcome_all_model,
                  n_month = n_month,
                  outcome_name = outcome_name),
    output_dir = "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/reports",
    output_file = paste0(outcome,"-", n_month, "_months.html")
  )
}


kable_df <- function(..., digits=2) {
  kable(...,digits = digits, booktabs = T) %>% 
    kable_styling(full_width = F,
                  latex_options = c("striped", "hold_position"))
  # kable_styling(full_width = F)
}