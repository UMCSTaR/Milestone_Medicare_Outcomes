# edit OR table labels for each outcomes and 4 ratings
# for manuscript presentation

clean_or_table_labels <- function(data) {
  data %>%
    mutate(
      outcome = factor(outcome, levels = c(
        "any_cmp", "readmit", "severe_cmp", "death"
      )),
      outcome = case_when(
        outcome == "any_cmp" ~ "Any Complication",
        outcome == "readmit" ~ "Readmission",
        outcome == "severe_cmp" ~ "Severe Complication",
        outcome == "death" ~ "Death"
      ),
      rating = str_to_title(rating),
      rating = ifelse(rating == "Overall_mean", "Overall", rating)
    ) %>%
    rename(
      "Patient outcomes" = outcome,
      Rating = rating,
      "Odds ratio" = OR,
      "p-value" = p_value
    ) 
}

