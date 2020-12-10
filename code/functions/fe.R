fe = function(model, pdf = TRUE) {
  # fe
  fe = extract_fixed_effects(model) %>%
    mutate(
      OR = exp(value),
      OR_lower = exp(lower_2.5),
      OR_upper = exp(upper_97.5),
      term = ifelse(term == "IntResponseValue_mean", "overall mean", term)
    ) %>%
    rename(Estimate = value)
  
  if (pdf == TRUE) {
    fe %>%
      select(term, starts_with('OR'), everything())  %>%
      kable_df() %>%
      pack_rows('Milestone Rating', 2, 2) %>%
      pack_rows('Patient', 3, 10) %>%
      pack_rows('Hospital', 11, 13)  
  } else {
    fe %>%
      select(term, contains("OR"), se, p_value) %>%
      filter(term != "Intercept") %>%
      mutate(
        term = case_when(
          term == "hosp_beds_2grp" ~ "hosp_beds(>=350)",
          term == "AHRQ_score_scale" ~ "comorbidity_score",
          term == "ses_2grp" ~ "SES",
          term == "val_hosp_mcday2inptday_ratio" ~ "hosp_mcday2inptday_ratio",
          term == "val_hosp_rn2bed_ratio" ~ "hosp_rn2bed_ratio",
          term == "flg_admit_emerg" ~ "emergent_admission",
          term == "facility_clm_yr" ~ "claim_year",
          TRUE ~ as.character(term)
        )
      ) %>%
      rename("Fixed Effects" = term) 
  }
  
}
