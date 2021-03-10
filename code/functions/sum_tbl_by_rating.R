# use for cohort descriptive stats report

sum_tbl_by_rating <- function(data = milestone_medicare_pc,
                              rating_var = mean_ge_8) {
  
  theme_gtsummary_journal(
    journal = c("jama"),
    set_theme = TRUE
  )
  
  data %>%
    transmute(
      {{rating_var}},
      `Male Gender` = flg_male,
      `Race - White` = race_white,
      `Emergency Admission Status` = flg_admit_emerg,
      `Mean Age on admission, yr` = age_at_admit,
      `Mean AHRQ Elixhauser Index` = AHRQ_score,
      `Hospital Beds ≥ 350` = hosp_beds,
      `Colectomy Type` = colectomy_type,
      `Presence of Assisting Surgeon` = had_assist_surg,
      hosp_mcday2inptday = val_hosp_mcday2inptday_ratio,
      hosp_rn2bed = val_hosp_rn2bed_ratio
    ) %>% 
    mutate({{rating_var}} := ifelse({{rating_var}} == 0 , "mean<8", "mean>=8")) %>% 
    tbl_summary(by = {{rating_var}},
                statistic = list(
                  # all_continuous() ~ "{mean} ({sd}) [{p25}, {p75}]",
                  all_continuous() ~ "{mean} ({sd})",
                  all_categorical() ~ "{n} ({p}%)"
                ),
                missing = "no") %>%
    add_overall() %>%
    add_p()
}

sum_tbl_by_rating_assist <- function(data = milestone_medicare_pc,
                                     rating_var = mean_ge_8) {
  
  data %>%
    select(!!rlang::sym(rating_var),
           had_assist_surg,
           hosp_beds) %>%
    mutate(!!rlang::sym(rating_var) := ifelse(!!rlang::sym(rating_var) == 0 , "mean<8", "mean>=8")) %>% 
    tbl_summary(by = !!rlang::sym(rating_var),
                missing = "no") %>%
    add_overall() %>%
    add_p() 
}
