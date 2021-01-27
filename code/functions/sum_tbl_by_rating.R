# use for cohort descriptive stats report

sum_tbl_by_rating <- function(data = milestone_medicare_pc,
                              rating_var = mean_ge_8) {
  
  data %>%
    select(
      {{rating_var}},
      flg_male,
      age_at_admit,
      race_white,
      AHRQ_score,
      flg_admit_emerg,
      had_assist_surg,
      cases_per_12month,
      colectomy_type,
      hosp_beds,
      val_hosp_mcday2inptday_ratio,
      val_hosp_rn2bed_ratio
    ) %>% 
    rename(emergent_admission = flg_admit_emerg) %>% 
    mutate({{rating_var}} := ifelse({{rating_var}} == 0 , "mean<8", "mean>=8")) %>% 
    tbl_summary(by = {{rating_var}},
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
