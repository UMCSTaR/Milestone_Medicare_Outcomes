# adding labels and var without dropping any observations
prep_data_for_model <- function(
  data, 
  standardize = vars(age_at_admit, AHRQ_score),
  collapse_race = TRUE,
  proc = NULL
) {
  
  # package requirements ----------------------------------------------------
  if(!requireNamespace('tidyverse') | !requireNamespace("tidyext"))
    stop('tiydverse and tidyext required. devtools::install_github(m-clark/tidyext)')
  
  
  # # potential filters -------------------------------------------------------
  # # cohort selection (may already be done)
  # data = data %>% 
  #   filter(id_physician_npi != "" &       # drop physician NPI
  #            !is.na(val_yr_practice))     # drop year after graduation NA
  
  
  # add flg to indicate surgeons at least performed 4 out of 5 procedures
  # at least 1 case for 4 proc (e) 
  at_least_one_proc_medicare_surgeons = data %>% 
    filter(e_proc_grp_lbl != "Ventral Hernia Repair") %>% 
    group_by(id_physician_npi) %>% 
    mutate(n_proc_per_surg = n_distinct(e_proc_grp_lbl)) %>% 
    arrange(id_physician_npi, e_proc_grp_lbl) %>% 
    ungroup() %>% 
    filter(n_proc_per_surg == 4) %>% 
    distinct(id_physician_npi)
  
  data = data %>% 
    mutate(gs_perform_4 = ifelse(id_physician_npi %in% at_least_one_proc_medicare_surgeons$id_physician_npi, 1, 0))
  
  # make a procedure category for multiple procedure 
  data = data %>% 
    mutate(e_proc_grp_lbl = ifelse(flg_multi_cpt == 1, "multi_procedures", e_proc_grp_lbl))
  
  
  # deal with procedure filter and 
  if (!is.null(proc)) {
    data = data %>% 
      filter(e_proc_grp_lbl %in% proc)
  } else {
    # onehot proc group: 
    # 1 Appendectomy              
    # 2 Cholecystectomy       
    # 3 Groin Hernia Repair 
    # 4 multi_procedures      
    # 5 Partial Colectomy     
    # 6 Ventral Hernia Repair 
    
    data = data %>% 
      tidyext::onehot(var = 'e_proc_grp_lbl', keep.original = T) %>% 
      rename(
        proc_Appendectomy          = e_proc_grp_lbl_Appendectomy, 
        proc_Partial_Colectomy     = e_proc_grp_lbl_Partial.Colectomy,
        proc_Cholecystectomy       = e_proc_grp_lbl_Cholecystectomy,
        proc_Groin_Hernia_Repair   = e_proc_grp_lbl_Groin.Hernia.Repair,
        proc_Ventral_Hernia_Repair = e_proc_grp_lbl_Ventral.Hernia.Repair,
        proc_multi_procedures      = e_proc_grp_lbl_multi_procedures
      )
    
  }
  
  
  # standardize numeric  and keep original----------------------------------------------------- 
  data = data %>% 
    mutate_at(standardize, .funs = list(scale = function(x) scale(x)[,1]))

  
  # onehot categorical ------------------------------------------------------
  
  # race; wbho = 1234
  data = data %>% 
    onehot(var = 'e_race_wbho', keep.original = T) %>% 
    rename(
      race_white = e_race_wbho_1,
      race_black = e_race_wbho_2
    )
  
  if (collapse_race) {
    data = data %>% 
      mutate(
        race_hisp_other = e_race_wbho_3 | e_race_wbho_4
      )
  } else {
    data = data %>% 
      rename(
        race_hispanic = e_race_wbho_3,
        race_other    = e_race_wbho_4
      )
  }
  
  # add lable for race
  data = data %>% 
    mutate(e_race_wbho_lable = ifelse(e_race_wbho == 1, "Non-Hispanic White", NA),
           e_race_wbho_lable = ifelse(e_race_wbho == 2, "Non-Hispanic Black", e_race_wbho_lable),
           e_race_wbho_lable = ifelse(e_race_wbho == 3, "Hispanic", e_race_wbho_lable),
           e_race_wbho_lable = ifelse(e_race_wbho == 4, "Other", e_race_wbho_lable))
  
  
  
  # e_admit_type 1- emergency 2 urgent 3 elective 4 other 9 UK/missing
  # Given that 'other' is indistinguishable from unknown, which was lumped with
  # missing, other will be given NA as well.  It has very few values anyway.  This was checked for appropriate NA
  
  data = data %>% 
    mutate(e_admit_type = ifelse(e_admit_type == "4_Other" | e_admit_type == "9-Unknown/Missing", 
                                 NA, 
                                 e_admit_type)) %>% 
    mutate(flg_admit_emerg = ifelse(e_admit_type == "3_Elective", 0, 1))
  
  
  
  # dichotomize Hospital Bed size
  data = data %>% 
    mutate(hosp_beds_2grp = ifelse(e_hosp_beds_4grp == 1 | e_hosp_beds_4grp == 2, 
                                   0, 
                                   1),
           hosp_beds_2grp = ifelse(is.na(e_hosp_beds_4grp),
                                   NA, 
                                   hosp_beds_2grp))
  
  #dichotomize SES
  data = data %>% 
    mutate(ses_2grp = ifelse(e_ses_5grp == 1 | e_ses_5grp == 2, 
                             0, 
                             1),
           ses_2grp = ifelse(is.na(e_ses_5grp) , 
                             NA, 
                             ses_2grp))
  
  # convert logical to integers ---------------------------------------------
  data = data %>% 
    mutate_at(vars(matches('^race|^proc|^flg_admit_emerg')), as.integer)
  
  
  
  # misc other --------------------------------------------------------------
  
  # Add to this as necessary
  # quintiles actually aren't quintiles because there is only info for 1:5 and
  # yet there is a zero category.  Presumably missing.
  data = data %>% 
    mutate(
      id_physician_npi = factor(id_physician_npi),
      facility_prvnumgrp = factor(facility_prvnumgrp),
      e_ses_5grp = ifelse(e_ses_5grp == 0, NA, e_ses_5grp),
      facility_clm_yr = facility_clm_yr - 2007
    ) 
  
  
  # return ------------------------------------------------------------------
  
  as_tibble(data)
}
