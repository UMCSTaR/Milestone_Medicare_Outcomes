# Goal:  multiple outcomes, milestone ratings for
# milestone rating: overall mean, operative mean, profesisonal mean
library(tidyverse)
library(furrr)


load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_ratings.rdata")

main_data = milestone_medicare_ratings


# note that the variable name for QE/CE status may change
main_data =  main_data %>% 
  mutate(los_gt_75perc = ifelse(val_los > quantile(val_los, probs = 0.75), 1, 0),
  ) %>% 
  tidyext::row_sums(flg_cmp_po_severe_poa, flg_cmp_po_any_poa, flg_readmit_30d, los_gt_75perc, flg_util_reop,
                    varname = 'flg_any_but_death') %>% 
  mutate(flg_any_but_death = as.integer(flg_any_but_death > 0),
         flg_any_but_death = ifelse(flg_death_30d, NA, flg_any_but_death))


# Outcome list ------------------------------------------------------------

outcomes = c(
  'flg_cmp_po_severe_poa',
  'flg_cmp_po_any_poa',
  'flg_readmit_30d',
  'flg_death_30d',
  'flg_util_reop',
  'los_gt_75perc',
  'flg_any_but_death'
)



# Covariate list ----------------------------------------------------------

covariates = c(
  'flg_male',
  'age_at_admit_scale',
  'race_white',
  'race_hisp_other',
  'flg_admit_emerg',
  'AHRQ_score_scale',
  'ses_2grp',
  # 'cpt_cd',
  'facility_clm_yr',
  'flg_multi_surgeon',
  'flg_assistant_surgeon',
  'hosp_beds_2grp',
  # 'flg_hosp_ICU_hosp',
  'val_hosp_mcday2inptday_ratio',
  'val_hosp_rn2bed_ratio'
)

# choose primary variable -----------

# primary = 'IntResponseValue_mean'   # overall mean
# primary = 'operative_rating_mean'   # operative mean by (PC3, MK2, ICS3)
# primary = 'prof_rating_mean'        # professionalism bhy (Prof1, Prof 2, Prof3)
# primary = 'ever_less_7_rating'        # ever had less than 7 rating



# Model formula -----------------------------------------------------------

create_formulas <- function(
  y = outcomes,
  primary_covariate = 'IntResponseValue_mean',
  other_covariates = covariates,
  random_effects = 'id_physician_npi',
  interaction_term = NULL,
  mgcv = FALSE
) {
  covariates_all = paste0(
    primary_covariate, 
    ifelse(!is.null(interaction_term),' * ', ''), 
    ifelse(!is.null(interaction_term), interaction_term, ''), 
    ' + ',
    paste0(covariates, collapse = ' + ')
  )
  
  
  if (mgcv)
    re = paste0(' + s(', random_effects, ", bs = 're')", collapse = '')
  # " + s(id_physician_npi, bs='re') + s(facility_prvnumgrp, bs = 're')"
  else 
    re = paste0(' + (1 | ', random_effects, ')', collapse = '')
  
  lapply(y, function(y) paste0(paste0(y, ' ~ '), covariates_all, re))
}



# Model Function ----------------------------------------------------------

run_models <- function(
  formula,
  data = main_data,
  method = 'lme4',
  proc = NULL,
  ...
) {
  
  if (!is.null(proc)) data = dplyr::filter(data, e_proc_grp_lbl %in% proc)
  
  if (method == 'lme4') {
    
    f = as.formula(formula)
    
    model = lme4::glmer(
      formula = f,
      data = data,
      family = 'binomial',
      ...
    )
  }
  
  if (method == 'glmmTMB') {
    
    f = as.formula(formula)
    
    model = glmmTMB::glmmTMB(
      formula = f,
      data = data,
      family = 'binomial',
      ...
    )
  }
  
  if (method == 'mgcv') {
    
    f = as.formula(formula)
    
    model = mgcv::bam(
      formula = f,
      data = data,
      family = 'binomial',
      ...
    )
  }
  
  if (method == 'brms') {
    
    f = as.formula(formula)
    
    model = brms::brm(
      formula = f,
      data = data,
      family = 'binomial',
      cores = 2,  # this will max out total cores used given 6 outcomes
      verbose = FALSE,
      ...
    )
  }
  
  model
} 

# Run models --------------------------------------------------

primary = "IntResponseValue_mean"
procedure = "Partial Colectomy"

procedures = main_data %>% 
  count(e_proc_grp_lbl) %>% 
  filter(e_proc_grp_lbl != "multi_procedures") %>% 
  pull(e_proc_grp_lbl) 
  


fs = create_formulas(
  y = outcomes,
  primary_covariate = primary,
  other_covariates = covariates,
  interaction_term = NULL,
  # random_effects = c('id_physician_npi', 'facility_prvnumgrp')
  random_effects = c('id_physician_npi', 'facility_prvnumgrp', 'cpt_cd')
)

names(fs) = outcomes

results = 
  pmap(list(formula = fs,
            proc = procedure,
            method = 'glmmTMB')
      ,run_models)


save(results, file = paste0("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/",
                            primary, "_",
                            str_replace(procedure, " ", "_"),".rdata"))
