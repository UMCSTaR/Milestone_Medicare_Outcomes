# Goal:  multiple outcomes, milestone ratings for
# milestone rating: overall mean, operative mean, profesisonal mean
library(tidyverse)
library(purrr)


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
source("code/functions/create_formula.R")

# Model Function ----------------------------------------------------------
source("code/functions/run_model.R")


# Run models --------------------------------------------------

primaries = c("IntResponseValue_mean", "prof_rating_mean", "operative_rating_mean",
            "ever_less_7_rating")
procedures = main_data %>% 
  count(e_proc_grp_lbl) %>% 
  filter(e_proc_grp_lbl != "multi_procedures") %>% 
  pull(e_proc_grp_lbl) 
  

fs = create_formulas(
  y = outcomes,
  primary_covariate = primaries,
  other_covariates = covariates,
  interaction_term = NULL,
  # random_effects = c('id_physician_npi', 'facility_prvnumgrp')
  random_effects = c('id_physician_npi', 'facility_prvnumgrp', 'cpt_cd')
)

names(fs) = outcomes


model_ls = expand.grid(fs = fs,
            ml = "glmmTMB",
            proc = procedures) %>% 
  unnest(fs) 

model_name = model_ls %>% 
  mutate(proc_name = str_sub(proc,1,3)) %>% 
  mutate(outcome = ifelse(str_detect(fs, "severe"), "severe_cmp", NA),
         outcome = ifelse(str_detect(fs, "any"), "any_cmp", outcome),
         outcome = ifelse(str_detect(fs, "readmit"), "readmit", outcome),
         outcome = ifelse(str_detect(fs, "death"), "death", outcome),
         outcome = ifelse(str_detect(fs, "reop"), "reop", outcome),
         outcome = ifelse(str_detect(fs, "los"), "los", outcome),
         outcome = ifelse(str_detect(fs, "but_death"), "but_death", outcome)
         ) %>% 
  mutate(pred = ifelse(str_detect(fs, "IntResponseValue_mean"), "all_mean", NA),
         pred = ifelse(str_detect(fs, "prof_rating_mean"), "prof", pred),
         pred = ifelse(str_detect(fs, "operative_rating_mean"), "operative", pred),
         pred = ifelse(str_detect(fs, "ever_less_7_rating"), "less_7", pred),) %>% 
  mutate(model_name = paste(proc_name, outcome, pred, sep = "_")) %>% 
  pull(model_name)


results = pmap(list(formula = model_ls$fs,
                    method = model_ls$ml,
                    proc = model_ls$proc), run_models)

names(results) = model_name


# save(results, file = paste0("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/",
#                             primary, "_",
#                             str_replace(procedure, " ", "_"),".rdata"))



