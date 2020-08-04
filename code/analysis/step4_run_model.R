# Goal:  multiple outcomes, multiple procedures, milestone ratings 
# create models based on procedures
# milestone rating: overall mean, operative mean, professional mean, >7 binary

library(tidyverse)
library(purrr)

# load data
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_ratings.rdata")

n_months = 36

main_data = milestone_medicare_ratings %>% 
  filter(month<=n_months)

# create patient outcomes variables
main_data =  main_data %>% 
  mutate(los_gt_75perc = ifelse(val_los > quantile(val_los, probs = 0.75), 1, 0),
         flg_cmp_po_any_not_poa = as.numeric(flg_cmp_po_any_not_poa),
         flg_cmp_po_any_not_poa = as.numeric(flg_cmp_po_any_not_poa),
         flg_readmit_30d = as.numeric(flg_readmit_30d),
         los_gt_75perc = as.numeric(los_gt_75perc),
         flg_util_reop = as.numeric(flg_util_reop)
  ) %>% 
  tidyext::row_sums(flg_cmp_po_severe_not_poa, flg_cmp_po_any_not_poa, flg_readmit_30d, los_gt_75perc, flg_util_reop,
                    varname = 'flg_any_but_death') %>% 
  mutate(flg_any_but_death = as.integer(flg_any_but_death > 0),
         flg_any_but_death = ifelse(flg_death_30d, NA, flg_any_but_death))


# Outcome list ------------------------------------------------------------

outcomes = c(
  'flg_cmp_po_severe_not_poa',
  'flg_cmp_po_any_not_poa',
  'flg_readmit_30d',
  'flg_death_30d'
  # 'flg_util_reop',
  # 'los_gt_75perc',
  # 'flg_any_but_death'
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

# check if all vars in the data
all(covariates %in% names(main_data))


# Model formula -----------------------------------------------------------
source("code/functions/create_formula.R")

# Model Function ----------------------------------------------------------
source("code/functions/run_model.R")


# Run models --------------------------------------------------

# milestone ratings --------
primaries = c(
  "IntResponseValue_mean",
  "never_less_8_rating",
  "mean_ge_8",  
  
  "prof_rating_mean",
  "prof_rating_ge8",    
  
  "operative_rating_mean",
  "operative_rating_ge8",        
                   
  "leadership_rating_mean",
  "leadership_rating_ge8"     
)

# check if all vars in the data
all(primaries %in% names(main_data))

# procedures ---------------
# 5 major procedure
select_proc <- function(proc) {
  if (proc == "all") {
  main_data %>% 
    count(e_proc_grp_lbl) %>% 
    filter(e_proc_grp_lbl != "multi_procedures") %>% 
    pull(e_proc_grp_lbl) 
  } else {
  proc
  }
}

# choose procedure -------
# "Appendectomy"
# "Cholecystectomy"
# "Groin Hernia Repair"
# "Partial Colectomy"
# "Ventral Hernia Repair"
# all: all 5 procedures
procedure = select_proc("Partial Colectomy")
  
# make model formulas ------------
fs = create_formulas(
  y = outcomes,
  primary_covariate = primaries,
  other_covariates = covariates,
  interaction_term = NULL,
  # random_effects = c('id_physician_npi', 'facility_prvnumgrp')
  # random_effects = c('id_physician_npi', 'facility_prvnumgrp', 'cpt_cd')
  random_effects = c('id_physician_npi', 'cpt_cd')
)

names(fs) = outcomes

# make model formula lists with different procedures
model_ls = expand.grid(fs = fs, 
              ml = "glmmTMB",
              proc = procedure) %>% 
    unnest(fs) 

# create model names based on procedure, outcomes and 
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
         pred = ifelse(str_detect(fs, "never_less_8_rating"), "never_less_8", pred),
         pred = ifelse(str_detect(fs, "mean_ge_8"), "mean_ge_8", pred),
         
         pred = ifelse(str_detect(fs, "prof_rating_mean"), "prof", pred),
         pred = ifelse(str_detect(fs, "prof_rating_ge8"), "prof_ge8", pred),
         
         pred = ifelse(str_detect(fs, "operative_rating_mean"), "operative", pred),
         pred = ifelse(str_detect(fs, "operative_rating_ge8"), "operative_ge8", pred),
         
         pred = ifelse(str_detect(fs, "leadership_rating_mean"), "leadership", pred),
         pred = ifelse(str_detect(fs, "leadership_rating_ge8"), "leadership_ge8", pred)) %>% 
  mutate(model_name = paste(proc_name, outcome, pred, sep = "_")) %>% 
  pull(model_name)


results = pmap(list(formula = model_ls$fs,
                    method = model_ls$ml,
                    proc = model_ls$proc), run_models)

names(results) = model_name

# example
summary(results$Par_severe_cmp_all_mean)

# save model ---------
if (n_months == 12) {
  save(results,
       file  = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month12_pc.rdata")
} else if (n_months == 24) {
  save(results,
       file  = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc.rdata")
} else if (n_months == 36) {
  save(results,
       file  = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_non_limit_pc.rdata")
}




