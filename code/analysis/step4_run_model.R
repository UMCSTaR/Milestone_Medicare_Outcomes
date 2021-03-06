# Goal:  multiple outcomes, multiple procedures, milestone ratings 
# create models based on procedures
# milestone rating: overall mean, operative mean, professional mean, >8 binary

library(tidyverse)

# load data
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_pc_primary.rdata")


# decided not to use 9
# add cut off 9 binary rating indicator
# milestone_medicare_pc_primary = milestone_medicare_pc_primary %>% 
#   mutate(overall_eql9 = ifelse(IntResponseValue_mean == 9, "=9","<9"),
#          prof_eql9 = ifelse(prof_rating_mean == 9, "=9","<9"),
#          op_eql9 = ifelse(operative_rating_mean == 9, "=9","<9"),
#          leader_eql9 = ifelse(leadership_rating_mean == 9, "=9","<9"))

# # fix AHRQ score
# ahrq = read_csv("/Volumes/George_Surgeon_Projects/standardized_medicare_data_using_R/analytic/full_data/archive/ahrq.csv")
# 
# milestone_medicare_ratings = milestone_medicare_ratings %>% 
#   select(-AHRQ_score) %>%
#   left_join(ahrq) 
# 
# milestone_medicare_ratings = milestone_medicare_ratings %>% 
#   mutate(AHRQ_score_scale = scale(AHRQ_score),
#          AHRQ_score_scale = as.numeric(AHRQ_score_scale))
# 
# save(milestone_medicare_ratings, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_ratings.rdata")

# input-----
n_months = 24
# interaction term --
# interaction_term = "had_assist_surg"
interaction_term = NULL # no interaction
# cutoff year
cutoff = 8
# including case volume
case_vol = TRUE


# data process
main_data = milestone_medicare_pc_primary %>% 
  filter(month<=n_months)

n_distinct(main_data$id_physician_npi)

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
)



# Covariate list ----------------------------------------------------------

covariates = c(
  'flg_male',
  'age_at_admit_scale',
  'race_white',
  # 'race_hisp_other',
  'flg_admit_emerg',
  'AHRQ_score_scale',
  'ses_2grp',
  # 'cpt_cd',
  'facility_clm_yr',
  # 'flg_multi_surgeon', # do not use this var
  'had_assist_surg',
  'cases_per_12month',
  'hosp_beds_2grp',
  # 'flg_hosp_ICU_hosp',
  'val_hosp_mcday2inptday_ratio',
  'val_hosp_rn2bed_ratio'
)

# check if all vars in the data
all(covariates %in% names(main_data))

# remove case volume covariate if desire
if(case_vol == FALSE) {
  n_val = grep("cases_per_12month", covariates)
  covariates = covariates[-n_val]
}


# Model formula -----------------------------------------------------------
source("code/functions/create_formula.R")

# Model Function ----------------------------------------------------------
source("code/functions/run_model.R")


# Run models --------------------------------------------------

# milestone ratings --------


# milestone ratings for 9 cutoff--------
if(cutoff == 8){
  primaries = c(
    "IntResponseValue_mean",
    "mean_ge_8",  
    
    "prof_rating_mean",
    "prof_rating_ge8",    
    
    "operative_rating_mean",
    "operative_rating_ge8",        
    
    "leadership_rating_mean",
    "leadership_rating_ge8"     
  )
} else if (cutoff == 9) {
  primaries = c(
    "overall_eql9",
    "prof_eql9",
    "op_eql9",
    "leader_eql9"
  )
}


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

# interaction with assistant surgeon case
if(!is.null(interaction_term)){
  fs = create_formulas(
    y = outcomes,
    primary_covariate = primaries,
    other_covariates = covariates,
    interaction_term = interaction_term,
    # random_effects = c('id_physician_npi', 'facility_prvnumgrp')
    # random_effects = c('id_physician_npi', 'facility_prvnumgrp', 'cpt_cd')
    random_effects = c('id_physician_npi', 'cpt_cd')
  )
}

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
         # pred = ifelse(str_detect(fs, "never_less_8_rating"), "never_less_8", pred),
         pred = ifelse(str_detect(fs, "mean_ge_8"), "mean_ge_8", pred),
         pred = ifelse(str_detect(fs, "overall_eql9"), "overall_eql9", pred),
         
         pred = ifelse(str_detect(fs, "prof_rating_mean"), "prof", pred),
         pred = ifelse(str_detect(fs, "prof_rating_ge8"), "prof_ge8", pred),
         pred = ifelse(str_detect(fs, "prof_eql9"), "prof_eql9", pred),
         
         pred = ifelse(str_detect(fs, "operative_rating_mean"), "operative", pred),
         pred = ifelse(str_detect(fs, "operative_rating_ge8"), "operative_ge8", pred),
         pred = ifelse(str_detect(fs, "op_eql9"), "op_eql9", pred),
         
         pred = ifelse(str_detect(fs, "leadership_rating_mean"), "leadership", pred),
         pred = ifelse(str_detect(fs, "leadership_rating_ge8"), "leadership_ge8", pred),
         pred = ifelse(str_detect(fs, "leader_eql9"), "leader_eql9", pred)) %>% 
  mutate(model_name = paste(proc_name, outcome, pred, sep = "_")) %>% 
  pull(model_name)


results = pmap(list(formula = model_ls$fs,
                    method = model_ls$ml,
                    proc = model_ls$proc), run_models)

names(results) = model_name

# example
summary(results$Par_any_cmp_leadership_ge8)

# save model ---------
if (is.null(interaction_term) &
    cutoff == 8) {
  if (case_vol == TRUE) {
    save(results,
         file  = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc.rdata")
  } else {
    save(results,
         file  = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc_no_case_vol.rdata")
  }
  
} else if (!is.null(interaction_term) & cutoff == 8) {
  
  if (case_vol == TRUE) {
    save(results,
         file  = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc_interaction.rdata")
  } else {
    save(results,
         file  = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc_interaction_no_case_vol.rdata")
  }
} else if (cutoff == 9) {
  save(results,
       file  = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc_cutoff9.rdata")
}



# QA death-------
# univariate model for death ----
model_univariate_death = glm(
  formula = "flg_death_30d ~ IntResponseValue_mean",
  data = main_data,
  family = 'binomial'
)


uni_death_model_no_limit_model_sum = broom::tidy(model_univariate_death)

save(uni_death_model_no_limit, file = "data/uni_death_model_no_limit_model.rdata")

