# Goal:  multiple outcomes, milestone ratings

library(tidyverse)

load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_pc.rdata")

main_data = milestone_medicare_pc


# note that the variable name for QE/CE status may change
main_data =  main_data %>% 
  mutate(los_gt_75perc = ifelse(val_los > quantile(val_los, probs = 0.75), 1, 0),
  ) %>% 
  tidyext::row_sums(flg_cmp_po_severe, flg_cmp_po_any, flg_readmit_30d, los_gt_75perc, flg_util_reop,
                    varname = 'flg_any_but_death') %>% 
  mutate(flg_any_but_death = as.integer(flg_any_but_death > 0),
         flg_any_but_death = ifelse(flg_death_30d, NA, flg_any_but_death))


# Outcome list ------------------------------------------------------------

outcomes = c(
  'flg_cmp_po_severe',
  'flg_cmp_po_any',
  'flg_readmit_30d',
  'flg_death_30d',
  'flg_util_reop',
  'los_gt_75perc',
  'flg_any_but_death'
)



# Covariate list ----------------------------------------------------------

covariates = c(
  'flg_male',
  'age_at_admit',
  'race_white',
  'race_hisp_other',
  'flg_admit_emerg',
  'AHRQ_score',
  'ses_2grp',
  'cpt_cd',
  'facility_clm_yr',
  'flg_multi_surgeon',
  'flg_assistant_surgeon',
  'hosp_beds_2grp',
  'flg_hosp_ICU_hosp',
  'val_hosp_mcday2inptday_ratio',
  'val_hosp_rn2bed_ratio'
)

# primary = 'qe_ce_cat'
primary = 'IntResponseValue_mean'

# if single proc
covariates_all = paste0(primary, ' + ', paste0(covariates, collapse = ' + '))
covariates_all

# if no interaction
# covariates_all = paste0(primary, ' + ', procedures, ' + ', paste0(covariates, collapse = ' + '))
# covariates_all

# interaction
# covariates_all = paste0(primary, ' * ', procedures, ' + ', paste0(covariates, collapse = ' + '))
# covariates_all



# Model formula -----------------------------------------------------------

create_formulas <- function(
  y = outcomes,
  primary_covariate = primary,
  other_covariates = covariates,
  random_effects = 'id_physician_npi',
  interaction_term = NULL,
  mgcv = FALSE
) {
  covariates_all = paste0(
    primary, 
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
  data,
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


fs = create_formulas(
  y = outcomes,
  primary_covariate = primary,
  other_covariates = covariates,
  interaction_term = NULL,
  random_effects = c('id_physician_npi', 'facility_prvnumgrp')
)

names(fs) = outcomes


# Run models in parallel --------------------------------------------------

library(future)

plan(multiprocess(workers = length(outcomes)))

library(furrr)

# ~ 30 sec with nAGQ 0 and calc.derivs FALSE; over ~24 minutes with both off, ~21 with just derivs =F but may still have convergence issues, though no strong differences than without; also checked with tmb
system.time({
  results <- future_map2(
    fs,
    list(data = main_data),
    run_models,
    proc = 'Partial Colectomy',
    method = 'lme4',
    nAGQ = 0,
    control = lme4::glmerControl(calc.derivs = FALSE)
  )
})

# about 5 min for 5 year, 12 for 10yr
system.time({
  results <- future_map2(
    fs,
    list(data = main_data),
    run_models,
    proc = 'Partial Colectomy',
    method = 'glmmTMB'
  )
})
# 
# results = future_pmap(
#   formulas,
#   data,
#   run_models,
#   method = 'mgcv',
#   discrete = TRUE,
#   nthreads = length(outcomes)
# )

# Run docs ----------------------------------------------------------------

### PDF

rmds = paste0(
  'analysis/report/all_outcomes_misc_procs/partial_colectomy/',
  list.files('analysis/report/all_outcomes_misc_procs/partial_colectomy', pattern = '.Rmd')
)

rmds = rmds[!grepl(rmds, pattern = 'main_doc|synthesis')]


# create docs in parallel
# library(future)
# 
# plan(multiprocess(workers = length(rmds)))
# 
# library(furrr)
# 
# system.time({
#   future_map(rmds, rmarkdown::render)
# })
# 
# plan(sequential)

# latex can't handle parallel operations because it will pull from the same-named images whenever compilation takes place. Serial should have no issue.

if (year_since_grad == 5) {
  map(rmds, rmarkdown::render, output_dir = 'analysis/report/all_outcomes_misc_procs/partial_colectomy/5yr/')
} else {
  map(rmds, rmarkdown::render, output_dir = 'analysis/report/all_outcomes_misc_procs/partial_colectomy/10yr/')
}

# for testing
# rmarkdown::render(input = rmds[3], output_dir = 'analysis/report/all_outcomes_misc_procs/partial_colectomy/5yr/')
