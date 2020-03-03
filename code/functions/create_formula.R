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
