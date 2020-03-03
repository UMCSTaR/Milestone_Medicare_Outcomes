run_models <- function(
  formula = NULL,
  data = main_data,
  method = 'lme4',
  proc = NULL,
  ...
) {
  
  if (!is.null(proc)) data = dplyr::filter(data, e_proc_grp_lbl %in% proc)
  
  if (method == 'lme4') {
    
    f = as.formula(as.character(formula))
    
    model = lme4::glmer(
      formula = f,
      data = data,
      family = 'binomial',
      ...
    )
  }
  
  if (method == 'glmmTMB') {
    
    f = as.formula(as.character(formula))
    
    model = glmmTMB::glmmTMB(
      formula = f,
      data = data,
      family = 'binomial',
      ...
    )
  }
  
  if (method == 'mgcv') {
    
    f = as.formula(as.character(formula))
    
    model = mgcv::bam(
      formula = f,
      data = data,
      family = 'binomial',
      ...
    )
  }
  
  if (method == 'brms') {
    
    f = as.formula(as.character(formula))
    
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
