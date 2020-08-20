library(tidyverse)
library(tidyext)

# medicare process -------
# Description: 
# - load raw medicare analytic dataset 2007-2017
# - add new variables 
# - exclude foreign graduate and sub-specialty defined by ABS

# medicare data
analytic_data = data.table::fread("/Volumes/George_Surgeon_Projects/standardized_medicare_data_using_R/analytic/full_data/analytic_selected_vars.csv")            
source("code/functions/prep_medicare_data.R")

analytic_data = prep_data_for_model(analytic_data)

# filter foreign grads
medicare_us = analytic_data %>% 
  filter(forgein_graduate == 0)

n_distinct(medicare_us$id_physician_npi)

# filter only gs by abs ----
medicare_gs = medicare_us %>% 
  filter(fellowship_abs == 0)

n_distinct(medicare_gs$id_physician_npi)

medicare_gs %>% 
  cat_by(e_proc_grp_lbl)

# define by abs
save(medicare_gs, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/medicare_gs_by_abs.rdata")


