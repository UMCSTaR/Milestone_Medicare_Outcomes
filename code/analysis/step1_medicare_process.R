library(tidyverse)
library(tidyext)

# medicare process -------

# medicare data
load("/Volumes/George_Surgeon_Projects/medicare_data/xilin_analytic_file/add_cmb_and_selected_vars/full_analytic_data.rdata")            
source("code/functions/prep_medicare_data.R")

analytic_data = prep_data_for_model(analytic_data)

# filter foreign grads
medicare_us = analytic_data %>% 
  filter(forgein_graduate == 0)

n_distinct(medicare_us$id_physician_npi)

# filter only gs by abs ----
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/abs_fellowship_npi.rdata")
medicare_gs = medicare_us %>% 
  filter(!id_physician_npi %in% abs_fellowship_npi)

medicare_gs %>% 
  cat_by(e_proc_grp_lbl)
# Partial Colectomy     686886       65.2  

# define by abs
save(medicare_gs, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/medicare_gs_by_abs.rdata")


