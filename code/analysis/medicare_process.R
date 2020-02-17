library(tidyverse)
library(tidyext)


# medicare process -------

# medicare data
load("/Volumes/George_Surgeon_Projects/medicare_data/xilin_analytic_file/add_cmb_and_selected_vars/full_analytic_data.rdata")            
source("code/functions/prep_medicare_data.R")

analytic_data = prep_data_for_model(analytic_data)

# filter only gs ----
medicare_gs = analytic_data %>% 
  filter(gs_perform_4 == 1)

medicare_gs %>% 
  cat_by(e_proc_grp_lbl)

# Partial Colectomy     n365324       %60.2 

# filter Partial Colectomy ----
medicare_gs_pc = medicare_gs %>% 
  filter(e_proc_grp_lbl == "Partial Colectomy")

save(medicare_gs_pc, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/medicare_gs_pc.rdata")


