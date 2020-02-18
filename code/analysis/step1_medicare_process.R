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

save(medicare_us, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/medicare_us.rdata")

# filter only gs by procedure ----
medicare_gs = medicare_us %>% 
  filter(gs_perform_4 == 1)

medicare_gs %>% 
  cat_by(e_proc_grp_lbl)
# Partial Colectomy     320386        60.7 

# filter only gs by abs ----
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/abs_fellowship_npi.rdata")
medicare_gs = medicare_us %>% 
  filter(!id_physician_npi %in% abs_fellowship_npi)

medicare_gs %>% 
  cat_by(e_proc_grp_lbl)
# Partial Colectomy     712953       65.1  


# filter Partial Colectomy ----
medicare_gs_pc = medicare_gs %>% 
  filter(e_proc_grp_lbl == "Partial Colectomy")

# define by procedure 
save(medicare_gs, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/medicare_gs.rdata")
# define by abs
save(medicare_gs, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/medicare_gs_by_abs.rdata")
# pc procedure
save(medicare_gs_pc, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/medicare_gs_pc.rdata")
# pc procedure by ABS
save(medicare_gs_pc, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/medicare_gs_pc_abs.rdata")


