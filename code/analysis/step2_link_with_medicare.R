# link Milestone data to medicare
# description:
# - unique matched PersonID and Medicare
# - multiple matched PersonID to Medicare (to get more possible matched)
# - npi from AMA and ABS to match with Medicare

library(tidyverse)
library(tidyext)

# load matched ACGME data
# unique matches
milestone_person = read_csv("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/milestone_linked_npi.csv") %>% 
  mutate_if(is.numeric, as.character)
# multiple macthes
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/milestone_nppes_ama_abs_15_18.rdata")

# load medicare data
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/medicare_gs_by_abs.rdata")
# general surgeon partial colectomy

analytic_data = medicare_gs

# match by NPI process -------
## 1. unique match -----
n_distinct(analytic_data$id_physician_npi) # n 28944
n_distinct(milestone_person$npi.linked)    # n 4628

milestone_medicare = analytic_data %>% 
  inner_join(milestone_person, by = c("id_physician_npi" = "npi.linked"))

n_distinct(milestone_medicare$id_physician_npi) # 475

milestone_medicare %>% 
  cat_by(facility_clm_yr)

milestone_medicare_unique = milestone_medicare %>% 
  distinct(PersonID, id_physician_npi) %>% 
  glimpse()

# 2. multiple matches ----------
# 2.1 multi match by NPEES and can't decide by yog or yob ------
milestone_person_m = milestone_person %>% 
  filter(is.na(npi.linked))

multi = milestone_nppes_ama_abs_15_18 %>% 
  select(PersonID, NPI.ms, NPI.nppes) %>% 
  gather("npi_version", "npi", -PersonID) %>% 
  filter(PersonID %in% milestone_person_m$PersonID) %>% 
  filter(!is.na(npi)) 

# check
n_distinct(multi$PersonID)  # 15

milestone_medicare_muti = analytic_data %>% 
  inner_join(multi, by = c("id_physician_npi" = "npi"))

milestone_match_with_medicare_person = milestone_medicare_muti %>% 
  distinct(PersonID, id_physician_npi)

# PersonID id_physician_npi
# 494347   1760771901      
# 533431   1871693077  



# 2.2 AMA and ABS with no matches, by names ----------
# these people didn't match by NPPES file using names but matched with AMA or ABS,
# use the matched NPI from AMA/ABS to match with Medicare
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/no_match_ama.rdata")
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/no_match_abs.rdata")

# ama
npi_ama = no_match_ama %>% 
  distinct(PersonID, npi) %>% 
  rename(npi.ama = npi)

ama_medicare_person = analytic_data %>% 
  inner_join(npi_ama, by = c("id_physician_npi" = "npi.ama")) %>% 
  distinct(PersonID, id_physician_npi) %>% 
  glimpse()
# 6

# abs 
npi_abs = no_match_abs %>% 
  distinct(PersonID, npi.abs) 

abs_medicare_person = analytic_data %>% 
  inner_join(npi_abs, by = c("id_physician_npi" = "npi.abs")) %>% 
  distinct(PersonID, id_physician_npi) %>% 
  glimpse()
# 3

# 3. combine unique and multiple --------

milestone_medicare_person = rbind(milestone_medicare_unique,
milestone_match_with_medicare_person,
ama_medicare_person,
abs_medicare_person) %>% 
  distinct()

# check dup for personID and NPI
milestone_medicare_person %>% 
  add_count(PersonID) %>% 
  filter(n>1)

milestone_medicare_person %>% 
  add_count(id_physician_npi) %>% 
  filter(n>1)

# select medicare cases
milestone_medicare = analytic_data %>% 
  inner_join(milestone_medicare_person, by = "id_physician_npi")

n_distinct(milestone_medicare$id_physician_npi)


save(milestone_medicare, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare.rdata")

# only keep partial colectomy ------
medicare_pc = milestone_medicare %>% 
  filter(e_proc_grp_lbl == "Partial Colectomy")

n_distinct(medicare_pc$id_physician_npi) #345

save(medicare_pc, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/medicare_pc.rdata")


# only keep n>=5 --------

milestone_medicare_pc_5 = medicare_pc %>% 
  filter(surgeon_volume>=5)

n_distinct(milestone_medicare_pc_5$id_physician_npi)

#208
save(milestone_medicare_pc_5, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_pc_5.rdata")

