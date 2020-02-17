library(tidyverse)
library(tidyext)

# load matched ACGME data
# unique matches
milestone_person = read_csv("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/milestone_linked_npi.csv") %>% 
  mutate_if(is.numeric, as.character)
# multiple macthes
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/milestone_nppes_ama_abs_15_18.rdata")

# medicare data
# load("/Volumes/George_Surgeon_Projects/medicare_data/xilin_analytic_file/add_cmb_and_selected_vars/full_analytic_data.rdata")            
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/medicare_gs_pc.rdata")

analytic_data = medicare_gs_pc


# match by NPI process -------
## 1. unique match -----
n_distinct(analytic_data$id_physician_npi) # n 37988   #gs pc 6692
n_distinct(milestone_person$npi.linked)    # n 4628

milestone_medicare = analytic_data %>% 
  inner_join(milestone_person, by = c("id_physician_npi" = "npi.linked"))

n_distinct(milestone_medicare$id_physician_npi) # 676  # gs pc 19

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
n_distinct(milestone_multi_npi$PersonID) # 4642

milestone_medicare_muti = analytic_data %>% 
  inner_join(multi, by = c("id_physician_npi" = "npi"))

milestone_match_with_medicare_person = milestone_medicare_muti %>% 
  distinct(PersonID, id_physician_npi)

# PersonID id_physician_npi
# 494347   1760771901      
# 533431   1871693077  

milestone_medicare_muti %>% 
  count(facility_clm_yr)


# 2.2 AMA and ABS with no matches, by names ----------
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
# 7

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

