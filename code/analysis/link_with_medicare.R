library(tidyverse)
library(tidyext)

# load matched ACGME data
# unique matches
milestone_person = read_csv("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/milestone_linked_npi.csv") %>% 
  mutate_if(is.numeric, as.character)
# multiple macthes
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/milestone_nppes_ama_abs_15_18.rdata")

# medicare data
load("/Volumes/George_Surgeon_Projects/medicare_data/xilin_analytic_file/add_cmb_and_selected_vars/full_analytic_data.rdata")            


# match by NPI process -------
## unique match -----
n_distinct(analytic_data$id_physician_npi) # n 37988
n_distinct(milestone_person$npi.linked)    # n 4628

milestone_medicare = analytic_data %>% 
  inner_join(milestone_person, by = c("id_physician_npi" = "npi.linked"))

n_distinct(milestone_medicare$id_physician_npi) # 676

milestone_medicare %>% 
  cat_by(facility_clm_yr)

# multiple matches ----------
milestone_person_m = milestone_person %>% 
  filter(is.na(npi.linked))

multi = milestone_nppes_ama_abs_15_18 %>% 
  select(PersonID, NPI.ms, NPI.nppes) %>% 
  gather("npi_version", "npi", -PersonID) %>% 
  filter(PersonID %in% milestone_person_m$PersonID)

multi = multi %>% 
  filter(!is.na(npi)) 

n_distinct(multi$PersonID)  # 15

n_distinct(milestone_multi_npi$PersonID) # 4642

milestone_medicare_muti = analytic_data %>% 
  inner_join(multi, by = c("id_physician_npi" = "npi"))

milestone_medicare_muti %>% 
  distinct(PersonID, id_physician_npi)

# PersonID id_physician_npi
# 494347   1760771901      
# 533431   1871693077  

milestone_medicare_muti %>% 
  count(facility_clm_yr)

# no need to do the multiple match 15 people, since only 2 got matched with 15 cases
# all the way back to 2007

# AMA and ABS with no matches
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/no_match_ama.rdata")
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/no_match_abs.rdata")
