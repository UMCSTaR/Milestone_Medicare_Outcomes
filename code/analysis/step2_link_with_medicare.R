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
# multiple matches
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/milestone_nppes_ama_abs_15_18.rdata")

# load medicare data
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/medicare_gs_by_abs.rdata")

analytic_data = medicare_gs

# match by NPI process -------
## 1. unique match -----
n_distinct(analytic_data$id_physician_npi) # n 33760
n_distinct(milestone_person$npi.linked)    # n 4628

no_npi_milestone = sum(is.na(milestone_person$npi.linked))/nrow(milestone_person)
# 0.01991104

milestone_medicare = analytic_data %>% 
  inner_join(milestone_person, by = c("id_physician_npi" = "npi.linked"))

n_distinct(milestone_medicare$id_physician_npi) # 1083

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
# 9

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

# filter non-fellowship surgeons-----
# 1. fellowship data------
fellow_council = read_csv("~/Box/ACGME Milestones v Outcomes/ACGME Milestones v Outcomes - Data/fellowship_npi.csv") %>% 
  mutate(npi= as.character(npi))

n_distinct(milestone_medicare$id_physician_npi)

milestone_medicare_gs = milestone_medicare %>%
  anti_join(fellow_council, by = c("id_physician_npi" = "npi")) 

n_distinct(milestone_medicare_gs$id_physician_npi) #1082

# 1. NPI degree------
load("/Volumes/George_Surgeon_Projects/Other/NPPES_Data_Dissemination_January_2020/npi_md_single_spty_gs.rdata")

milestone_medicare_gs =  milestone_medicare_gs %>% 
  mutate(nppes_gs = ifelse(id_physician_npi %in% npi_md_single_spty_gs$NPI,
                           "gs","not gs")) 

milestone_medicare_gs = milestone_medicare_gs %>% 
  filter(nppes_gs == "gs") 

n_distinct(milestone_medicare_gs$id_physician_npi) #784

milestone_medicare_gs %>% 
  group_by(facility_clm_yr) %>% 
  mutate(facility_clm_yr = facility_clm_yr +2007) %>% 
  mutate(n_surgeon = n_distinct(id_physician_npi),
         n_cases = n()) %>% 
  distinct(facility_clm_yr, n_surgeon, n_cases)

save(milestone_medicare_gs, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_gs.rdata")


# only keep partial colectomy ------
milestone_medicare_pc = milestone_medicare_gs %>% 
  filter(e_proc_grp_lbl == "Partial Colectomy")

n_distinct(milestone_medicare_pc$id_physician_npi) #581

save(milestone_medicare_pc, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_pc.rdata")


# only keep n>=5 --------

milestone_medicare_pc_5 = milestone_medicare_pc %>% 
  filter(surgeon_volume>=5)

n_distinct(milestone_medicare_pc_5$id_physician_npi)

#429
save(milestone_medicare_pc_5, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_pc_5.rdata")

