library(tidyverse)

# load original milestone data from ACGME----
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_02_2020.rdata")

# What perc of PersonID missing NPI
milestone_person = milestone %>% 
  select(PersonID, NPI, Birth_date, Degree_date, birth_year) %>% 
  distinct() %>% 
  glimpse()

milestone_person %>% 
  summarise(n_surg = n(), 
            n_NPI = sum(!is.na(NPI)),
            perc_npi = n_NPI/n_surg*100)

anyDuplicated(milestone_person$PersonID)


# NPPES to get names for Milestone ------
load("/Volumes/George_Surgeon_Projects/Other/NPPES_Data_Dissemination_January_2020/npidata_pfile_2020_selected_var.rdata")

npidata_pfile_2020_selected_var = npidata_pfile_2020_selected_var %>% 
  mutate(NPI = as.character(NPI))

milestone_person_nppes = milestone_person %>% 
  left_join(npidata_pfile_2020_selected_var)

milestone_person_nppes %>% 
  filter(is.na(first_name), !is.na(NPI)) %>% 
  glimpse()

save(milestone_person_nppes, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/milestone_person_nppes.rdata")

# check if anyone is not in the NPPES group
milestone_person_nppes %>% 
  filter(is.na(`Entity Type Code`), !is.na(NPI)) %>% 
  glimpse()

# 2 people don't have names, all matched


