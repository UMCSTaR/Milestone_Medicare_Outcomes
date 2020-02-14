library(tidyverse)

milestone_linked = read_csv("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/milestone_linked_15_19.csv")

milestone_linked_npi =  milestone_linked %>% 
  distinct(PersonID, NPI, NPI.nppes) %>% 
  rename(NPI.milestone = NPI) %>% 
  mutate(npi.linked = ifelse(!is.na(NPI.nppes), NPI.nppes, NPI.milestone))

write_csv(milestone_linked_npi, path = "/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/milestone_linked_npi.csv")


milestone_linked_npi %>% 
  filter(is.na(npi.linked))
