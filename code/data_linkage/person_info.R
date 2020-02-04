# This document clean a person file for each NPI
library(tidyverse)
library(lubridate)

# load milestone dt ----
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/data/milestone_01_2020.rdata")

str(milestone)

milestone_person_level =  milestone %>% 
  select(PersonID, NPI, Last4SSN, ssn, BirthDate, Birth_date, DegreeDate, Degree_date, PositionType, ResidentStatus, StartDate, ExpectedCompletionDate, complete_date) %>% 
  distinct()

n_distinct(milestone_person_level$PersonID, na.rm = T)
# 4743 surgeons

# NPI ------
milestone_npi = milestone %>%
  filter(!is.na(PersonID)) %>% 
  distinct(PersonID, NPI) 

# PersonID has multiple NPI
id_npi = milestone_npi %>% 
  filter(!is.na(NPI)) %>% 
  add_count(NPI) %>% 
  filter(n>1) %>% 
  glimpse()

# add vars
milestone_npi = milestone_npi %>% 
  mutate(multi_NPI = ifelse(NPI %in% id_npi$NPI, 1, 0))

# check 
milestone %>% 
  filter(NPI == 1356782205 | NPI == 1336439686) %>% 
  distinct(NPI, PersonID)
  
rm(id_npi)

# SSN ------
milestone_ssn = milestone %>% 
  distinct(PersonID, ssn)

# muli ssn
id_ssn = milestone_ssn %>% 
  add_count(PersonID) %>% 
  filter(n>1) %>% 
  arrange(PersonID)

# add vars
milestone_ssn = milestone_ssn %>% 
  mutate(multi_ssn = ifelse(PersonID %in% id_ssn$PersonID, 1, 0))

# check
milestone %>% 
  filter(PersonID == 739127 | PersonID == 982342) %>% 
  distinct(NPI, PersonID, ssn)

rm(id_ssn)

# Birthdate ----
milestone_birth = milestone %>% 
  distinct(PersonID, Birth_date)

# muli ssn
id_birth = milestone_birth %>% 
  add_count(PersonID) %>% 
  filter(n>1) %>% 
  arrange(PersonID)

id_birth %>% 
  group_by(PersonID) %>% 
  mutate(n = row_number()) %>% 
  filter(n == 1, is.na(Birth_date)) # all birthdate is not missing for the first record

milestone_birthday = milestone_birth %>% 
  filter(!is.na(Birth_date)) %>% 
  distinct(PersonID, Birth_date)

# who don't have birthdat
setdiff(unique(milestone_npi$PersonID), unique(milestone_birthday$PersonID))
# "606844" "800106"

# check
milestone %>%
  filter(PersonID == "606844" | PersonID == "800106") %>%
  distinct(PersonID, BirthDate)








# add vars
milestone_ssn = milestone_ssn %>% 
  mutate(multi_ssn = ifelse(PersonID %in% id_ssn$PersonID, 1, 0))

rm(id_ssn)

qa_person = milestone_person_level %>% 
  add_count(PersonID) %>% 
  filter(n>1) %>% 
  arrange(PersonID)


qa_person_unique =  milestone %>% 
  distinct(PersonID, NPI, ssn, Birth_date, Degree_date) %>% 
  add_count(PersonID) %>% 
  filter(n>1) %>% 
  arrange(PersonID) 

qa_person_unique %>% 
  select(PersonID, ssn, Birth_date, Degree_date)

