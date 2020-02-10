# This document clean a person file for each NPI
library(tidyverse)
library(lubridate)

# load milestone dt ----
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_02_2020.rdata")

str(milestone)

milestone_person_level =  milestone %>% 
  select(PersonID, NPI, Last4SSN, BirthDate, Birth_date, DegreeDate, Degree_date, PositionType, ResidentStatus, StartDate, ExpectedCompletionDate, complete_date) %>% 
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
  filter(!is.na(PersonID)) %>% 
  distinct(PersonID, Last4SSN)

# muli ssn
id_ssn = milestone_ssn %>% 
  add_count(PersonID) %>% 
  filter(n>1) %>% 
  arrange(PersonID) %>% 
  glimpse()

# add vars
# milestone_ssn = milestone_ssn %>% 
#   mutate(multi_ssn = ifelse(PersonID %in% id_ssn$PersonID, 1, 0))


rm(id_ssn)

# Birthdate ----
milestone_birth = milestone %>% 
  filter(!is.na(PersonID)) %>% 
  distinct(PersonID, Birth_date)

# muli ssn
id_birth = milestone_birth %>% 
  add_count(PersonID) %>% 
  filter(n>1) %>% 
  arrange(PersonID) %>% 
  glimpse()

id_birth %>% 
  group_by(PersonID) %>% 
  mutate(n = row_number()) %>% 
  filter(n == 1, is.na(Birth_date)) # all birthdate is not missing for the first record

# check
milestone %>%
  filter(PersonID == "606844" | PersonID == "800106") %>%
  distinct(PersonID, BirthDate)

rm(milestone_birth)


# DegreeDate -------
milestone_degree_dt = milestone %>% 
  distinct(PersonID, Degree_date)

# second date is na
qa = milestone_degree_dt %>% 
  add_count(PersonID)  %>% 
  filter(n>1) %>% 
  arrange(PersonID)
  
qa %>% 
  group_by(PersonID) %>% 
  mutate(n = row_number()) %>% 
  filter(n == 1 , is.na(Degree_date))

rm(qa)

# drop na date
milestone_degree_dt = milestone_degree_dt %>% 
  filter(!is.na(Degree_date))  # n = 4,741

# who don't have degree_date
setdiff(unique(milestone_npi$PersonID), unique(milestone_degree_dt$PersonID))
# [1] "606844" "800106"

# check
milestone %>% 
  filter(PersonID == 606844 | PersonID == 800106) %>% 
  distinct(NPI, PersonID, DegreeDate)


# start date-----
milestone_start_date = milestone %>%
  filter(!is.na(PersonID)) %>% 
  distinct(PersonID, StartDate) 

# not unique
milestone_start_date %>% 
  add_count(PersonID) %>% 
  filter(n>1) %>% 
  arrange(PersonID)

rm(milestone_start_date)


# combine NPI, SSN, Bithdate, Degreedate--------
milestone_person_info = milestone_npi %>% 
  left_join(milestone_birthday)  %>% # birthdAT
  left_join(milestone_ssn) %>% 
  left_join(milestone_degree_dt)

save(milestone_person_info, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/milestone_person_info.rdata")
