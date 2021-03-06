# This document clean a person file for each NPI
library(tidyverse)
library(lubridate)

# 2015 data -----
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_2015.rdata")

names(milestone_15_all)

milestone_15_person =  milestone_15_all %>% 
  select(PersonID, NationalProviderID, FirstName, MiddleName, LastName, Birth_date, Degree_date, birth_year) %>% 
  distinct()

save(milestone_15_person, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/milestone_15_person.rdata")

# load milestone dt ----
# change here for new data from ACGME
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_02_2020.rdata")

names(milestone)

str(milestone)

n_distinct(milestone$PersonID)

# select personal information
milestone_person_level =  milestone %>% 
  select(PersonID, NPI,Birth_date, Degree_date, birth_year) %>% 
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

rm(id_birth)


# DegreeDate -------
milestone_degree_dt = milestone %>% 
  filter(!is.na(Degree_date)) %>% 
  distinct(PersonID, Degree_date)

# second date is na
milestone_degree_dt %>% 
  add_count(PersonID)  %>% 
  filter(n>1) %>% 
  arrange(PersonID)


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


# combine NPI, Bithdate, Degreedate--------
milestone_person_info = milestone_npi %>% 
  left_join(milestone_birth)  %>% 
  left_join(milestone_degree_dt)

save(milestone_person_info, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/milestone_person_info.rdata")


# 2016 to 2018 with names ------
# add person info
milestone_16_18 = readxl::read_xlsx("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/GRADUATES_SURGERY_NAMES_2016_17_18_grads.xlsx")   
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/milestone_person_info.rdata")

milestone_16_18_person = milestone_16_18 %>% 
  mutate(PersonID = as.character(PersonID)) %>% 
  left_join(milestone_person_info, by = "PersonID") %>% 
  glimpse()

# person ADS but not milestone
milestone_16_18 %>%   # 3581 grads
  transmute(PersonID = as.character(PersonID)) %>% 
  anti_join(milestone_person_info)  # 4743 grads from ACGME with milestone records
  

# check 
milestone_16_18_person %>% 
  filter(NationalProviderID != NPI)
# 0

milestone_16_18 %>% 
  add_count(PersonID) %>% 
  filter(n>1)
# 1 personID dup

save(milestone_16_18_person, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/milestone_16_18_person.rdata")

# 2015 --------
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_2015.rdata")
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/milestone_16_18_person.rdata")

vars = names(milestone_16_18_person)
names(milestone_15_all)

milestone_15_all = milestone_15_all %>% 
  mutate(NPI = NA,
         multi_NPI = NA) 

milestone_15_person = milestone_15_all %>% 
  select(vars) %>% 
  distinct()

milestone_15_18_person = rbind(milestone_15_person, milestone_16_18_person)


# 4721 = 3581 (2016-2019) (one personID dup) + 1141 (2015)
save(milestone_15_18_person, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/milestone_15_18_person.rdata")

n_distinct(milestone_15_18_person$PersonID)
#4721
