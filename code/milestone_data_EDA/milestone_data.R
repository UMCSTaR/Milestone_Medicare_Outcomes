library(tidyverse)

load("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_2015.rdata")

glimpse(milestone_15_all)

milestone_15_all %>% 
  distinct(AcademicYear, PersonID) %>% 
  count(AcademicYear)

load("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_02_2020.rdata")

glimpse(milestone)

milestone %>% 
  distinct(AcademicYear, PersonID) %>% 
  count(PersonID) %>% 
  count(n)

milestone_15_all %>% 
  distinct(PersonID) %>% 
  mutate(PersonID = as.character(PersonID)) %>% 
  inner_join(milestone)
