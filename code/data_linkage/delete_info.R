library(tidyverse)

load("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_2015.rdata")

milestone_15_all = milestone_15_all %>% 
  select(-FirstName, -LastName, -MiddleName)

save(milestone_15_all, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_2015.rdata" )


load("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/milestone_15_18_person.rdata")

milestone_15_18_person = milestone_15_18_person %>% 
  select(-FirstName, -LastName, -MiddleName)

save(milestone_15_18_person, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/milestone_15_18_person.rdata")
  
