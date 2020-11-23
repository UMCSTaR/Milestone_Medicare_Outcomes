# combine evaluation data to milestone persons info
# details: we get milestone evaluation and person info in two files. so we need to merge them
# processing
# - only include PGY5  valuations
# - some checks to evaluate what other year milestone data we have

library(tidyverse)
library(tidyext)

# load data -----
# evaluations
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_2015.rdata")
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_02_2020.rdata")
# people from 
milestone_person = read_csv("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/milestone_linked_npi.csv") %>% 
  mutate_if(is.numeric, as.character)

milestone_678 = milestone %>% 
  select(PersonID, ScheduleWindowDescription, AcademicYear, SurveyName, SponsorID, residentyear, QuestionID:ResponseText) %>% 
  rename(eval_peroid = ScheduleWindowDescription) %>% 
  glimpse()

# check
milestone_678 %>% 
  cat_by(AcademicYear, residentyear) %>% 
  arrange(AcademicYear, residentyear)

p_2014 = milestone_678 %>% 
  filter(AcademicYear == 2014) %>% 
  distinct(PersonID, AcademicYear) 

p_2015 = milestone_678 %>% 
  filter(AcademicYear == 2015) %>% 
  distinct(PersonID, AcademicYear)

p_2014_2015 = p_2014 %>% 
  full_join(p_2015, by = "PersonID") 


# combine all year milestone ------
milestone_5 = milestone_15_all %>% 
  select(PersonID, ScheduleWindowDescription, AcademicYear, SurveyName, SponsorID, residentyear, QuestionID:ResponseText) %>% 
  rename(eval_peroid = ScheduleWindowDescription) %>% 
  glimpse()

# check
milestone_5 %>% 
  cat_by(residentyear)


milestone5678 = rbind(milestone_678, milestone_5)

academic_year_resident_year = milestone %>% 
  count(AcademicYear, residentyear) 

write_csv(academic_year_resident_year, path = "data/academic_year_resident_year.csv")

milestone = rbind(milestone_678, milestone_5) %>% 
  filter(PersonID %in% milestone_person$PersonID)

n_distinct(milestone5678$PersonID)
n_distinct(milestone$PersonID)

# check
milestone %>% 
  filter(residentyear == 5) %>% 
  cat_by(AcademicYear)

n_distinct(milestone$PersonID)  #4720


# Final year milestone -------
milestone_final_year = milestone %>% 
  filter(residentyear == 5)

# PGY 4 milestone rating
milestone_pgy4_year = milestone %>% 
  filter(residentyear == 4)

# check
n_distinct(milestone_final_year$PersonID)  # 4718
n_distinct(milestone_pgy4_year$PersonID)  # 3553

# any duplicate
milestone_final_year %>% 
  distinct(PersonID, residentyear) %>% 
  add_count(PersonID) %>% 
  filter(n>1)

milestone_pgy4_year %>% 
  distinct(PersonID, residentyear) %>% 
  add_count(PersonID) %>% 
  filter(n>1)

# PHY5 evaluations
save(milestone_final_year, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_final_year.rdata")
# PGY4 evaluations
save(milestone_pgy4_year, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/pgy4_ratings/milestone_pgy4_year.rdata")

