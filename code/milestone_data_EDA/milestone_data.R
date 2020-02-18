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
  cat_by(eval_peroid)

# combine all year milestone ------
milestone_5 = milestone_15_all %>% 
  select(PersonID, ScheduleWindowDescription, AcademicYear, SurveyName, SponsorID, residentyear, QuestionID:ResponseText) %>% 
  rename(eval_peroid = ScheduleWindowDescription) %>% 
  glimpse()

milestone = rbind(milestone_678, milestone_5) %>% 
  filter(PersonID %in% milestone_person$PersonID)

n_distinct(milestone$PersonID)  #4720


# Final year milestone -------
milestone_final_year = milestone %>% 
  filter(residentyear == 5)

# check
n_distinct(milestone_final_year$PersonID)  # 4718


save(milestone_final_year, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_final_year.rdata")

