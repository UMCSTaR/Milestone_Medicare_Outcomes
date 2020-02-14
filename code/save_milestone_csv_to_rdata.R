# this document is initial exploration for the milestone data

library(tidyverse)
library(lubridate)

# this take 2 min
milestone = read_csv("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/ACGME Milestones Data Update 19-UFA04037 2020.02.05.csv",
                     col_types = cols(.default = "c"))   # read all as charactor

# 2016 to 2018
milestone_16_18 = readxl::read_xlsx("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/GRADUATES_SURGERY_NAMES_2016_17_18_grads.xlsx")   
# 2015
milestone_15 = read_csv("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/UMICH_GS_2015_GRADS.csv")
milestone_15_person = readxl::read_xlsx("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/SurgeryGrads_2015_graduates.xlsx")

milestone_15 = milestone_15 %>% 
  mutate(SpecialtyCode = as.character(SpecialtyCode),
         NationalProviderID = as.character(NationalProviderID))

n_distinct(milestone_15$PersonID)  # 1170
n_distinct(milestone_15_person$PersonID) #1142


milestone_15_person %>% 
  select(PersonID) %>% 
  anti_join(milestone_15)
# 857612

milestone_15_all = milestone_15 %>% 
  inner_join(milestone_15_person)

n_distinct(milestone_15_all$PersonID)

names(milestone_15_all)

# date
milestone_15_all = milestone_15_all %>% 
  mutate(Birth_date = dmy(BirthDate),
         Degree_date = dmy(DegreeDate),
         complete_date = mdy(ExpectedCompletionDate),
         birth_year = lubridate::year(Birth_date))

save(milestone, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_02_2020.rdata")

# 2015
save(milestone_15_all, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_2015.rdata")
