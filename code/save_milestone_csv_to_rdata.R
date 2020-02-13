# this document is initial exploration for the milestone data

library(tidyverse)
library(lubridate)

# this take 2 min
milestone = read_csv("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/ACGME Milestones Data Update 19-UFA04037 2020.02.05.csv",
                     col_types = cols(.default = "c"))   # read all as charactor

# 2016 to 2018
milestone_16_18 = readxl::read_xlsx("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/GRADUATES_SURGERY_NAMES_2016_17_18_grads.xlsx")   
# 2015
milestone_15 = readxl::read_xlsx("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/SurgeryGrads_2015_graduates.xlsx")


# date
milestone = milestone %>% 
  mutate(Birth_date = dmy(BirthDate),
         Degree_date = dmy(DegreeDate),
         complete_date = mdy(ExpectedCompletionDate),
         birth_year = lubridate::year(Birth_date))

save(milestone, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_02_2020.rdata")

