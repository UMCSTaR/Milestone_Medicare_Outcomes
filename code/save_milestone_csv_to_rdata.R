# this document is initial exploration for the milestone data

library(tidyverse)
library(lubridate)

milestone = read_csv("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/ACGME Milestones Data Update 19-UFA04037 2020.02.05.csv",
                     col_types = cols(.default = "c"))   # read all as charactor

# date
milestone = milestone %>% 
  mutate(Birth_year = year(dmy(BirthDate)),
         Degree_year = year(dmy(DegreeDate)),
         complete_year = year(mdy(ExpectedCompletionDate)))

save(milestone, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_02_2020.rdata")

