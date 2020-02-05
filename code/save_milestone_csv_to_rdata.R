# this document is initial exploration for the milestone data

library(tidyverse)
library(lubridate)

milestone = read_csv("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/ACGME Milestones Data Update 19-UFA04037 2020.02.05.csv",
                     col_types = cols(.default = "c"))   # read all as charactor

# date
milestone = milestone %>% 
  mutate(Birth_date = dmy(BirthDate),
         Degree_date = dmy(DegreeDate),
         complete_date = mdy(ExpectedCompletionDate))

# ssn
milestone = milestone%>% 
  mutate(ssn = str_extract(Last4SSN, "\\d+")) 

save(milestone, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_02_2020.rdata")

