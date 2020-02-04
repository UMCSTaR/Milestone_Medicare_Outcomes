# this document is initial exploration for the milestone data

library(tidyverse)
library(lubridate)

milestone = read_csv("/Volumes/George_Surgeon_Projects/ACGME_milestone/data/Data-UFA04037 ACGME George Mich DUA PE 4Dec2019_ESHsigned.csv",
                     col_types = cols(.default = "c"))   # read all as charactor

# date
milestone = milestone %>% 
  mutate(Birth_date = dmy(BirthDate),
         Degree_date = dmy(DegreeDate),
         complete_date = mdy(ExpectedCompletionDate))

# ssn
milestone = milestone%>% 
  mutate(ssn = str_extract(Last4SSN, "\\d+")) 

save(milestone, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/data/milestone_01_2020.rdata")

