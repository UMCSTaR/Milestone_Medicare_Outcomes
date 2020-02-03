# this document is initial exploration for the milestone data

library(tidyverse)
library(lubridate)

milestone = read_csv("/Volumes/George_Surgeon_Projects/ACGME_milestone/data/Data-UFA04037 ACGME George Mich DUA PE 4Dec2019_ESHsigned.csv",
                     col_types = cols(.default = "c"))   # read all as charactor

# date
milestone = milestone %>% 
  mutate(Birth_year = year(dmy(BirthDate)),
         Degree_year = year(dmy(DegreeDate)),
         complete_year = year(mdy(ExpectedCompletionDate)))

save(milestone, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/data/milestone_01_2020.rdata")

