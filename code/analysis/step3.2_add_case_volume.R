# add case volumes per year for trainees
# note since trainees graduate in July, so at the graduation year, the actual practicing time is 5 months.
# yearly volume was calculated as monthly volume*12

library(tidyverse)

load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_pc_primary.rdata")

# add volume variables
milestone_medicare_pc_primary = milestone_medicare_pc_primary %>% 
  mutate(month_after_graduation = case_when(grad_year == 2015 ~ 30,
                                            grad_year == 2016 ~ 17,
                                            grad_year == 2017 ~ 5)) %>% 
  add_count(id_physician_npi, name = "n_cases_tot") %>% 
  mutate(cases_per_12month = (n_cases_tot/month_after_graduation)*12) 


save(milestone_medicare_pc_primary, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_pc_primary.rdata")


# not cohort related; just checking ---------------------------------------
milestone_medicare_pc_primary %>% 
  group_by(grad_year) %>% 
  summarise(mean_case_by_grad_year = mean(n_cases_tot),
            cases_per_year = mean(cases_per_12month)) 
  
