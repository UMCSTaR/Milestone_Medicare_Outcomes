library(tidyverse)

# load data ------
# mdedicare dt
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_pc.rdata")
# milestone data
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_final_year.rdata")


# select milstone records for medicare physician -------
milestone_person_in_medicare = milestone_final_year %>% 
  filter(PersonID %in% milestone_medicare_pc$PersonID)

milestone_end_person_in_medicare  = milestone_person_in_medicare %>% 
  filter(grepl("Year-End",eval_peroid))

# check
n_distinct(milestone_person_in_medicare$PersonID)  #345
n_distinct(milestone_end_person_in_medicare$PersonID)  #338

no_end_eval_person = milestone_person_in_medicare %>% 
  filter(!PersonID %in% milestone_end_person_in_medicare$PersonID) %>% 
  distinct(PersonID) %>% 
  pull()
# 7 people don't have year-end eval, only mid-year eval


range(milestone_end_person_in_medicare$IntResponseValue)

# mean milestone ratings --------
person_values = milestone_end_person_in_medicare %>% 
  group_by(PersonID) %>% 
  mutate(IntResponseValue = as.numeric(IntResponseValue),
  IntResponseValue_mean = mean(IntResponseValue, na.rm = T)) %>% 
  distinct(PersonID, SponsorID, IntResponseValue_mean) %>% 
  ungroup() %>% 
  mutate(IntResponseValue_mean_scale = as.numeric(scale(IntResponseValue_mean))) %>% 
  glimpse()

save(person_values, file = "data/person_in_medicare_and_milestone.rdata")
  
ggplot(data = person_values) +
  geom_histogram(aes(x = IntResponseValue_mean_scale)) +
  theme_classic() +
  ggtitle("Mean Eval Rating for End-year eval among surgeons in Medicare")

# mean operative milestone ratings (PC3, MK2, ICS3)------
milestone_end_person_in_medicare %>% 
  count(QuestionKey)

operative_rating = milestone_end_person_in_medicare %>% 
  filter(QuestionKey %in% c("comp1_PC_Q3", "comp2_MK_Q2", "comp6_ICS_Q3")) %>% 
  group_by(PersonID) %>% 
  mutate(operative_rating_mean = mean(as.numeric(IntResponseValue), na.rm = T)) %>% 
  distinct(PersonID, operative_rating_mean) %>% 
  ungroup() %>% 
  glimpse()

# mean professionalism
prof_rating = milestone_end_person_in_medicare %>% 
  filter(ReportCategory == "Professionalism") %>% 
  group_by(PersonID) %>% 
  mutate(prof_rating_mean = mean(as.numeric(IntResponseValue), na.rm = T)) %>% 
  distinct(PersonID, prof_rating_mean) %>% 
  ungroup() %>% 
  glimpse()

# person value all
person_values = person_values %>% 
  left_join(prof_rating) %>% 
  left_join(operative_rating)

# attach score to the medicare -----
# (inner join loose 7 people who don't have end-year eval)
milestone_medicare_pc =  milestone_medicare_pc %>% 
  inner_join(person_values, by = "PersonID")

n_distinct(milestone_medicare_pc$PersonID) #338


save(milestone_medicare_pc, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_pc.rdata")


