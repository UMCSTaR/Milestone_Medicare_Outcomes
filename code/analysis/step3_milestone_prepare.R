library(tidyverse)

# load data ------
# mdedicare dt
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare.rdata")

# milestone data
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_final_year.rdata")


# select milstone records for medicare physician -------
milestone_person_in_medicare = milestone_final_year %>% 
  filter(PersonID %in% milestone_medicare$PersonID)

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
  glimpse()

ggplot(data = person_values) +
  geom_histogram(aes(x = IntResponseValue_mean)) +
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

# mean professionalism------
prof_rating = milestone_end_person_in_medicare %>% 
  filter(ReportCategory == "Professionalism") %>% 
  group_by(PersonID) %>% 
  mutate(prof_rating_mean = mean(as.numeric(IntResponseValue), na.rm = T)) %>% 
  distinct(PersonID, prof_rating_mean) %>% 
  ungroup() %>% 
  glimpse()

# ever has low rating ------
low_rating_person = milestone_end_person_in_medicare %>% 
  mutate(ever_less_7_rating = as.numeric(IntResponseValue)<7) %>% 
  filter(ever_less_7_rating == TRUE) %>% 
  distinct(PersonID, ever_less_7_rating) %>% 
  pull(PersonID)

low_rating = milestone_end_person_in_medicare %>% 
  distinct(PersonID) %>% 
  mutate(ever_less_7_rating = ifelse(PersonID %in% low_rating_person, 1, 0))


# person value all
person_values = person_values %>% 
  left_join(prof_rating) %>% 
  left_join(operative_rating) %>% 
  left_join(low_rating)

# attach score to the medicare -----
# (inner join loose 7 people who don't have end-year eval)
milestone_medicare_ratings =  milestone_medicare %>% 
  inner_join(person_values, by = "PersonID")

n_distinct(milestone_medicare_ratings$PersonID) #338


# filter medicare cases after graduation -----------
gradaution_year = milestone_final_year %>% 
  distinct(PersonID, AcademicYear) %>% 
  group_by(PersonID) %>% 
  top_n(1, AcademicYear) %>% 
  ungroup() %>% 
  mutate(grad_year = as.numeric(AcademicYear) +1) %>% 
  select(- AcademicYear)


milestone_medicare_ratings = milestone_medicare_ratings %>% 
  left_join(gradaution_year) %>% 
  filter(grad_year<= (facility_clm_yr + 2007))  # facility_clm_yr was standardized to 0


save(milestone_medicare_ratings, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_ratings.rdata")


