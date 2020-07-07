# Link milestone ratings (which are our main predictors) to medicare cases
# - attach year-end eval from the last year to medicare
# - create overall mean, professional eval mean and operative evals mean, >7 rating binary
# - filter case by graduating year (only count case after graduation)

library(tidyverse)

# load data ------
# medicare gs
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_gs.rdata")

# milestone data
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_final_year.rdata")

n_distinct(milestone_medicare_gs$id_physician_npi) #779

# select last milestone records for medicare physician -------
milestone_person_in_medicare = milestone_final_year %>% 
  filter(PersonID %in% milestone_medicare_gs$PersonID)

milestone_end_person_in_medicare  = milestone_person_in_medicare %>% 
  filter(grepl("Year-End",eval_peroid))

# check
n_distinct(milestone_person_in_medicare$PersonID)  #779
n_distinct(milestone_end_person_in_medicare$PersonID)  #769

no_end_eval_person = milestone_person_in_medicare %>% 
  filter(!PersonID %in% milestone_end_person_in_medicare$PersonID) %>% 
  distinct(PersonID) %>% 
  pull()

length(no_end_eval_person)
# 10 people don't have year-end eval, only mid-year eval

range(milestone_end_person_in_medicare$IntResponseValue)
# 0-9

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

operative_rating = operative_rating %>% 
  mutate(operative_rating_lt7 = ifelse(operative_rating_mean>=7, 1,0))


# mean professionalism------
prof_rating = milestone_end_person_in_medicare %>% 
  filter(ReportCategory == "Professionalism") %>% 
  group_by(PersonID) %>% 
  mutate(prof_rating_mean = mean(as.numeric(IntResponseValue), na.rm = T)) %>% 
  distinct(PersonID, prof_rating_mean) %>% 
  ungroup() %>% 
  glimpse()

prof_rating = prof_rating %>% 
  mutate(prof_rating_lt7 = ifelse(prof_rating_mean>=7, 1,0))


# leadership --------
leadership_rating = milestone_end_person_in_medicare %>% 
  filter(QuestionKey %in% c("comp5_PR_Q1", "comp6_ICS_Q1", "comp6_ICS_Q2", "comp3_SBP_Q1")) %>% 
  group_by(PersonID) %>% 
  mutate(leadership_rating_mean = mean(as.numeric(IntResponseValue), na.rm = T)) %>% 
  distinct(PersonID, leadership_rating_mean) %>% 
  ungroup() %>% 
  glimpse()

leadership_rating = leadership_rating %>% 
  mutate(leadership_rating_lt7 = ifelse(leadership_rating_mean>=7, 1,0))


range(leadership_rating$leadership_rating_mean)

# mean rating less than 7 ------
person_less_than_7 = person_values %>% 
  filter(IntResponseValue_mean<7) %>% 
  pull(PersonID)

mean_lt_7 = milestone_end_person_in_medicare %>% 
  distinct(PersonID) %>% 
  mutate(mean_lt_7 = ifelse(PersonID %in% person_less_than_7, 1, 0))


# ever has low rating ------
low_rating_person = milestone_end_person_in_medicare %>% 
  mutate(ever_less_7_rating = as.numeric(IntResponseValue)<7) %>% 
  filter(ever_less_7_rating == TRUE) %>% 
  distinct(PersonID, ever_less_7_rating) %>% 
  pull(PersonID)

low_rating = milestone_end_person_in_medicare %>% 
  distinct(PersonID) %>% 
  mutate(ever_less_7_rating = ifelse(PersonID %in% low_rating_person, 1, 0))


# person value all -----
person_values = person_values %>% 
  left_join(prof_rating) %>% 
  left_join(operative_rating) %>% 
  left_join(low_rating) %>% 
  left_join(mean_lt_7) %>% 
  left_join(leadership_rating)

# attach score to the medicare -----
# (inner join loose 7 people who don't have end-year eval)
milestone_medicare_ratings =  milestone_medicare_gs %>% 
  inner_join(person_values, by = "PersonID")

n_distinct(milestone_medicare_ratings$PersonID) #769


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

n_distinct(milestone_medicare_ratings$PersonID) #768

save(milestone_medicare_ratings, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_ratings.rdata")

