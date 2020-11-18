# QA data for ACGME
# refer to doc "misc_docs/Kenji_ACGME Summary Milestones vs Early Career Outcomes.docx"

library(tidyverse)

# load data-----
# all last year milestone evals
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_final_year.rdata")

# 14-17 all milestone grads
all_grads = read_csv("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/all_grads_15_17.csv",
                     col_types = cols(.default = "c"))
# final cohort 
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_ratings.rdata")

# Compare ---
all_grads_eval = milestone_final_year %>% 
  inner_join(all_grads, by = c("PersonID", "residentyear")) 


# check
if(n_distinct(all_grads_eval$PersonID) == nrow(all_grads)) {
  message("all grads have evals ", emo::ji("check"))
}

all_grads_eval_end = all_grads_eval %>%
  filter(grepl("Year-End",eval_peroid))

n_distinct(all_grads_eval_end$PersonID) # 3498
n_distinct(all_grads_eval$PersonID) # 3521
# not all pgy5 trainees have "Year-End"




# Q1: 3001 (=3521-520) and 520 residents difference -----
milestone_medicare_pc = milestone_medicare_ratings %>% 
  mutate(hosp_beds = case_when(hosp_beds_2grp == 0 ~ "<350",
                               hosp_beds_2grp == 1 ~ ">=350")) %>% 
  filter(!is.na(IntResponseValue_mean),
         month <= 24)

n_distinct(milestone_medicare_pc$PersonID) #520


# get evals
final_cohort_ratings = milestone_final_year %>% 
  filter(PersonID %in% milestone_medicare_pc$PersonID,
         grepl("Year-End",eval_peroid)) 

n_distinct(final_cohort_ratings$PersonID) # 520

final_cohort_ratings %>% count(eval_peroid)

# final_cohort_ratings %>% filter(PersonID == "495287") %>% 
#   count(eval_peroid, residentyear)


# overall rating ----
person_values = all_grads_eval_end %>% 
  group_by(PersonID) %>% 
  mutate(IntResponseValue = as.numeric(IntResponseValue),
         IntResponseValue_mean = mean(IntResponseValue, na.rm = T)) %>% 
  distinct(PersonID, SponsorID, IntResponseValue_mean) %>% 
  ungroup() %>% 
  glimpse()

# mean operative milestone ratings (PC3, MK2, ICS3)------
operative_rating = all_grads_eval_end %>% 
  filter(QuestionKey %in% c("comp1_PC_Q3", "comp2_MK_Q2", "comp6_ICS_Q3")) %>% 
  group_by(PersonID) %>% 
  mutate(operative_rating_mean = mean(as.numeric(IntResponseValue), na.rm = T)) %>% 
  distinct(PersonID, operative_rating_mean) %>% 
  ungroup() %>% 
  glimpse()

operative_rating = operative_rating %>% 
  mutate(operative_rating_ge8 = ifelse(operative_rating_mean<8, 0,1))

# mean professionalism------
prof_rating = all_grads_eval_end %>% 
  filter(ReportCategory == "Professionalism") %>% 
  group_by(PersonID) %>% 
  mutate(prof_rating_mean = mean(as.numeric(IntResponseValue), na.rm = T)) %>% 
  distinct(PersonID, prof_rating_mean) %>% 
  ungroup() %>% 
  glimpse()

prof_rating = prof_rating %>% 
  mutate(prof_rating_ge8 = ifelse(prof_rating_mean<8, 0, 1))


# leadership --------
leadership_rating = all_grads_eval_end %>% 
  filter(QuestionKey %in% c("comp5_PR_Q1", "comp6_ICS_Q1", "comp6_ICS_Q2", "comp3_SBP_Q1")) %>% 
  group_by(PersonID) %>% 
  mutate(leadership_rating_mean = mean(as.numeric(IntResponseValue), na.rm = T)) %>% 
  distinct(PersonID, leadership_rating_mean) %>% 
  ungroup() %>% 
  glimpse()

leadership_rating = leadership_rating %>% 
  mutate(leadership_rating_ge8 = ifelse(leadership_rating_mean<8, 0, 1))


# mean rating less than 8 ------
person_less_than_8 = person_values %>%
  filter(IntResponseValue_mean < 8) %>% pull(PersonID)

mean_gt_8 = all_grads_eval_end %>% 
  distinct(PersonID) %>% 
  mutate(mean_ge_8 = ifelse(PersonID %in% person_less_than_8, 0, 1))


# ever has low rating ------
low_rating_person = all_grads_eval_end %>% 
  mutate(ever_less_8_rating = as.numeric(IntResponseValue)<8) %>% 
  filter(ever_less_8_rating == TRUE) %>% 
  distinct(PersonID, ever_less_8_rating) %>% 
  pull(PersonID)

low_rating = all_grads_eval_end %>% 
  distinct(PersonID) %>% 
  mutate(never_less_8_rating = ifelse(PersonID %in% low_rating_person, 0, 1))


# person value all -----
person_values = person_values %>% 
  left_join(prof_rating) %>% 
  left_join(operative_rating) %>% 
  left_join(low_rating) %>% 
  left_join(mean_gt_8) %>% 
  left_join(leadership_rating) 

# flag final cohort trainees -----
n_distinct(person_values$PersonID)

final_cohort_personid = milestone_medicare_ratings %>% 
  filter(month<=24, !is.na(IntResponseValue_mean)) %>% 
  distinct(PersonID) %>% 
  pull

person_values = person_values %>% 
  mutate(final_cohort = ifelse(PersonID %in% final_cohort_personid,
                               1, 0))

# save eval values ----
# save(person_values, file = "data/person_values.rdata")




# Q2: there were missing Milestones ratings for some trainees at the time of graduation? ----
no_end_year = all_grads_eval %>%
  select(PersonID) %>%
  anti_join(all_grads_eval_end) %>%
  distinct()

example = milestone_final_year %>%
  filter(PersonID == 551245)

example %>% count(eval_peroid, residentyear)


# answer: We defined the time of graduation as 1. resident year = 5 and 
# 2. eval period = "Year-End Milestone Evaluations". A small number of trainees
# don't have "Year-End" rating (35 out of 3521 didn't have year-end ratings). 
# We excluded these group in our cohort.

# multi graduation year
person_year = milestone_final_year %>% 
  filter(residentyear == 5) %>% 
  distinct(PersonID, residentyear, AcademicYear) %>% 
  arrange(PersonID, desc(AcademicYear)) %>% 
  group_by(PersonID) %>% mutate(n_yr=n()) %>% ungroup()

select_id = person_year %>% 
  filter(n_yr >1) %>% 
  distinct(PersonID) %>% 
  pull

milestone_final_year %>% 
  filter(PersonID %in% select_id)

sum(unique(final_cohort_ratings$PersonID) %in% select_id)

