# add new milestone rating category variables

milestone_ratings <- function(data) {
  # mean milestone ratings --------
  person_values = data %>% 
    group_by(PersonID) %>% 
    mutate(IntResponseValue = as.numeric(IntResponseValue),
           IntResponseValue_mean = mean(IntResponseValue, na.rm = T)) %>% 
    distinct(PersonID, SponsorID, IntResponseValue_mean) %>% 
    ungroup() 
  
  # mean operative milestone ratings (PC3, MK2, ICS3)------
  operative_rating = data %>% 
    filter(QuestionKey %in% c("comp1_PC_Q3", "comp2_MK_Q2", "comp6_ICS_Q3")) %>% 
    group_by(PersonID) %>% 
    mutate(operative_rating_mean = mean(as.numeric(IntResponseValue), na.rm = T)) %>% 
    distinct(PersonID, operative_rating_mean) %>% 
    ungroup() 
  
  operative_rating = operative_rating %>% 
    mutate(operative_rating_ge8 = ifelse(operative_rating_mean<8, 0,1))
  
  
  # mean professionalism------
  prof_rating = data %>% 
    filter(ReportCategory == "Professionalism") %>% 
    group_by(PersonID) %>% 
    mutate(prof_rating_mean = mean(as.numeric(IntResponseValue), na.rm = T)) %>% 
    distinct(PersonID, prof_rating_mean) %>% 
    ungroup() 
  
  prof_rating = prof_rating %>% 
    mutate(prof_rating_ge8 = ifelse(prof_rating_mean<8, 0, 1))
  
  
  # leadership --------
  leadership_rating = data %>% 
    filter(QuestionKey %in% c("comp5_PR_Q1", "comp6_ICS_Q1", "comp6_ICS_Q2", "comp3_SBP_Q1")) %>% 
    group_by(PersonID) %>% 
    mutate(leadership_rating_mean = mean(as.numeric(IntResponseValue), na.rm = T)) %>% 
    distinct(PersonID, leadership_rating_mean) %>% 
    ungroup() 
  
  leadership_rating = leadership_rating %>% 
    mutate(leadership_rating_ge8 = ifelse(leadership_rating_mean<8, 0, 1))
  
  # mean rating less than 8 ------
  person_less_than_8 = person_values %>%
    filter(IntResponseValue_mean < 8) %>% pull(PersonID)
  
  mean_gt_8 = data %>% 
    distinct(PersonID) %>% 
    mutate(mean_ge_8 = ifelse(PersonID %in% person_less_than_8, 0, 1))
  
  
  # ever has low rating ------
  low_rating_person = data %>% 
    mutate(ever_less_8_rating = as.numeric(IntResponseValue)<8) %>% 
    filter(ever_less_8_rating == TRUE) %>% 
    distinct(PersonID, ever_less_8_rating) %>% 
    pull(PersonID)
  
  low_rating = data %>% 
    distinct(PersonID) %>% 
    mutate(never_less_8_rating = ifelse(PersonID %in% low_rating_person, 0, 1))
  
  # person value all -----
  person_values %>% 
    left_join(prof_rating) %>% 
    left_join(operative_rating) %>% 
    left_join(low_rating) %>% 
    left_join(mean_gt_8) %>% 
    left_join(leadership_rating)
}
