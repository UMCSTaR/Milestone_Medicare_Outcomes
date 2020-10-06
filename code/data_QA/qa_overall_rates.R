medicare_17 = data.table::fread("/Volumes/George_Surgeon_Projects/standardized_medicare_data_using_R/analytic/full_data/analytic_selected_vars.csv")            
medicare_16 = data.table::fread("/Volumes/George_Surgeon_Projects/medicare_data/full_std_analytic/analytic.csv")            
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_ratings.rdata")

data_qa <- function(data) {
  data %>%
    summarise(
      n_case = n(),
      n_surgeon = length(unique(id_physician_npi)),
      n_hosp = length(unique(facility_prvnumgrp)),
      n_patient = length(unique(member_id)),
         death_rate = sum(flg_death_30d, na.rm = T) / n_case,
      readmit_rate = sum(flg_readmit_30d) / n_case,
      reop_rate = sum(flg_util_reop) / n_case
    ) %>%
    mutate_at(vars(contains("rate")), scales::percent_format())
}

milestone_sum = data_qa(milestone_medicare_pc) %>% 
  mutate(data_src = "milestone_medicare_partial_colectomy") %>% 
  select(data_src, contains("rate"))
medicare_16_sum = data_qa(medicare_16) %>% 
  mutate(data_src = "medicare07-16") %>% 
  select(data_src, contains("rate"))
medicare_17_sum = data_qa(medicare_17) %>% 
  mutate(data_src = "medicare07-17") %>% 
  select(data_src, contains("rate"))


rbind(milestone_sum, medicare_16_sum, medicare_17_sum)


