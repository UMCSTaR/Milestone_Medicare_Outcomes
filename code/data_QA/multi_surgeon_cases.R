# check multi-surgeon cases in Partical colectomy cohort in MOV
# - find all cases in medicare by admissiion date and discharge date, and patient ID.
# flag PA


library(tidyverse)

# load dt
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_ratings.rdata") # milestone PC cohort
raw = data.table::fread("/Volumes/George_Surgeon_Projects/standardized_medicare_data_using_R/analysis_ready_data/Archived/analytic_selected_vars.csv") # all medicare analytic file 2007 - 2017
load("/Volumes/George_Surgeon_Projects/Surgeon Profile data/pa_splty_only.rdata") # all PA NPI defined by carrier claim

# create PC 24 months cohort -----
milestone_medicare_pc = milestone_medicare_ratings %>% 
  mutate(hosp_beds = case_when(hosp_beds_2grp == 0 ~ "<350",
                               hosp_beds_2grp == 1 ~ ">=350")) %>% 
  filter(!is.na(IntResponseValue_mean),
         month <= 24)

nrow(milestone_medicare_pc) # 2368

milestone_medicare_pc %>% tidyext::cat_by(flg_multi_surgeon)
milestone_medicare_pc %>% tidyext::cat_by(flg_assistant_surgeon) # 36% are assistant surgeons

# filter only multi-surgeon cases-------
milestone_medicare_pc_multi = milestone_medicare_pc %>% 
  filter(flg_multi_surgeon ==1) 
#1450

milestone_medicare_pc_multi %>% tidyext::cat_by(flg_assistant_surgeon)

select_case_date = milestone_medicare_pc_multi %>% 
  distinct(member_id, dt_facclm_adm, dt_facclm_dschg) 

# find all medicare cases that filed with the multi cases in MOV
all_multi_surg = raw  %>% 
  mutate(dt_facclm_adm = as.character(dt_facclm_adm),
         dt_facclm_dschg = as.character(dt_facclm_dschg)) %>% 
  inner_join(select_case_date)

nrow(all_multi_surg) #2858

# add flg for MOV surgeons and PA
all_multi_surg = all_multi_surg %>% 
  mutate(mov_surg = ifelse(id_physician_npi %in% milestone_medicare_pc$id_physician_npi, 1,0),
         pa = ifelse(id_physician_npi %in% pa_splty_only, 1, 0))

# check 10 cases
all_multi_surg %>% select(id_physician_npi, dt_facclm_adm,
                          dt_facclm_dschg, mov_surg, flg_assistant_surgeon, pa)

summary_surg = all_multi_surg %>%
  group_by(member_id, dt_facclm_adm, dt_facclm_dschg) %>%
  mutate(pa_part = ifelse(mean(pa), 1, 0),
         assist_sug_part = ifelse(mean(flg_assistant_surgeon), 1, 0)) %>%
  ungroup() 

summary_surg %>% 
  rename(having_assit_surg = assist_sug_part) %>% 
  tidyext::cat_by(having_assit_surg)

summary_surg %>% 
  count(mov_surg, assist_sug_part)

test = summary_surg %>% 
  select( dt_facclm_adm, dt_facclm_dschg, member_id, id_physician_npi,flg_assistant_surgeon, assist_sug_part, mov_surg) %>% 
  glimpse()

