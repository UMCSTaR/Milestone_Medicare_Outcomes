# create surgeon roles flags-----
# - create a new flag indicating had assistant surgeons 
# - exclude assistant surgeon claims (keep the primary case)
# - exclude multi-surgeon flag from covariates 


library(tidyverse)

# load dt----
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_ratings.rdata") # milestone PC cohort with ratings
raw = data.table::fread("/Volumes/George_Surgeon_Projects/standardized_medicare_data_using_R/analysis_ready_data/Archived/analytic_selected_vars.csv", stringsAsFactors = F) # all medicare analytic file 2007 - 2017
load("/Volumes/George_Surgeon_Projects/Surgeon Profile data/pa_splty_only.rdata") # all PA NPI defined by carrier claim


# rename to PC cohort to be clear--
milestone_medicare_pc = milestone_medicare_ratings

# identify case ----
# using procedure date and CPT,and patient ID
select_case_date = milestone_medicare_pc %>% 
  distinct(member_id, dt_profsvc_start, dt_profsvc_end, cpt_cd) 

# create unique case ID
select_case_date = select_case_date %>% 
  mutate(case_id = row_number()) %>% 
  select(case_id, everything())

all_cases_by_ms_surg = raw  %>% 
  mutate(dt_profsvc_start = as.character(dt_profsvc_start),
         dt_profsvc_end = as.character(dt_profsvc_end)) %>% 
  inner_join(select_case_date)


# add flg for MOV surgeons and PA----
all_cases_by_ms_surg = all_cases_by_ms_surg %>% 
  mutate(mov_surg = ifelse(id_physician_npi %in% milestone_medicare_pc$id_physician_npi, 1,0),
         pa = ifelse(id_physician_npi %in% pa_splty_only, 1, 0))


# glimpse cases
all_cases_by_ms_surg %>% select(case_id,id_physician_npi, dt_profsvc_start, dt_profsvc_end, mov_surg, flg_assistant_surgeon, pa)

# 1. define had assistant surgeons ------
all_cases_by_ms_surg = all_cases_by_ms_surg %>%
  group_by(case_id) %>%
  mutate(pa_part = ifelse(mean(pa), 1, 0),
         had_assist_surg = ifelse(mean(flg_assistant_surgeon), 1, 0)) %>%
  ungroup() 


all_cases_by_ms_surg %>% 
  tidyext::cat_by(had_assist_surg)

claim_w_assist = all_cases_by_ms_surg %>% 
  select(id, had_assist_surg)


# 2. exclude assistant surgeon claims -----
milestone_medicare_pc %>% 
  tidyext::cat_by(flg_assistant_surgeon)


milestone_medicare_no_assist = milestone_medicare_pc %>% 
  filter(flg_assistant_surgeon == 0) %>% 
  left_join(claim_w_assist, by = "id")

milestone_medicare_no_assist %>% 
  tidyext::cat_by(had_assist_surg)

milestone_medicare_pc_primary = milestone_medicare_no_assist

nrow(milestone_medicare_pc_primary) #1710

save(milestone_medicare_pc_primary, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_pc_primary.rdata")




# not relevant to cohort --------------------------------------------------
# data QA --
library(gtsummary)
library(kableExtra)

# before exclude assistant surgeon summary
load("data/summary_tbl.rdata")

month_24 = milestone_medicare_pc_primary %>% 
  filter(month<=24)

month_24 = month_24%>% 
  mutate(flg_cmp_po_any_not_poa = as.numeric(flg_cmp_po_any_not_poa),
         flg_cmp_po_severe_not_poa = as.numeric(flg_cmp_po_severe_not_poa)) %>% 
  mutate(
    open_colectomy = cpt_cd %in% c(44140:44147, 44160),
    lap_colectomy  = cpt_cd %in% 44204:44208,
    colectomy_type = factor(ifelse(
      cpt_cd %in% c(44140:44147, 44160), 'open', 'lap'
    ))
  ) 

# outcomes desc
sum_tbl_primary = month_24 %>% 
  summarise(n_case = n(),
            n_surgeon = length(unique(id_physician_npi)),
            n_hosp = length(unique(facility_prvnumgrp)),
            n_patient = length(unique(member_id)),
            POA_severe_complication_rate = sum(flg_cmp_po_severe_not_poa)/n_case,
            # severe_complication_rate = sum(flg_cmp_po_severe)/n_case,
            POA_any_complication_rate = sum(flg_cmp_po_any_not_poa)/n_case,
            # any_complication_rate = sum(flg_cmp_po_any)/n_case,
            death_rate = sum(flg_death_30d)/n_case,
            readmit_rate = sum(flg_readmit_30d)/n_case,
            reop_rate = sum(flg_util_reop)/n_case,
            had_assist_surg_rate = sum(had_assist_surg)/n_case) %>% 
  mutate_at(vars(contains("rate")), scales::percent_format()) %>% 
  t() 

merge(sum_tbl_primary,summary_tbl,by="row.names",all.x=TRUE) %>% 
  rename("excluded assistant claims" = V1.x,
         "original cohort" = V1.y,
          " " = Row.names) %>% 
  kable() %>% 
  kable_styling(full_width = F)





