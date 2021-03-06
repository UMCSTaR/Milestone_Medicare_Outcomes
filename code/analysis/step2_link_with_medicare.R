# link Milestone data to medicare
# description:
# - unique matched PersonID and Medicare
# - multiple matched PersonID to Medicare (to get more possible matched)
# - npi from AMA and ABS to match with Medicare

library(tidyverse)
library(tidyext)
library(lubridate)
library(kableExtra)


# load matched ACGME data
# unique matches
milestone_person = read_csv("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/milestone_linked_npi.csv") %>% 
  mutate_if(is.numeric, as.character)
# multiple matches
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/milestone_nppes_ama_abs_15_18.rdata")

# ABS fellow data
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/abs_fellowship_npi.rdata")
# non-us medical school students flag from AMA and ABS
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/non_us_npi.rdata")

# milestone evaluation data
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_final_year.rdata")

# keep pgy5 student
person_year = milestone_final_year %>% 
  filter(residentyear == 5) %>% 
  distinct(PersonID, residentyear, AcademicYear) %>% 
  arrange(PersonID, desc(AcademicYear)) %>% 
  group_by(PersonID) %>% 
  slice(1) %>% ungroup()

# add graduation year
# after checking, gradution year is academicyear+1
person_year = person_year %>% 
  mutate(AcademicYear = as.numeric(AcademicYear)) %>% 
  mutate(grad_year = AcademicYear+1)

person_year %>% 
  count(grad_year, AcademicYear)

# delete milestone grad year 2018, because no medicare data
person_year_15_17 = person_year %>% 
  filter(grad_year <=2017)

# save data
write_csv(person_year_15_17, "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/all_grads_15_17.csv")

person_year_15_17 %>% 
  count(grad_year, name = "n trainees")

n_distinct(person_year_15_17$PersonID) #3521

milestone_person_15_17 = milestone_person %>% 
  filter(PersonID %in% person_year_15_17$PersonID)



# # load medicare data
# load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/medicare_gs_by_abs.rdata")
# 
# analytic_data = medicare_gs
# 
# # match by NPI process -------
# ## 1. unique match -----
# n_distinct(analytic_data$id_physician_npi) # n 33760
# n_distinct(milestone_person_15_17$npi.linked)    # n 4628
# 
# sum(is.na(milestone_person_15_17$npi.linked))/nrow(milestone_person_15_17)
# # 0.02


# 2. exclude specialty trained and non-us------
# 2.1 exclude non-us -----
milestone_person_us = milestone_person_15_17 %>% 
  filter(!npi.linked %in% non_us_npi) 

n_distinct(milestone_person_us$npi.linked) #2875

# 2.2 exclude fellowship from ABS ------
milestone_person_us_no_fellow_abs = milestone_person_us %>% 
  filter(!npi.linked %in% abs_fellowship_npi)  

milestone_person_us_no_fellow_abs %>% 
  left_join(person_year) %>% 
  distinct(npi.linked, .keep_all = T) %>% 
  count(grad_year, name = "n_surg")

# 2.3 fellowship council data ------
fellow_council = read_csv("~/Box/ACGME Milestones v Outcomes/ACGME Milestones v Outcomes - Data/fellowship_npi.csv") %>% 
  mutate(npi= as.character(npi))

milestone_person_us_no_fellow = milestone_person_us_no_fellow_abs %>%
  anti_join(fellow_council, by = c("npi.linked" = "npi")) 

milestone_person_us_no_fellow %>% 
  left_join(person_year) %>% 
  distinct(npi.linked, .keep_all = T) %>% 
  count(grad_year, residentyear, name = "n_surg")


# 2.4 exclude fellowship from Carrier Specialty list -----
load("/Volumes/George_Surgeon_Projects/standardized_medicare_data_using_R/analysis_ready_data/all_gs_splty.rdata")

milestone_person_us_no_fellow =  milestone_person_us_no_fellow %>% 
  mutate(carrier_nppes_gs = ifelse(npi.linked %in% gs_splty_only,
                           "gs","not gs")) 

milestone_person_us_nppes = milestone_person_us_no_fellow %>% 
  filter(carrier_nppes_gs == "gs") 

milestone_person_us_nppes %>% 
  left_join(person_year) %>% 
  count(grad_year)

# # QA NPPES NPI ----
# load("/Volumes/George_Surgeon_Projects/Other/NPPES_Data_Dissemination_January_2020/npi_md_single_spty_gs_2020.rdata")
# milestone_person_us_no_fellow = milestone_person_us_no_fellow %>%
#   mutate(npi_nppes_gs = ifelse(npi.linked %in% npi_md_single_spty_gs$NPI,
#                            "gs","not gs"))
# 
# save(milestone_person_us_no_fellow, file = "data/milestone_person_us_no_fellow.rdata")


# join medicare with milestone by npi
milestone_medicare = analytic_data %>% 
  inner_join(milestone_person_us_nppes, by = c("id_physician_npi" = "npi.linked"))

n_distinct(milestone_medicare$id_physician_npi) # 944

milestone_medicare_unique = milestone_medicare %>% 
  distinct(PersonID, id_physician_npi) %>% 
  glimpse()

# delete duplicate npi and person ID if any
milestone_medicare_unique = milestone_medicare_unique %>% 
  add_count(id_physician_npi, name = "npi_n") %>% 
  add_count(PersonID, name = "id_n") %>% 
  filter(npi_n ==1, id_n ==1) %>% 
  select(-npi_n, -id_n)


# didn't get linked milestoners
milestone_person_us_nppes %>% 
  anti_join(milestone_medicare_unique) %>% 
  select(PersonID, NPI = npi.linked) %>% 
  glimpse()


# 3. multiple matches ----------
# 2.1 multi match by NPEES and can't decide by yog or yob ------
milestone_person_m = milestone_person_us_nppes %>% 
  filter(is.na(npi.linked))

# check
n_distinct(milestone_person_m$PersonID)  # 0; all linked milstone has unique NPI


# 2.2 AMA and ABS with no matches, by names ----------
# these people didn't match by NPPES file using names but matched with AMA or ABS,
# use the matched NPI from AMA/ABS to match with Medicare
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/no_match_ama.rdata")
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/de_name_data/no_match_abs.rdata")

# ama
npi_ama = no_match_ama %>% 
  distinct(PersonID, npi) %>% 
  rename(npi.ama = npi)

ama_medicare_person = analytic_data %>% 
  inner_join(npi_ama, by = c("id_physician_npi" = "npi.ama")) %>% 
  distinct(PersonID, id_physician_npi) %>% 
  glimpse()
# 11

# abs 
npi_abs = no_match_abs %>% 
  distinct(PersonID, npi.abs) 

abs_medicare_person = analytic_data %>% 
  inner_join(npi_abs, by = c("id_physician_npi" = "npi.abs")) %>% 
  distinct(PersonID, id_physician_npi) %>% 
  glimpse()
# 4

# 3. combine unique and multiple --------
milestone_medicare_person = rbind(
  milestone_medicare_unique,
  ama_medicare_person,
  abs_medicare_person
) %>%
  distinct()

# delete dup for personID and NPI if any
milestone_medicare_person = milestone_medicare_person %>% 
  add_count(PersonID, name = "id_n") %>% 
  add_count(id_physician_npi, name = "npi_n") %>% 
  filter(id_n == 1, npi_n == 1) %>% 
  select(-id_n, -npi_n)


# 4. add graduation year ------
milestone_medicare_person = milestone_medicare_person %>% 
  left_join(person_year) 

# select medicare cases
milestone_medicare = analytic_data %>% 
  inner_join(milestone_medicare_person, by = "id_physician_npi")

n_distinct(milestone_medicare$id_physician_npi)  #957

milestone_medicare %>% 
  distinct(id_physician_npi, grad_year) %>% 
  count(grad_year, name = "n_surg")

milestone_medicare %>% 
  group_by(facility_clm_yr) %>% 
  mutate(n_surg = length(unique(id_physician_npi)),
         n_case = n()) %>% 
  distinct(facility_clm_yr, n_surg, n_case) %>% 
  mutate(facility_clm_yr = facility_clm_yr+2007)

# QA why didn't match -----
matched_2015 = milestone_medicare %>% 
  filter(grad_year ==2015) %>% 
  distinct(id_physician_npi) %>% 
  pull

not_match_2015_ms = milestone_person_us_nppes %>% 
  left_join(person_year, by = "PersonID") %>% 
  filter(grad_year == 2015,
         !npi.linked %in% matched_2015) %>% 
  select(npi.linked, grad_year)

# match with all chop procedure cohort
all_chop_surgoens = data.table::fread("/Volumes/George_Surgeon_Projects/standardized_medicare_data_using_R/std/full_data/professionals.csv",
                                      select = "provider_npi")

milestone_person_us_nppes %>% 
  left_join(person_year, by = "PersonID") %>% 
  select(npi = npi.linked, grad_year) %>% 
  filter(npi %in% pull(all_chop_surgoens)) %>% 
  count(grad_year)
  

# 5. practice after graduation -------
# add 
milestone_medicare = milestone_medicare %>% 
  mutate(graduate_date_ms = paste0(grad_year, "-07-01"),
         graduate_date_ms = as_date(graduate_date_ms),
         month = ceiling((as_date(dt_facclm_adm) - as_date(graduate_date_ms))/30),
         month = as.numeric(month)) %>% 
  filter(month>0)

milestone_medicare %>% 
  mutate(facility_clm_yr = facility_clm_yr +2007) %>% 
  count(facility_clm_yr)

quantile(milestone_medicare$month, probs = c(0.05,0.1, 0.3,0.4,.5,.6,.9,1))

n_distinct(milestone_medicare$id_physician_npi) #951

milestone_medicare_gs = milestone_medicare

save(milestone_medicare_gs, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_gs.rdata")

# check graduation year
milestone_medicare_gs %>% 
  distinct(id_physician_npi, grad_year, graduate_date_ms) %>% 
  group_by(grad_year) %>% 
  slice(1:2)

ggplot(data = milestone_medicare_gs, aes(x = month)) +
  geom_histogram(bins = 30) +
  labs(caption = "number of cases performed after month of graduation",
       y = "number of medicare cases",
       x = "months after graduation") +
  # coord_cartesian(xlim = c(10, 15)) +
  theme_minimal()

ggsave(filename = "images/month_after_grad_cases.png")

# facility claim year and graduation 
n_distinct(milestone_medicare_gs$id_physician_npi)

t1 = milestone_medicare_gs %>% 
  mutate(facility_clm_yr = facility_clm_yr + 2007) %>% 
  filter(facility_clm_yr == 2015) %>% 
  distinct(grad_year, id_physician_npi) %>% 
  count(grad_year, name = "n_surg")

t2 = milestone_medicare_gs %>% 
  mutate(facility_clm_yr = facility_clm_yr + 2007) %>% 
  filter(facility_clm_yr == 2016) %>% 
  distinct(grad_year, id_physician_npi) %>% 
  count(grad_year, name = "n_surg")

t3 = milestone_medicare_gs %>% 
  mutate(facility_clm_yr = facility_clm_yr + 2007) %>% 
  filter(facility_clm_yr == 2017) %>% 
  distinct(grad_year, id_physician_npi) %>% 
  count(grad_year, name = "n_surg")

rbind(t1,t2,t3) %>% 
  rename(" " = grad_year) %>% 
  kable(caption = "year of graduation by claim year") %>% 
  kable_styling(full_width = F) %>% 
  pack_rows("claim year 2015", 1,1) %>% 
  pack_rows("claim year 2016", 2,3) %>%  
  pack_rows("claim year 2017", 4,6)  

# 5.1 at least 1 procedures in first 12 months -------

first12_cases = milestone_medicare_gs %>% 
  filter(month<=12) %>% 
  add_count(id_physician_npi) %>% 
  distinct(id_physician_npi, n) 

ggplot(data = first12_cases) +
  geom_histogram(aes(x= n)) +
  labs(x = "number of cases in the first 12 months",
       y = "number of surgons",
       caption = "cohort includes all CHOP 5 procedures") +
  theme_minimal()

quantile(first12_cases$n)

# surgeons who had at least one casew in their first 12 month practice
milestone_medicare_gs_1case = milestone_medicare_gs %>% 
  filter(id_physician_npi %in% first12_cases$id_physician_npi) 

save(milestone_medicare_gs_1case, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/data_08_03/milestone_medicare_all_procedure_1case.rdata")

# 6. only keep partial colectomy ------
milestone_medicare_pc = milestone_medicare_gs_1case %>% 
  filter(e_proc_grp_lbl == "Partial Colectomy")

n_distinct(milestone_medicare_pc$id_physician_npi) #535
nrow(milestone_medicare_pc)

save(milestone_medicare_pc, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/data_08_03/milestone_medicare_pc.rdata")


# 6.1 partial colectomy by year------
# 12 month-----
milestone_medicare_pc_year1 = milestone_medicare_pc %>% 
  filter(month<=12)

n_distinct(milestone_medicare_pc_year1$id_physician_npi) #483; at least 1 case was based on 5 procedures; here is only PC

nrow(milestone_medicare_pc_year1)

save(milestone_medicare_pc_year1, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/data_08_03/milestone_medicare_pc_year1.rdata")


# 24 months-----
milestone_medicare_pc_year2 = milestone_medicare_pc %>% 
  filter(month<=24)

n_distinct(milestone_medicare_pc_year2$id_physician_npi) #527; at least 1 case was based on 5 procedures; here is only PC

nrow(milestone_medicare_pc_year2)

save(milestone_medicare_pc_year2, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/data_08_03/milestone_medicare_pc_year2.rdata")


