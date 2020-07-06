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

# milestone data
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_final_year.rdata")

person_year = milestone_final_year %>% 
  filter(residentyear == 5) %>% 
  distinct(PersonID, residentyear, AcademicYear) %>% 
  arrange(PersonID, desc(AcademicYear)) %>% 
  group_by(PersonID) %>% 
  slice(1) %>% ungroup()


# load medicare data
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/medicare_gs_by_abs.rdata")

analytic_data = medicare_gs

# match by NPI process -------
## 1. unique match -----
n_distinct(analytic_data$id_physician_npi) # n 33760
n_distinct(milestone_person$npi.linked)    # n 4628

milestone_person %>% 
  left_join(person_year) %>% 
  distinct(npi.linked, .keep_all = T) %>% 
  count(AcademicYear, residentyear, name = "n_surg")
  
sum(is.na(milestone_person$npi.linked))/nrow(milestone_person)
# 0.01991104

milestone_medicare = analytic_data %>% 
  inner_join(milestone_person, by = c("id_physician_npi" = "npi.linked"))

n_distinct(milestone_medicare$id_physician_npi) # 1083

milestone_medicare_unique = milestone_medicare %>% 
  distinct(PersonID, id_physician_npi) %>% 
  glimpse()

# delete duplicate npi and person ID
milestone_medicare_unique = milestone_medicare_unique %>% 
  add_count(id_physician_npi, name = "npi_n") %>% 
  add_count(PersonID, name = "id_n") %>% 
  filter(npi_n ==1, id_n ==1) %>% 
  select(-npi_n, -id_n)


# didn't get linked milestoners
milestone_person %>% 
  anti_join(milestone_medicare_unique) %>% 
  select(PersonID, NPI = npi.linked) %>% 
  glimpse()


# 2. multiple matches ----------
# 2.1 multi match by NPEES and can't decide by yog or yob ------
milestone_person_m = milestone_person %>% 
  filter(is.na(npi.linked))

multi = milestone_nppes_ama_abs_15_18 %>% 
  select(PersonID, NPI.ms, NPI.nppes) %>% 
  gather("npi_version", "npi", -PersonID) %>% 
  filter(PersonID %in% milestone_person_m$PersonID) %>% 
  filter(!is.na(npi)) 

# check
n_distinct(multi$PersonID)  # 15

milestone_medicare_muti = analytic_data %>% 
  inner_join(multi, by = c("id_physician_npi" = "npi"))

milestone_match_with_medicare_person = milestone_medicare_muti %>% 
  distinct(PersonID, id_physician_npi)

# PersonID id_physician_npi
# 494347   1760771901      
# 533431   1871693077  


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
# 9

# abs 
npi_abs = no_match_abs %>% 
  distinct(PersonID, npi.abs) 

abs_medicare_person = analytic_data %>% 
  inner_join(npi_abs, by = c("id_physician_npi" = "npi.abs")) %>% 
  distinct(PersonID, id_physician_npi) %>% 
  glimpse()
# 3

# 3. combine unique and multiple --------
milestone_medicare_person = rbind(milestone_medicare_unique,
milestone_match_with_medicare_person,
ama_medicare_person,
abs_medicare_person) %>% 
  distinct()

# delete dup for personID and NPI
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

milestone_medicare %>% 
  distinct(id_physician_npi, AcademicYear) %>% 
  add_count(id_physician_npi) %>% 
  filter(n>1)

n_distinct(milestone_medicare$id_physician_npi) 

milestone_medicare %>% 
  distinct(id_physician_npi, AcademicYear) %>% 
  count(AcademicYear, name = "n_surg")
  

# filter non-fellowship surgeons-----
# 1. fellowship data------
fellow_council = read_csv("~/Box/ACGME Milestones v Outcomes/ACGME Milestones v Outcomes - Data/fellowship_npi.csv") %>% 
  mutate(npi= as.character(npi))

n_distinct(milestone_medicare$id_physician_npi)

milestone_medicare_gs = milestone_medicare %>%
  anti_join(fellow_council, by = c("id_physician_npi" = "npi")) 

n_distinct(milestone_medicare_gs$id_physician_npi) #1080

# 2. NPI degree------
load("/Volumes/George_Surgeon_Projects/Other/NPPES_Data_Dissemination_January_2020/npi_md_single_spty_gs.rdata")

milestone_medicare_gs =  milestone_medicare_gs %>% 
  mutate(nppes_gs = ifelse(id_physician_npi %in% npi_md_single_spty_gs$NPI,
                           "gs","not gs")) 

milestone_medicare_gs = milestone_medicare_gs %>% 
  filter(nppes_gs == "gs") 

n_distinct(milestone_medicare_gs$id_physician_npi) #782

milestone_medicare_gs %>% 
  group_by(facility_clm_yr) %>% 
  mutate(facility_clm_yr = facility_clm_yr +2007) %>% 
  mutate(n_surgeon = n_distinct(id_physician_npi),
         n_cases = n()) %>% 
  distinct(facility_clm_yr, n_surgeon, n_cases)


# 3. last least 5 cases in first 12 month -------
# add practice month
milestone_medicare_gs = milestone_medicare_gs %>% 
  mutate(graduate_date_ms = paste0(AcademicYear, "-07-01"),
         graduate_date_ms = as_date(graduate_date_ms),
         month = ceiling((as_date(dt_facclm_adm) - as_date(graduate_date_ms))/30),
         month = as.numeric(month)) %>% 
  filter(month>0)

n_distinct(milestone_medicare_gs$id_physician_npi) #779

save(milestone_medicare_gs, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_gs.rdata")


ggplot(data = milestone_medicare_gs, aes(x = month)) +
  geom_histogram(bins = 30) +
  labs(caption = "number of cases performed after month of graduation",
       y = "number of medicare cases",
       x = "months after graduation") +
  # coord_cartesian(xlim = c(10, 15)) +
  theme_minimal()

ggsave(filename = "images/month_after_grad_cases.png")

# facility claim year and graduation 
t1 = milestone_medicare_gs %>% 
  mutate(facility_clm_yr = facility_clm_yr + 2007) %>% 
  filter(facility_clm_yr == 2015) %>% 
  distinct(AcademicYear, id_physician_npi) %>% 
  count(AcademicYear, name = "n_surg")

t2 = milestone_medicare_gs %>% 
  mutate(facility_clm_yr = facility_clm_yr + 2007) %>% 
  filter(facility_clm_yr == 2016) %>% 
  distinct(AcademicYear, id_physician_npi) %>% 
  count(AcademicYear, name = "n_surg")

t3 = milestone_medicare_gs %>% 
  mutate(facility_clm_yr = facility_clm_yr + 2007) %>% 
  filter(facility_clm_yr == 2017) %>% 
  distinct(AcademicYear, id_physician_npi) %>% 
  count(AcademicYear, name = "n_surg")

rbind(t1,t2,t3) %>% 
  rename(" " = AcademicYear) %>% 
  kable(caption = "year of graduation by claim year") %>% 
  kable_styling(full_width = F) %>% 
  pack_rows("claim year 2015", 1,2) %>% 
  pack_rows("claim year 2016", 3,4) %>%  
  pack_rows("claim year 2017", 5,8)  


# only keep partial colectomy ------
milestone_medicare_pc = milestone_medicare_gs %>% 
  filter(e_proc_grp_lbl == "Partial Colectomy")

n_distinct(milestone_medicare_pc$id_physician_npi) #577

save(milestone_medicare_pc, file = "/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_pc.rdata")

nrow(milestone_medicare_pc)
