library(tidyverse)

# abs_original = read.csv("/Users/xilinchen/Documents/projects/ABS data/data/SurgeonTableComplete_v4_Bea2.csv", sep = "|") %>% 
#   mutate(npi = as.character(npi))
# 
# abs_select = abs_original %>% 
#   select(npi, contains("_name"), year) %>% 
#   rename(med_school_yog = year) %>% 
#   filter(!is.na(npi))
# 
# save(abs_select, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/abs_select.rdata")

# abs
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/abs_select.rdata")
# unmatched
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/step2_ama/nppes_ama_npi_no_match.rdata")

# add flag for abs
abs_select = abs_select %>% 
  mutate(abs = 1) %>% 
  rename(first_name_abs = first_name,
         middle_name_abs = middle_name,
         last_name_abs = last_name)

# add yog for milestone
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/original/milestone_02_2020.rdata")
milestone_degree = milestone %>% 
  distinct(PersonID, Degree_date) %>% 
  mutate(degree_year = lubridate::year(Degree_date))

nppes_ama_npi_no_match = nppes_ama_npi_no_match %>%   # add degree date
  left_join(milestone_degree)


# match by NPI -----
nppes_ama_abs = nppes_ama_npi_no_match %>% 
  left_join(abs_select, by = c("NPI" = "npi"))

# create flg for matched med school year
nppes_ama_abs = nppes_ama_abs %>% 
  mutate(yog_ms_abs = ifelse(degree_year == med_school_yog, 1, 0))

nppes_ama_abs_match = nppes_ama_abs %>% 
  filter(yog_ms_abs == 1)  %>% 
  add_count(PersonID) %>% 
  filter(n == 1)  %>% 
  select(-n)

save(nppes_ama_abs_match, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/step3_abs/nppes_ama_abs_match.rdata")
  
# non macth
nppes_ama_abs_no_match = nppes_ama_npi_no_match %>% 
  filter(!PersonID %in% nppes_ama_abs_match$PersonID) 

n_distinct(nppes_ama_abs_no_match$PersonID)

