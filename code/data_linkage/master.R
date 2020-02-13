library(tidyverse)
library(lubridate)
library(tidyext)

# load miletone data with names ---------
# load("/Volumes/George_Surgeon_Projects/ACGME_milestone/milestone_person_nppes.rdata")
# 2016 - 2019
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/linkage/milestone_16_18_person.rdata")

str(milestone_16_18_person)

milestone = milestone_16_18_person %>% 
  rename(first_name = FirstName,
         last_name = LastName,
         middle_name = MiddleName) %>% 
  select(PersonID, NationalProviderID, contains("_name"), Birth_date, Degree_date) %>% 
  mutate_at(vars(contains("_name")), str_to_title)  %>% # format name
  mutate(yob.ms = lubridate::year(Birth_date),          # yob
         degree_year.ms = lubridate::year(Degree_date)) %>% 
  rename(NPI = NationalProviderID) %>% 
  glimpse()

milestone_16_18_person %>% 
  add_count(PersonID) %>% 
  filter(n>1) %>% 
  glimpse()
# 1 person ID dup with same NPI


# Missing npi
milestone %>% 
  filter(is.na(NPI)) %>% 
  distinct(PersonID)
# 794

# NPPES -------
load("/Volumes/George_Surgeon_Projects/Other/NPPES_Data_Dissemination_January_2020/npidata_pfile_2020_selected_var_md_do_na.rdata")

# rename data
nppes = npidata_pfile_2020_selected_var_md_do_na %>% 
  mutate(NPI = as.character(NPI),
         md_do_nppes = 1)

nppes = nppes %>% 
  mutate_at(vars(contains("_name")), str_to_title)   %>% # format name
  mutate(middle_initial = str_sub(middle_name, 0, 1))    # middle initial

# left join by first and last name
milestone_nppes =  milestone %>% 
#  left_join(nppes, by = c("first_name", "middle_name" = "middle_initial", "last_name"), suffix = c(".ms", ".nppes")) %>% 
  left_join(nppes, by = c("first_name", "last_name"), suffix = c(".ms", ".nppes")) %>% 
  glimpse()


# no match
milestone_nppes %>% 
  distinct(PersonID, NPI.ms, NPI.nppes, md_do_nppes) %>% 
  filter(is.na(NPI.ms), is.na(md_do_nppes)) 
# 32 surgeons can't get matched



# get unique matched personID 
personid_nppes_m = milestone_nppes %>% 
  filter(!is.na(NPI.nppes)) %>%   # filter matched nppes obs
  add_count(PersonID) %>%         # see if unique or multiple match  
  filter(n==1) %>% 
  pull(PersonID)

milestone_nppes = milestone_nppes %>% 
  mutate(nppes_unique_match = ifelse(PersonID %in% personid_nppes_m, 1, 0))

# descriptive of macthed data
milestone_nppes %>% 
  distinct(PersonID, nppes_unique_match) %>% 
  cat_by(nppes_unique_match)

milestone_nppes %>% 
  filter(is.na(NPI.ms)) %>% 
  distinct(PersonID, nppes_unique_match) %>% 
  cat_by(nppes_unique_match)

# 221 didn't get  unique match in NPPES
# 573 get unique macthed with NPPES

rm(personid_nppes_m)

# QA
# no match with NPPES 
milestone_nppes %>% 
  filter(is.na(NPI.nppes)) %>% 
  distinct(PersonID) %>% 
  summarise(n_no_match_nppes = n())

# AMA ------
# load ama selected vars
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/ama_select.rdata")

str(ama_select)

ama = ama_select %>% 
  select(npi, contains("_name"), BirthDate) %>%
  mutate(last_name_ama = str_to_title(last_name_ama),     # format names
         first_name_ama = str_to_title(first_name_ama),
         middle_name_ama =str_to_title(middle_name_ama),
         middle_initia_ama = str_sub(middle_name_ama, 0, 1),
         yob.ama = lubridate::year(BirthDate),
         ama = 1) %>% 
  rename(npi.ama = npi) %>% 
  as_tibble()

str(ama)

# milestone to AMA by NPI  
milestone_nppes_ama = milestone_nppes %>% 
  left_join(ama, by = c("NPI.nppes" = "npi.ama")) %>% 
  mutate(yob_ms_ama = ifelse(yob.ms == yob.ama, 1, 0))        # flag milestone YOB match with ama YOB

milestone_nppes_ama = milestone_nppes_ama %>% 
  select(PersonID, NPI.ms, NPI.nppes, contains("_name"), yob.ms, yob.ama, degree_year.ms,
         md_do_nppes, ama, nppes_unique_match, yob_ms_ama)

str(milestone_nppes_ama)

# ABS --------
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/abs_select.rdata")

# add flag for abs
abs_select = abs_select %>% 
  rename(first_name_abs = first_name,
         middle_name_abs = middle_name,
         last_name_abs = last_name,
         med_school_yog.abs = med_school_yog,
         npi.abs = npi) %>% 
  mutate(abs = 1,
         first_name_abs = as.character(first_name_abs),
         last_name_abs = as.character(last_name_abs),
         middle_name_abs = as.character(middle_name_abs)) 

str(abs_select)

milestone_nppes_ama_abs = milestone_nppes_ama %>% 
  left_join(abs_select, by = c("NPI.nppes" = "npi.abs"))

milestone_nppes_ama_abs = milestone_nppes_ama_abs %>% 
  mutate(yog_ms_abs = ifelse(degree_year.ms == med_school_yog.abs, 1, 0))

# order vars
milestone_nppes_ama_abs = milestone_nppes_ama_abs%>% 
  select(PersonID, nppes_unique_match, yob_ms_ama, yog_ms_abs, everything())

str(milestone_nppes_ama_abs)

# Summary -------
n_distinct(milestone_nppes_ama_abs$PersonID)
# 3580

# unique NPI match ----
milestone_nppes_ama_abs %>% 
  distinct(PersonID, nppes_unique_match) %>% 
  cat_by(nppes_unique_match)
# 2417

nppes_uniq = milestone_nppes_ama_abs %>% 
  filter(nppes_unique_match == 1) %>% 
  glimpse()

# nppes_unique_match     N `% of Total`
# <dbl> <int>        <dbl>
#   1                  1163         32.5
# 2                    2417         67.5

# uniqe NPI with matched YOB and YOG
milestone_nppes_ama_abs %>% 
  filter(nppes_unique_match == 1,
         yob_ms_ama ==1  , yog_ms_abs ==1) %>% 
  summarise(tot = n())
  

# yob + yog for multi match ----
yob_yog = milestone_nppes_ama_abs %>% 
  filter(nppes_unique_match == 0,    # multi match
         yob_ms_ama == 1,
         yog_ms_abs == 1) %>% 
  distinct(PersonID, NPI.nppes) 

str(yob_yog) # unique match using YOG and YOB

# n = 825, all 825 trainees were identified by yob and yog uniquely under multi NPI

# same yob, diff yog
milestone_nppes_ama_abs = milestone_nppes_ama_abs %>% 
  mutate_at(vars(yob_ms_ama, yog_ms_abs),funs(ifelse(is.na(.), 0, .))) %>%  # NA to 0
  glimpse()

yob = milestone_nppes_ama_abs %>% 
  filter(nppes_unique_match == 0, !PersonID %in% yob_yog$PersonID) %>% 
  filter(yob_ms_ama == 1 ,
         yog_ms_abs == 0)  %>% 
  add_count(PersonID) %>% 
  filter(n == 1) %>% 
  select(-n) %>% 
  glimpse()

n_distinct(yob$PersonID)
# 39

# diff yob, same yog
yog = milestone_nppes_ama_abs %>% 
  filter(nppes_unique_match == 0, !PersonID %in% yob_yog$PersonID) %>% 
  filter(yob_ms_ama == 0 ,
         yog_ms_abs == 1)  %>% 
  add_count(PersonID) %>% 
  filter(n == 1) %>% 
  select(-n) %>% 
  glimpse()

n_distinct(yog$PersonID)
# 116

  
# if both abs and AMA have match, we can use names from the 3 data sources to make decision
multi_yog_yob = inner_join(yob, yog, by = "PersonID") %>% 
  pull(PersonID)

qa_multi_abs = milestone_nppes_ama_abs %>% 
  filter(PersonID %in% multi_yog_yob) %>% 
  filter(last_name_abs == last_name,
         first_name_abs == first_name) %>% 
  glimpse()

milestone_nppes_ama_abs %>% 
  filter(PersonID %in% multi_yog_yob) %>% 
  filter(last_name_ama == last_name,
         first_name_ama == first_name) %>% 
  glimpse()

# From the test data, ABS has the more accurate NPI

# if ABS or AMA don't agree, we choose ABS after I cherry picked to check cases
test = milestone_nppes_ama_abs %>% 
  filter(PersonID == "383547")


# no match by AMA or abs
milestone_nppes_ama_abs %>% 
  distinct(PersonID, nppes_unique_match, md_do_nppes) %>% 
  filter(nppes_unique_match == 0, is.na(md_do_nppes))

no_match_ama_abs = milestone_nppes_ama_abs %>% 
  filter(nppes_unique_match == 0) %>% 
  filter(!is.na(md_do_nppes)) %>%  #151
  filter(!PersonID %in% yob_yog$PersonID) %>% # 825
  filter(!PersonID %in% yob$PersonID,
         !PersonID %in% yog$PersonID) %>%
  distinct(PersonID)


# use matched info to get NPI for personID
npi_nppes_uniq = nppes_uniq %>% 
  distinct(PersonID, NPI.nppes)

npi_yob_yog = yob_yog %>% 
  distinct(PersonID, NPI.nppes)

npi_yob = yob %>% 
  distinct(PersonID, NPI.nppes) %>% 
  filter(!PersonID %in% multi_yog_yob)  # keep yog 

npi_yog = yog %>% 
  distinct(PersonID, NPI.nppes) 

linked_npi = rbind(npi_nppes_uniq, npi_yob_yog, npi_yob, npi_yog)
  
# check
linked_npi %>% 
  add_count(PersonID) %>% 
  filter(n>1)



milestone_linked = milestone %>% 
  left_join(linked_npi, by= "PersonID") %>% 
  select(PersonID, NPI, NPI.nppes, everything())


milestone_linked %>% 
  filter(is.na(NPI), is.na(NPI.nppes))

diff_npi_check = milestone_linked %>% 
  filter(NPI != NPI.nppes) %>% 
  left_join(nppes, by = c("NPI")) %>% 
  select(PersonID, NPI, NPI.nppes, contains("_name"))

# 28
  

