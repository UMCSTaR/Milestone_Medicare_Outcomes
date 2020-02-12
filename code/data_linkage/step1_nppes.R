library(tidyverse)

load("/Volumes/George_Surgeon_Projects/ACGME_milestone/milestone_person_info.rdata")
# nppes
load("/Volumes/George_Surgeon_Projects/Other/NPPES_Data_Dissemination_January_2020/npidata_pfile_2020_selected_var.rdata")


npidata_pfile_2020_selected_var = npidata_pfile_2020_selected_var %>% 
  mutate(NPI = as.character(NPI))

milestone_person_nppes = milestone_person_info %>% 
  left_join(npidata_pfile_2020_selected_var)

# check
milestone_person_nppes %>% 
  filter(!is.na(NPI), is.na(first_name)) %>% 
  glimpse()
# every npi from milestone is connect to NPPES, some are deactived or no NPI

save(milestone_person_nppes, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/milestone_person_nppes.rdata")

glimpse(milestone_person_nppes)

# entity type
milestone_person_nppes %>% 
  filter(!is.na(NPI)) %>% 
  count(`Entity Type Code`)

# `Entity Type Code`     n
# <dbl> <int>
#   1                  1  3091
# 2                  2     3
# 3                 NA     2



# Reverse Check with NPPES ---------
library(tidyverse)

# load miletone data with names
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/milestone_person_nppes.rdata")

milestone_person_nppes = milestone_person_nppes %>% 
  rename(npi_ms = NPI) # renanme milestone NPI to diff with NPPES NPI

# load NPPES with DO or MD 
load("/Volumes/George_Surgeon_Projects/Other/NPPES_Data_Dissemination_January_2020/npidata_pfile_2020_selected_var_md_do_na.rdata")

npidata_pfile_2020_selected_var_md_do_na = npidata_pfile_2020_selected_var_md_do_na %>% 
  mutate(md_do_nppes = 1) # add flag to indicate data from nppes

# milestone names left join with nppes names
milestone_person_nppes_match = milestone_person_nppes %>% 
  left_join(npidata_pfile_2020_selected_var_md_do_na, 
            by = c("first_name", "last_name", "middle_name"))

# one milestone with multiple match NPPES -----
multi_nppes = milestone_person_nppes_match %>% 
  filter(!is.na(NPI)) %>% 
  add_count(PersonID) %>% 
  filter(n>1) %>% 
  select(-n)

n_distinct(multi_nppes$PersonID)  #223
save(multi_nppes, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/step1_nppes/multi_nppes.rdata")

#  non-match -------
non_matched_nppes = milestone_person_nppes_match %>% 
  filter(is.na(NPI), !is.na(npi_ms)) 

n_distinct(non_matched_nppes$PersonID)  #35
save(non_matched_nppes, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/step1_nppes/non_match.rdata")

# unique macthed to check against milestone ------
matched_nppes = milestone_person_nppes_match %>% 
  filter(!is.na(NPI)) %>% 
  add_count(PersonID) %>% 
  filter(n==1) %>% 
  select(-n)

save(matched_nppes, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/step1_nppes/matched_nppes.rdata")

# QA --------
matched_nppes %>% 
  mutate(nppes_miles_npi = ifelse(npi_ms == NPI, "same", "diff")) %>% 
  tidyext::cat_by(nppes_miles_npi)
# 0.1% miss

npi_diff = milestone_person_nppes_match %>% 
  filter(!is.na(NPI)) %>% 
  add_count(PersonID) %>% 
  filter(n==1) %>% 
  mutate(nppes_miles_npi = ifelse(npi_ms == NPI, "same", "diff")) %>% 
  filter(nppes_miles_npi == "diff") %>% 
  select(PersonID, npi_ms, NPI, contains("name"))

npi_ms_diff = unique(npi_diff$npi_ms)

load("/Volumes/George_Surgeon_Projects/Other/NPPES_Data_Dissemination_January_2020/npidata_pfile_2020_selected_var.rdata")
qa = npidata_pfile_2020_selected_var %>% 
  mutate(NPI = as.character(NPI)) %>% 
  filter(NPI %in% npi_ms_diff) %>% 
  glimpse()

# based on qa, the differences are from the `Provider Credential Text`, and deactive NPI
# NPI from Milestones are non MD or DO.
