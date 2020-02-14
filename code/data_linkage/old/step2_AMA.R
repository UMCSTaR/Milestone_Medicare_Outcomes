# Match Milestones with AMA

library(tidyverse)
# load("/Volumes/AMA_Masterfile/Data/ama.rdata")
# ama_select = ama %>% 
#   select(ResearchID, npi, contains("Name"), BirthDate, MedSchoolID)
# # renname vars
# ama_select = ama_select %>% 
#   rename(first_name_ama = MailFirstName,
#          last_name_ama = MailLastName,
#          middle_name_ama = MailMiddleName)
# save(ama_select, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/ama_select.rdata")

# load ama selected vars
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/ama_select.rdata")
# not unique macth
load("/Volumes/George_Surgeon_Projects/ACGME_milestone/step1_nppes/multi_nppes.rdata")
n_distinct(multi_nppes$PersonID) # 144

# ama ----------
# npi summary for AMA
ama_select %>% 
  summarise(n_tot = n_distinct(ResearchID), 
            n_npi = n_distinct(npi, na.rm = T))
      
# ama rename
str(ama_select)

# add birth_year and format names, add ama flag
ama_select = ama_select %>% 
  mutate(last_name_ama = str_to_title(last_name_ama),
         first_name_ama = str_to_title(first_name_ama),
         middle_name_ama =str_to_title(middle_name_ama),
         birth_year = lubridate::year(BirthDate),
         ama = 1) %>% 
  as_tibble()

str(ama_select)


# milestone data -------------
str(multi_nppes)

# add birth_year and format names
multi_nppes = multi_nppes %>% 
  mutate(last_name = str_to_title(last_name),
         first_name = str_to_title(first_name),
         middle_name =str_to_title(middle_name),
         NPI = as.character(NPI),
         birth_year = lubridate::year(Birth_date))
  
# milestone to AMA by NPI ------
multi_nppes_ama = multi_nppes %>% 
  left_join(ama_select, by = c("NPI" = "npi")) %>% 
  mutate(birth_ms_ama = ifelse(birth_year.x == birth_year.y, 1, 0))        # flag milestone YOB match with ama npi YOB

# nppes ama matched with YOB
nppes_ama_npi_matched = multi_nppes_ama %>% 
  filter(birth_ms_ama == 1) %>% 
  add_count(PersonID) %>% 
  filter(n == 1) %>% 
  select(PersonID, npi_ms, NPI) 

n_distinct(nppes_ama_npi_matched$PersonID) #120

save(nppes_ama_npi_matched, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/step2_ama/nppes_ama_npi_matched.rdata")

# nppes ama non-match -------
nppes_ama_npi_no_match = multi_nppes_ama %>% 
  filter(!PersonID %in% nppes_ama_npi_matched$PersonID) %>% 
  select(PersonID, npi_ms, NPI, birth_year.x, birth_year.y, contains("name")) 

n_distinct(nppes_ama_npi_no_match$PersonID) #24

save(nppes_ama_npi_no_match, file = "/Volumes/George_Surgeon_Projects/ACGME_milestone/step2_ama/nppes_ama_npi_no_match.rdata")


