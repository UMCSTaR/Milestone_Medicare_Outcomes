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

# ama ----------
# npi summary for AMA
ama_select %>% 
  summarise(n_tot = n_distinct(ResearchID), 
            n_npi = n_distinct(npi, na.rm = T))
      
# ama rename
str(ama_select)

ama_select = ama_select %>% 
  mutate(last_name_ama = str_to_title(last_name_ama),
         first_name_ama = str_to_title(first_name_ama),
         middle_name_ama =str_to_title(middle_name_ama),
         birth_year = lubridate::year(BirthDate)) %>% 
  as_tibble()

# milestone data -------------
str(multi_nppes)

multi_nppes = multi_nppes %>% 
  mutate(last_name = str_to_title(last_name),
         first_name = str_to_title(first_name),
         middle_name =str_to_title(middle_name))
  
# milestone to AMA
multi_nppes %>% 
  left_join(ama_select, by = c("NPI" = "npi"))

ama_select %>% 
  filter(is.na(birth_year))

# matched surgeons
milestone_person_nppes_ama %>% 
  summarise(tot = n(),
            no_ama = sum(is.na(ResearchID)))

milestone_person_nppes_ama %>% 
  add_count(PersonID) %>% 
  filter(n>1)

test = milestone_person_nppes_ama %>% 
  add_count(PersonID) %>% 
  filter(n == 1) %>% 
  filter(!is.na(NPI), npi != "NA") %>% 
  mutate(no_match_npi = ifelse(npi == NPI, "macthed", "non-matched")) %>% 
  count(no_match_npi)


test = milestone_person_nppes_ama %>% 
  filter(!is.na(`Provider Credential Text`)) %>% 
  count(`Provider Credential Text`, sort = T) 


# medschool ID is different between AMA and Milestone
milestone_medschool = milestone %>% 
  filter(!is.na(PersonID)) %>% 
  distinct(PersonID, MedicalSchoolID)

milestone_ama %>% 
  left_join(milestone_medschool) %>% 
  select(PersonID, MedicalSchoolID, MedSchoolID) %>% 
  mutate(same_id = ifelse(MedSchoolID == MedicalSchoolID, 1, 0)) %>% 
  count(same_id)


str(Physician_Compare_National_Downloadable_File)
