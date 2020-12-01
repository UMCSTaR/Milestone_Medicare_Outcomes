mov = milestone_medicare_pc %>% 
  summarise(n_case = n(),
            n_surgeon = length(unique(id_physician_npi)),
            multi_surg = mean(flg_multi_surgeon),
            assist_surg = mean(flg_assistant_surgeon)) %>% 
  mutate(cohort = "MvO") %>% 
  select(cohort, everything())

load("~/Documents/Repo/QE_CE_Outcome/data/assist_multi_tble.rdata")
assist_multi_tble %>% 
  mutate(cohort = "QE CE") %>% 
  select(cohort, everything()) %>% 
  rename(assist_surg = assit_surg) %>% 
  full_join(mov) %>% 
  # mutate(assist_surg = scales::percent(assist_surg))
  mutate(across(ends_with("_surg"), scales::percent))

         