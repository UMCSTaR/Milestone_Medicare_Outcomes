library(mixedup)
library(tidyverse)
library(glmmTMB)
library(flextable)


# 1. setup -------------------------------------------------------------------

source("/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/code/functions/fe.r")

kable_df <- function(..., digits=2) {
  kable(...,digits = digits, booktabs = T) %>% 
    kable_styling(full_width = F,
                  latex_options = c("striped", "hold_position"))
  # kable_styling(full_width = F)
}


# 2. load_data ---------------------------------------------------------------

# cohort
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_pc_primary.rdata")
# model
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc.rdata")


# 3. or table --------------------------------------------------------------
names = names(results)
bin = str_subset(names, "8")
cont = names[!names %in% bin]

or_all <- function(model, rating_var) {
  rating_var = enquo(rating_var)
  
  fe(model, pdf = FALSE) %>% 
    mutate_if(is.double, ~round(.x, 2)) %>% 
    mutate(!!rating_var := paste0(OR, " (", OR_lower, "-", OR_upper, ")")) %>% 
    select(`Fixed Effects`, !!rating_var)
}

## binary -----
bin_or_list = imap(results[bin], or_all)

bin_or = reduce(bin_or_list, full_join, by = "Fixed Effects") 
  # rename(Characteristic = "Fixed Effects")


newnames = c("Characteristic","Overall Mean", "Professional",
             "Operative",
             "Leadership")



## any complication-----
bin_or %>% 
  select("Fixed Effects", contains("any_cmp")) %>% 
  rename_with(~newnames) %>% 
  mutate_all(~replace_na(.,"...")) %>% 
  slice(1, 14:16, 2:13) %>% 
  mutate(Characteristic = case_when(Characteristic == "mean_ge_8" ~ "Overall Mean",
                                    Characteristic == "prof_rating_ge8" ~ "Professional",
                                    Characteristic == "operative_rating_ge8" ~ "Operative",
                                    Characteristic == "leadership_rating_ge8" ~ "Leadership",
                                    Characteristic == "flg_male" ~ "Patient gender (Male)",
                                    Characteristic == "age_at_admit_scale" ~ "Patient age, y",
                                    Characteristic == "race_white" ~ "Patient Race (White)",
                                    Characteristic == "emergent_admission" ~ "Emergent Admission",
                                    Characteristic == "comorbidity_score" ~ "Comorbidity Score",
                                    Characteristic == "claim_year" ~ "Claim year",
                                    Characteristic == "had_assist_surg" ~ "Presence of assistant surgeon",
                                    Characteristic == "cases_per_12month" ~ "Surgeon case volume per year",
                                    Characteristic == "hosp_beds(>=350)" ~ "Hospital bed size (≥350)",
                                    Characteristic == "hosp_mcday2inptday_ratio" ~ "Hospital medicare to inpatient days ratio",
                                    Characteristic == "hosp_rn2bed_ratio" ~ "Hospital registered nurse to bed ratio",
                                    TRUE ~ Characteristic
                                    )) %>% 
  flextable() %>% 
  add_header_lines(values =c("Odds Ratio (95% Confidence Interval)",
                             "Any complication regression tables per Milestone rating")) %>% 
  theme_box() %>%
  autofit() %>% 
  save_as_docx(path= "reports/sup_tbl_any_cmp.docx")
  

