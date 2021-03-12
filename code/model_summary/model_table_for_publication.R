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
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc_no_case_vol.rdata")


# 3. or table --------------------------------------------------------------
names = names(results)
bin = str_subset(names, "8")
cont = names[!names %in% bin]

or_all <- function(model, rating_var) {
  rating_var_quo = enquo(rating_var)
  rating_var_p = paste0(rating_var, "_p")
  
  
  fe(model, pdf = FALSE) %>% 
    mutate_if(is.double, ~round(.x, 2)) %>% 
    mutate(!!rating_var_quo := paste0(OR, " (", OR_lower, "-", OR_upper, ")"),
           `Fixed Effects` = ifelse(`Fixed Effects` %in% c("mean_ge_8",
                                                            "prof_rating_ge8",
                                                            "operative_rating_ge8",
                                                            "leadership_rating_ge8"),
                                     "milestone_rating", `Fixed Effects`),
           {{rating_var_p}} := p_value) %>% 
    select(`Fixed Effects`, !!rating_var_quo, {{rating_var_p}})
}

## binary -----
bin_or_list = imap(results[bin], or_all)

bin_or = reduce(bin_or_list, full_join, by = "Fixed Effects") 
  # rename(Characteristic = "Fixed Effects")


newnames = c("Characteristic","Overall Mean", "Overall Mean P",
             "Professional",
             "Professional P",
             "Operative",
             "Operative P",
             "Leadership",
             "Leadership P")



## any complication-----
bin_or %>% 
  select("Fixed Effects", contains("any_cmp")) %>% 
  rename_with(~newnames) %>% 

  
  # slice(1, 14:16, 2:13) %>% 
  mutate(Characteristic = case_when(Characteristic == "flg_male" ~ "Male Gender",
                                    Characteristic == "age_at_admit_scale" ~ "Mean Age on admission, yr",
                                    Characteristic == "race_white" ~ "Race - White",
                                    Characteristic == "SES" ~ "High SES",
                                    Characteristic == "emergent_admission" ~ "Emergency Admission Status",
                                    Characteristic == "comorbidity_score" ~ "Mean AHRQ Elixhauser Index",
                                    Characteristic == "claim_year" ~ "Claim year",
                                    Characteristic == "had_assist_surg" ~ "Presence of assistant surgeon",
                                    Characteristic == "cases_per_12month" ~ "Surgeon case volume per year",
                                    Characteristic == "hosp_beds(>=350)" ~ "Hospital Beds(≥350)",
                                    Characteristic == "had_assist_surg" ~ "Presence of Assisting Surgeon",
                                    Characteristic == "hosp_mcday2inptday_ratio" ~ "Facility Medicaid/Facility inpatient days ratio",
                                    Characteristic == "hosp_rn2bed_ratio" ~ "Registered nurse to bed ratio",
                                    TRUE ~ Characteristic
                                    )) %>% 
  flextable() %>% 
  add_header_lines(values =c("Odds Ratio (95% Confidence Interval)",
                             "Any complication regression tables per Milestone rating")) %>% 
  theme_box() %>%
  autofit() %>% 
  save_as_docx(path= "reports/any_cmp_regression_table.docx")
  

