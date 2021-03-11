library(tidyverse)
library(glmmTMB)
library(mixedup)
library(kableExtra)

# binary rating s=9 cutoff models
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc_cutoff9.rdata")


model_tables = map_df(results, extract_fixed_effects, .id = "outcome")

bin_rating = model_tables  %>%
  mutate(
    OR = exp(value),
    OR_lower = exp(lower_2.5),
    OR_upper = exp(upper_97.5),
    outcome = str_extract(outcome, "severe_cmp|any_cmp|readmit|death")
  ) %>% 
  filter(str_detect(term, "eql9")) %>% 
  select(outcome, term, coef = value, contains("OR"), p_value)

bin_rating %>% 
  kable(digits = 2, caption = "Binary ratings (=9) model results") %>% 
  kable_styling(full_width = F) %>% 
  collapse_rows(columns = 1, valign = "top") %>% 
  save_kable("images/model_cutoff9_coef_summary.png")

