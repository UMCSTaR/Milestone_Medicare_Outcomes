library(mixedup)
library(tidyverse)
library(glmmTMB)
library(flextable)


# setup -------------------------------------------------------------------

source("/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/code/functions/fe.r")

kable_df <- function(..., digits=2) {
  kable(...,digits = digits, booktabs = T) %>% 
    kable_styling(full_width = F,
                  latex_options = c("striped", "hold_position"))
  # kable_styling(full_width = F)
}


# load_data ---------------------------------------------------------------

# cohort
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/milestone_medicare_pc_primary.rdata")
# model
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc.rdata")


# or table --------------------------------------------------------------
names = names(results)
bin = str_subset(names, "8")
cont = names[!names %in% bin]
any_cmp = str_subset(names, "any_cmp")

or_all <- function(model, rating_var) {
  rating_var = enquo(rating_var)
  
  fe(model, pdf = FALSE) %>% 
    mutate_if(is.double, ~round(.x, 1)) %>% 
    mutate(!!rating_var := paste0(OR, " (", OR_lower, "-", OR_upper, ")")) %>% 
    select(`Fixed Effects`, !!rating_var)
}

bin_or_list = imap(results[bin], or_all)

bin_or = reduce(bin_or_list, full_join, by = "Fixed Effects") %>% 
  rename(Characteristic = "Fixed Effects")

# ---
any_cmp_list = imap(results[any_cmp], or_all)

any_cmp = reduce(any_cmp_list, full_join, by = "Fixed Effects") %>% 
  rename(Characteristic = "Fixed Effects")


## any complication-----
bin_or %>% 
  select(Characteristic, contains("any_cmp")) %>%
  rename("Overall Mean" = Par_any_cmp_mean_ge_8,
         Professional = Par_any_cmp_prof_ge8,
         Operative = Par_any_cmp_operative_ge8,
         Leadership = Par_any_cmp_leadership_ge8) %>% 
  mutate_all(~replace_na(.,"...")) %>% 
  slice(1, 14:16, 2:13) %>% 
  flextable() %>% 
  add_header_lines(values =c("Odds Ratio (95% Confidence Interval)",
                             "Any complication regression tables per Milestone rating")) %>% 
  theme_box() %>%
  autofit() %>% 
  save_as_docx(path= "reports/sup_tbl_any_cmp.docx")
  

