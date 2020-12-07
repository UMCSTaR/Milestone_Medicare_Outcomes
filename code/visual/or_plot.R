# OR plot and predicted probability plots
# 2 by 2 or
library(tidyverse)
library(glmmTMB)
library(mixedup)
library(purrr)
library(kableExtra)
library(patchwork)


my_color = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")

source("code/functions/or_plot.R")

# load 24 months ------
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc.rdata")

# 1. OR ----------------------------------------------------------------------
# ratings by category
model_tables = map_df(results, extract_fixed_effects, .id = "outcome")

# process
model_tables = model_tables %>%
  mutate(
    OR = exp(value),
    OR_lower = exp(lower_2.5),
    OR_upper = exp(upper_97.5)
  ) %>%
  mutate(outcome = str_remove_all(outcome, pattern = c("Par_|_all_mean|_prof|_leadership|_operative|_mean_ge_8|_ge8|_never_less_8")),
         outcome = factor(outcome,
                          levels = c(
                            "death", "severe_cmp","any_cmp", "readmit"
                          ))
         ) %>%
  select(outcome, term, OR, contains("OR"), p_value) 

# extract mean milestone coef 
mean_rating = model_tables  %>%
  filter(str_detect(term, "_mean")) %>% 
  mutate(term = case_when(term == "IntResponseValue_mean" ~ "overall_mean",
                          term == "prof_rating_mean" ~ "professional",
                          term == "operative_rating_mean" ~ "operative",
                          term == "leadership_rating_mean" ~ "leadership"))

# extract binary milestone coef 
bin_rating = model_tables  %>%
  filter(str_detect(term, "ge8|ge_8")) %>% 
  mutate(term = case_when(term == "mean_ge_8" ~ "overall_mean",
                          term == "prof_rating_ge8" ~ "professional",
                          term == "operative_rating_ge8" ~ "operative",
                          term == "leadership_rating_ge8" ~ "leadership"))



# or table ------
mean_model_summary = mean_rating %>% 
  mutate_if(is.numeric,~round(.,2)) %>% 
  mutate(`95% CI` = paste0("[", OR_lower, ", ", OR_upper, "]")) %>% 
  select(outcome, rating = term, OR, `95% CI`, p_value) 

mean_model_summary %>% 
  kable(digits = 2, caption = "Odds Ratios of Milestone Scores (mean)") %>% 
  kable_styling(full_width = F) %>% 
  collapse_rows(columns = 1, valign = "top") 

bin_model_summary = bin_rating %>% 
  mutate_if(is.numeric,~round(.,2)) %>% 
  mutate(`95% CI` = paste0("[", OR_lower, ", ", OR_upper, "]")) %>% 
  select(outcome, rating = term, OR, `95% CI`, p_value) 

bin_model_summary %>% 
  kable(digits = 2, caption = "Odds Ratios of Milestone Scores (ge 8)") %>% 
  kable_styling(full_width = F) %>% 
  collapse_rows(columns = 1, valign = "top") 


# plot by milestone category-------
# continuous milestone
or_by_mean_milestone = split(mean_rating,mean_rating$term)

or_plot_mean = imap(or_by_mean_milestone, or_plot)

# make 2X2 plot
(or_plot_mean$overall_mean | or_plot_mean$operative) /
  (or_plot_mean$leadership | or_plot_mean$professional) +
  plot_annotation(
    title = 'Mean milestone ratings by patient outcomes')
  
ggsave("images/or_2by2_mean_rating.png")


# bin miletone
or_by_bin_milestone = split(bin_rating,bin_rating$term)

or_plot_bin = imap(or_by_bin_milestone, or_plot)

# make 2X2 plot
(or_plot_bin$overall_mean | or_plot_bin$operative) /
  (or_plot_bin$leadership | or_plot_bin$professional) +
  plot_annotation(
    title = 'binary (≥8) milestone ratings by patient outcomes')

ggsave("images/or_2by2_bin_rating.png")




