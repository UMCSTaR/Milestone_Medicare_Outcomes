# OR plot and predicted probability plots
# 2 by 2 or-----
library(tidyverse)
library(glmmTMB)
library(mixedup)
library(kableExtra)
library(ggeffects)
# library(ggpattern)
library(flextable)
library(glue)


my_color = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")
# my_color = scico::scico(4, palette = "batlow", end = 0.7)
including_case_vol = FALSE # whether using model has case volume as covar

source("~/Documents/Repo/Milestone_Medicare_Outcomes/code/functions/clean_or_table_lables.R")
source("~/Documents/Repo/Milestone_Medicare_Outcomes/code/functions/or_plot.R")
source("~/Documents/Repo/Milestone_Medicare_Outcomes/code/functions/predicted_probs_all_models.R")


# load 24 months ------
if (including_case_vol == TRUE){
  load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc.rdata")
} else {
  load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc_no_case_vol.rdata")
}

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
## mean-----
mean_model_summary = mean_rating %>% 
  mutate_if(is.numeric,~round(.,2)) %>% 
  mutate(`95% CI` = paste0("(", OR_lower, ", ", OR_upper, ")")) %>% 
  select(outcome, rating = term, OR, `95% CI`, p_value) 

# change labels
mean_model_summary %>%
  clean_or_table_labels() %>%
  kable(digits = 2, caption = glue("Odds Ratios of Milestone Scores (mean ratings)
        including case volume == {including_case_vol}")) %>%
  kable_styling(full_width = F) %>%
  collapse_rows(columns = 1, valign = "top") 
  # save_kable("images/mean_model_summary.png") 
  # save_kable("/Volumes/GoogleDrive/My Drive/EQUIP Lab- Documents/Active Projects/2020.01 Milestone vs. Medicare Outcomes-Dan/Visualizations/mean_or_summary.png")
  

## binary----
bin_model_summary = bin_rating %>% 
  mutate_if(is.numeric,~round(.,2)) %>% 
  mutate(`95% CI` = paste0("(", OR_lower, ", ", OR_upper, ")")) %>% 
  select(outcome, rating = term, OR, `95% CI`, p_value) 

bin_model_summary %>% 
  clean_or_table_labels() %>%
  kable(digits = 2, caption = glue("Odds Ratios of Milestone Scores (<8 vs. ≥8 )
        including case volume == {including_case_vol}")) %>% 
  kable_styling(full_width = F) %>% 
  collapse_rows(columns = 1, valign = "top") 
  # save_kable("images/bin_model_summary.png")
  # save_kable("/Volumes/GoogleDrive/My Drive/EQUIP Lab- Documents/Active Projects/2020.01 Milestone vs. Medicare Outcomes-Dan/Visualizations/bin_or_summary.png")
  
## excel output----
mean_tbl = mean_model_summary %>%
  clean_or_table_labels() %>% 
  rename(OR = "Odds ratio") %>% 
  mutate(`OR(95% CI)` = paste0(OR, `95% CI`)) %>% 
  select(Rating, `Patient outcomes`, `OR(95% CI)`, `p-value`) 

bin_tbl = bin_model_summary %>%
  clean_or_table_labels() %>% 
  rename(OR = "Odds ratio") %>% 
  mutate(`OR(95% CI)` = paste0(OR, `95% CI`)) %>% 
  select(Rating, `Patient outcomes`, `OR(95% CI)`, `p-value`) 
  

two_tbl = full_join(mean_tbl, bin_tbl, by = c("Rating", "Patient outcomes"))

or_tbl = two_tbl %>% 
  mutate(Rating = factor(Rating, levels = c("Overall", "Leadership", "Operative", "Professional"))) %>% 
  arrange(Rating) %>% 
  flextable() %>% 
  merge_v(j = "Rating") %>%
  add_header('OR(95% CI).x' = "Continuous",
             'p-value.x' = "Continuous",
             'OR(95% CI).y' = "Binary",
             'p-value.y' = "Binary",
             top = TRUE ) %>% 
  merge_h(part = "header") %>% 
  theme_box() %>% 
  width(width = 1) %>% 
  add_header_lines(values = glue("model including case volume as covariate = {including_case_vol}")) %>% 
  autofit() 

if (including_case_vol == TRUE){
  save_as_docx(or_tbl, path = "reports/or_table_inlcude_case_vol.docx")
} else {
  save_as_docx(or_tbl, path = "reports/or_table_no_case_vol.docx")
}

# # archive plot -------
# # continuous milestone
# or_plot(data = mean_rating,
#         title = "Mean milestone ratings by patient outcomes") +
#   facet_wrap(~term)
#   
# ggsave("images/or_2by2_mean_rating.png")
# ggsave("/Volumes/GoogleDrive/My Drive/EQUIP Lab- Documents/Active Projects/2020.01 Milestone vs. Medicare Outcomes-Dan/Visualizations/or_2by2_mean_rating.svg")

# bin miletone
# or_plot(data = bin_rating,
#         title = "binary (≥8) milestone ratings by patient outcomes") +
#   facet_wrap(~term)
# 
# ggsave("images/or_2by2_bin_rating.png")


# 2. probability 2 by 2 ------------------------------------------------------
model_name_list = c('mean_ge_8', "prof_ge8", "operative_ge8", "leadership_ge8")
rating_name_list = c("mean_ge_8", "prof_rating_ge8", "operative_rating_ge8", "leadership_rating_ge8")
term_name_list = c("overall_mean", "leadership", "operative", "professional")

prob_list = list(model_name_list, rating_name_list, term_name_list)
# get pred probs for 32 models
bin_mean_pred = pmap_df(prob_list, predicted_probs_all_models)

# process data: label, levels
bin_mean_pred_tbl <-  bin_mean_pred %>% 
  mutate_at(c("predicted", "conf.low", "conf.high"), ~ round(., 2)) %>%
  select(-std.error,-group) %>% 
  mutate(outcome = str_remove_all(outcome, pattern = c("Par_|_mean_ge_8|_prof_ge8|_leadership_ge8|_operative_ge8")),
         x = ifelse(x ==1, ">=8", "<8")) %>% 
  rename(binary_rating = x) %>% 
  mutate(
    outcome = factor(outcome,
                     levels = c(
                       "death", "severe_cmp","any_cmp", "readmit"
                     )),
    term = factor(
      term,
      levels = c("overall_mean", "leadership", "operative", "professional")
    )
  )

# table -----
bin_mean_pred_tbl %>% 
  mutate(`95% CI` = paste0("(", conf.low, ", ", conf.high, ")")) %>% 
  select(term, everything(), -contains(".")) %>% 
  kable(digits = 2, caption = "Predicted Probabilities of Binay Milestone Scores") %>% 
  kable_styling(full_width = F) %>% 
  collapse_rows(columns = 1:2, valign = "top") 
  # save_kable("images/pred_probs.png")

# plot----
bin_mean_pred_tbl %>%
  mutate(
    outcome = case_when(
      outcome == "any_cmp" ~ "Any complication",
      outcome == "readmit" ~ "Readmission",
      outcome == "severe_cmp" ~ "Severe complication",
      outcome == "death" ~ "Death"
    ),
    outcome = factor(outcome, levels = c(
      "Any complication", "Readmission", "Severe complication", "Death"
    )),
    binary_rating = ifelse(binary_rating == ">=8", "Proficient",
                           "Not Yet Proficient"),
    term = str_to_title(term),
    term = ifelse(term == "Overall_mean", "Overall", term),
    term = factor(term, levels = c(
      "Overall", "Leadership", "Operative", "Professional"
    )),
  ) %>% 
  ggplot(aes(x = outcome, y = predicted)) +
  facet_wrap(~term, ncol =2, scales = "free") +
  # different patterns to indicate binary scale
  # geom_col_pattern(aes(fill = outcome,
  #                      pattern_fill = binary_rating,
  #                      # pattern_type = binary_rating,
  #                      pattern_density = binary_rating),
  #          position = position_dodge(width = 0.55), width = 0.4,
  #          colour = 'gray',
  #          pattern  = 'stripe',
  #          pattern_fill    = 'gray',
  #          pattern_colour  = 'gray'
  #          ) +
geom_col(
  aes(
    fill = binary_rating,
  ),
  position = position_dodge(width = 0.55), width = 0.4
) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, group = binary_rating),
                alpha = 0.7, position = position_dodge(width = 0.55),
                width = .2, show.legend = FALSE) +
  # different symbol to indicate binary scale
  # geom_col(aes(fill = outcome, group = binary_rating),
  #        position = position_dodge(width = 0.55), width = 0.5,
  #        show.legend = FALSE) +
  # geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = binary_rating,
  #                     shape = binary_rating),
  #                 alpha = 0.7, position = position_dodge(width = 0.55)) +
  # scale_color_manual(values = my_color, aesthetics = c("fill")) +
  scale_color_manual(values = c("gray40", "gray85"), aesthetics = c("fill")) +
  labs(
    # title = "Predicted probabilities of patient outcomes by milestone ratings",
       y = "Estimated probabilities of complications",
       x = "",
       caption = "* indicates the milestone rating coeffcient was statistically significant") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,0.6),
                     n.breaks = 5) +
  # scale_fill_grey() +
  visibly::theme_trueMinimal(center_axis_labels = T) +
  theme(axis.line.x = element_line(colour="black"),
        axis.line.y = element_line(colour="black"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        # axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle = 90, vjust = 0.5, size = 13),
        axis.ticks.y=element_blank(),
        strip.text = element_text(size = 13),
        legend.position="bottom",
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_blank(),
        plot.caption = element_text(size = 10)) 
  
if(including_case_vol == TRUE) {
  ggsave("~/Documents/Repo/Milestone_Medicare_Outcomes/images/prob_2by2_binary_rating_gray_include_case_vol.png")
  ggsave("/Volumes/GoogleDrive/My Drive/MED Lab- Documents/Active Projects/2020.01 Milestone vs. Medicare Outcomes-Dan/Visualizations/prob_2by2_binary_rating_include_case_vol.png")
} else {
  ggsave("~/Documents/Repo/Milestone_Medicare_Outcomes/images/prob_2by2_binary_rating_gray_include_no_case_vol.png")
  ggsave("/Volumes/GoogleDrive/My Drive/MED Lab- Documents/Active Projects/2020.01 Milestone vs. Medicare Outcomes-Dan/Visualizations/prob_2by2_binary_rating_include_no_case_vol.png")
}

