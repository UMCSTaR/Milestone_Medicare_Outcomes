# OR plot and predicted probability plots
# 2 by 2 or-----
library(tidyverse)
library(glmmTMB)
library(mixedup)
library(kableExtra)
library(ggeffects)


my_color = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")
my_color32 = rep(c(rep("#66c2a5",2), 
             rep("#fc8d62",2), 
             rep("#8da0cb",2), 
             rep("#e78ac3",2)), 4)
source("code/functions/or_plot.R")
source("code/functions/predicted_probs_all_models.R")

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
  collapse_rows(columns = 1, valign = "top") %>% 
  # save_kable("data/mean_model_summary.png") %>% 
  save_kable("/Volumes/GoogleDrive/My Drive/EQUIP Lab- Documents/Active Projects/2020.01 Milestone vs. Medicare Outcomes-Dan/Visualizations/mean_or_summary.png")


bin_model_summary = bin_rating %>% 
  mutate_if(is.numeric,~round(.,2)) %>% 
  mutate(`95% CI` = paste0("[", OR_lower, ", ", OR_upper, "]")) %>% 
  select(outcome, rating = term, OR, `95% CI`, p_value) 

bin_model_summary %>% 
  kable(digits = 2, caption = "Odds Ratios of Milestone Scores (ge 8)") %>% 
  kable_styling(full_width = F) %>% 
  collapse_rows(columns = 1, valign = "top") %>% 
  # save_kable("data/bin_model_summary.png")
  save_kable("/Volumes/GoogleDrive/My Drive/EQUIP Lab- Documents/Active Projects/2020.01 Milestone vs. Medicare Outcomes-Dan/Visualizations/bin_or_summary.png")
  

# plot -------
# continuous milestone
or_plot(data = mean_rating,
        title = "Mean milestone ratings by patient outcomes") +
  facet_wrap(~term)
  
ggsave("images/or_2by2_mean_rating.png")
ggsave("/Volumes/GoogleDrive/My Drive/EQUIP Lab- Documents/Active Projects/2020.01 Milestone vs. Medicare Outcomes-Dan/Visualizations/or_2by2_mean_rating.svg")



# bin miletone
or_plot(data = bin_rating,
        title = "binary (≥8) milestone ratings by patient outcomes") +
  facet_wrap(~term)

ggsave("images/or_2by2_bin_rating.png")


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

bin_mean_pred_tbl %>% 
  mutate(`95% CI` = paste0("[", conf.low, ", ", conf.high, "]")) %>% 
  select(term, everything(), -contains(".")) %>% 
  kable(digits = 2, caption = "Predicted Probabilities of Binay Milestone Scores") %>% 
  kable_styling(full_width = F) %>% 
  collapse_rows(columns = 1:2, valign = "top") %>% 
  save_kable("images/pred_probs.png")

# plot----
bin_mean_pred_tbl %>%
  ggplot(aes(x = outcome, y = predicted)) +
  facet_wrap(~term, ncol =2) +
  geom_col(aes(fill = outcome, group = binary_rating),
           position = position_dodge(width = 0.55), width = 0.5,
           show.legend = FALSE) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = binary_rating,
                      shape = binary_rating), 
                alpha = 0.7, position = position_dodge(width = 0.55)) +
  
  # geom_errorbar(aes(ymin = conf.low, ymax = conf.high, group = binary_rating), 
  #               alpha = 0.7, position = position_dodge(width = 0.55),
  #               width = .2,show.legend = FALSE) +
  scale_color_manual(values = my_color, aesthetics = c("fill")) +
  scale_shape_manual(values = c(3,4)) +
  labs(title = "Predicted probabilities by patient outcomes",
       y = "",
       x = "") +
  scale_y_continuous(labels = scales::percent) +
  visibly::theme_trueMinimal(center_axis_labels = T) +
  theme(plot.title = element_text(size=12),
        axis.text.x = element_text(size = 10),
        # axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle = 90, vjust = 0.5, size = 10),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(size = 13),
        legend.position="bottom",
        legend.title = element_blank())

ggsave("images/prob_2by2_binary_rating.png")
ggsave("/Volumes/GoogleDrive/My Drive/EQUIP Lab- Documents/Active Projects/2020.01 Milestone vs. Medicare Outcomes-Dan/Visualizations/prob_2by2_binary_rating.svg")

