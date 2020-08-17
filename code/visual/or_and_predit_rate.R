# OR plot and predicted probability plots
library(tidyverse)
library(glmmTMB)
library(mixedup)
library(purrr)
library(ggeffects)
library(kableExtra)

# load 24 months
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc.rdata")

# overall mean -------
# or
mean_rating = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "IntResponseValue_mean")

mean_rating_tbl = mean_rating %>%
  mutate(
    OR = exp(value),
    OR_lower = exp(lower_2.5),
    OR_upper = exp(upper_97.5)
  ) %>%
  mutate(outcome = str_remove_all(outcome, pattern = c("Par_|_all_mean")),
         term = "overall_mean") %>%
  select(outcome, term, OR, contains("OR"), p_value) 

mean_rating_tbl %>% 
  mutate_if(is.numeric,~round(.,2)) %>% 
  mutate(`95% CI` = paste0("[", OR_lower, ", ", OR_upper, "]")) %>% 
  select(outcome, OR, `95% CI`, p_value) %>% 
  kable(digits = 2, caption = "Odds Ratio of Mean Milestone Scores") %>% 
  kable_styling(full_width = F) 
  

# predicted rate
bin_mean_pred = map2_df(
  # model
  rlist::list.match(results, 'mean_ge_8'),
  # terms
  "mean_ge_8",
  ggpredict,
  .id = "outcome"
)

bin_mean_pred = bin_mean_pred %>% 
  mutate_at(c("predicted", "conf.low", "conf.high"), ~round(., 2)) %>% 
  select(- std.error, - group)

bin_mean_pred_tbl =  bin_mean_pred %>% 
  mutate(outcome = str_remove_all(outcome, pattern = c("Par_|_mean_ge_8")),
         x = ifelse(x ==1, ">=8", "<8")) %>% 
  rename(overall_mean = x)

bin_mean_pred_tbl %>% 
  mutate(`95% CI` = paste0("[", conf.low, ", ", conf.high, "]")) %>% 
  select(-contains(".")) %>% 
  kable(digits = 2, caption = "Predicted Probabilities of Mean Milestone Scores") %>% 
  kable_styling(full_width = F) %>% 
  collapse_rows(columns = 1, valign = "top") 
  


# plot
bin_mean_pred_tbl %>%
  filter(outcome == "any_cmp") %>% 
  
  ggplot(aes(x = overall_mean, y = predicted)) +
  geom_bar(stat = "identity", fill = "#00274C",width = 0.5) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  colour = "#FFCB05",
                  size = 1.3) +
  labs(title = "Predicted Probabilities of Any Complication",
       y = "",
       x = "Mean Milestone Scores") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
    




