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
  filter(term == "IntResponseValue_mean" |
           term == "prof_rating_mean")

mean_rating_tbl = mean_rating %>%
  mutate(
    OR = exp(value),
    OR_lower = exp(lower_2.5),
    OR_upper = exp(upper_97.5)
  ) %>%
  mutate(outcome = str_remove_all(outcome, pattern = c("Par_|_all_mean|_prof")),
         term = case_when(term == "IntResponseValue_mean" ~ "overall_mean",
                          term == "prof_rating_mean" ~ "professional_rating")) %>%
  select(outcome, term, OR, contains("OR"), p_value) 

# table
mean_rating_tbl %>% 
  mutate_if(is.numeric,~round(.,2)) %>% 
  mutate(`95% CI` = paste0("[", OR_lower, ", ", OR_upper, "]")) %>% 
  select(outcome, term, OR, `95% CI`, p_value) %>% 
  kable(digits = 2, caption = "Odds Ratio of Mean Milestone Scores") %>% 
  kable_styling(full_width = F) %>% 
  collapse_rows(columns = 1, valign = "top") 

# plot
ggplot(data = mean_rating_tbl %>% arrange(OR, outcome, term),
       aes(y = OR, x = term)) +
  geom_hline(aes(fill = outcome), yintercept = 1, alpha = 0.25, linetype = 2) +
  geom_pointrange(aes(ymin = OR_lower,
                      ymax = OR_upper,
                      shape = term)) +
  facet_wrap(~outcome, strip.position = "left", ncol = 1) +
  coord_flip() +
  labs(x = "", y = "Odds Ratio (95% CI)") +
  theme_minimal() +
  theme(axis.text.y=element_blank(),
        axis.text.x = element_text(size = 10),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(size = 12),
        legend.position="bottom",
        legend.title = element_blank())
  # visibly::theme_trueMinimal(center_axis_labels = T) 

  

ggplot(data = mean_rating_tbl,
       aes(y = OR, x = reorder(outcome, -OR))) +
  geom_point() +
  geom_vline(xintercept = 0, color = "firebrick4", alpha = 1/10) +
  geom_hline(yintercept = 1, alpha = 0.25)
  
  

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
    




