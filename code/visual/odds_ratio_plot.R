library(tidyverse)
library(glmmTMB)
library(mixedup)
library(purrr)

load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/results_tmb.rdata")

mean_rating = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "IntResponseValue_mean")

load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/prof_results_tmb.rdata")

prof_rating = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "prof_rating_mean") 

load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/operative_results_tmb.rdata")

operative_mean = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "operative_rating_mean")

# Odds ratio for all outcomes by predictors

rbind(mean_rating, prof_rating, operative_mean) %>% 
  mutate(OR = exp(value),
         OR_lower = exp(lower_2.5),
         OR_upper = exp(upper_97.5)
  ) %>% 
  ggplot(aes(y = OR, x = reorder(outcome, OR))) +
  geom_point() +
  geom_vline(xintercept = 0, color = "firebrick4", alpha = 1/10) +
  geom_hline(yintercept = 1, alpha = 0.25) +
  geom_pointrange(aes(ymin = OR_lower,
                    ymax = OR_upper),
                 .width = .95, 
                 size = 3/4, color = "#990024") +
  lims(y = c(0.5,2)) +
  coord_flip() +
  facet_wrap( ~ term, ncol=3) +
  theme(panel.grid   = element_blank(),
        panel.grid.major.y = element_line(color = alpha("firebrick4", 1/4), linetype = 3),
        axis.text.y  = element_text(hjust = 0),
        axis.ticks.y = element_blank()) +
  xlab("Patient Outcomes") 

ggsave("images/or_bar.png")
