library(tidyverse)
library(glmmTMB)
library(mixedup)
library(purrr)

load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/pc/rating_model.rdata")

mean_rating = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "IntResponseValue_mean")

# load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/prof_results_tmb.rdata")

prof_rating = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "prof_rating_mean") 

# load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/operative_results_tmb.rdata")

operative_mean = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "operative_rating_mean")

# load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/low_rating_results_tmb.rdata")

less_than_7 = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "ever_less_7_rating")

# leadership
leadership_rating = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "leadership_rating_mean")

# mean less than 7
mean_lt_7_rating = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "mean_lt_7")

# Odds ratio for all outcomes by predictors for partical colectomy-----------

rbind(mean_rating, prof_rating, operative_mean, less_than_7, 
      leadership_rating, mean_lt_7_rating) %>% 
  mutate(OR = exp(value),
         OR_lower = exp(lower_2.5),
         OR_upper = exp(upper_97.5),
         outcome = str_remove_all(outcome, "Par_|_all_mean|_prof|_all_mean|_ever_less_7|_operative|_leadership|_mean_less_7")
  ) %>% 
  ggplot(aes(y = OR, x = reorder(outcome, OR))) +
  geom_point() +
  geom_vline(xintercept = 0, color = "firebrick4", alpha = 1/10) +
  geom_hline(yintercept = 1, alpha = 0.25) +
  geom_pointrange(aes(ymin = OR_lower,
                    ymax = OR_upper),
                 size = 1/3, color = "#990024") +
  # lims(y = c(0.5,2)) +
  coord_flip() +
  facet_wrap( ~ term, ncol=2) +
  theme(panel.grid   = element_blank(),
        panel.grid.major.y = element_line(color = alpha("firebrick4", 1/4), linetype = 3),
        axis.text.y  = element_text(hjust = 0),
        axis.ticks.y = element_blank()) +
  labs(x = "Patient Outcomes", caption = "Partial Colectomy") 

ggsave("images/or_bar.png")


# odds ratio plot for all procedures, outcomes and milestones -------

load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/all_out_proc_rating.rdata")

primaries = c("IntResponseValue_mean", "prof_rating_mean", "operative_rating_mean",
              "ever_less_7_rating")

map_df(results, extract_fixed_effects, .id = "outcome") %>% 
  filter(term %in% primaries) %>% 
  mutate(OR = exp(value),
         OR_lower = exp(lower_2.5),
         OR_upper = exp(upper_97.5)
  ) %>% 
  filter(se<1) %>% 
  ggplot(aes(y = OR, x = reorder(outcome, OR))) +
  geom_point() +
  geom_vline(xintercept = 0, color = "firebrick4", alpha = 1/10) +
  geom_hline(yintercept = 1, alpha = 0.25) +
  geom_pointrange(aes(ymin = OR_lower,
                      ymax = OR_upper),
                  # .width = .95,
                  size = 3/4, color = "#990024") +
  lims(y = c(0.5,2)) +
  coord_flip() +
  theme(panel.grid   = element_blank(),
        panel.grid.major.y = element_line(color = alpha("firebrick4", 1/4), linetype = 3),
        axis.text.y  = element_text(hjust = 0),
        axis.ticks.y = element_blank()) +
  xlab("Patient Outcomes") 

ggsave("code/visual/or_all.png")
ggsave("images/or_all.png")


test = rbind(mean_rating, prof_rating, operative_mean, less_than_7) %>% 
  mutate(OR = exp(value),
         OR_lower = exp(lower_2.5),
         OR_upper = exp(upper_97.5)
  )
