library(tidyverse)
library(glmmTMB)
library(mixedup)
library(purrr)

n_month = 30

if (n_month == 12) {
  load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month12_pc.rdata")
} else if (n_month == 24) {
  load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc.rdata")
} else if (n_month>24) {
  load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_non_limit_pc.rdata")
}

# overall rating
mean_rating = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "IntResponseValue_mean")

less_than_8 = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(str_detect(term, "mean_ge_8")) # if one rate is less than 8

mean_lt_8_rating = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "never_less_8_rating")


# professional
prof_rating = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "prof_rating_mean") 

prof_rating_lt8 = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(str_detect(term,"prof_rating_ge8"))

# operative
operative_mean = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "operative_rating_mean")

operative_mean_lt8 = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "operative_rating_ge8")


# leadership
leadership_rating = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "leadership_rating_mean")

leadership_rating_lt8 = map_df(results, extract_fixed_effects, .id = "outcome")  %>% 
  filter(term == "leadership_rating_ge8")



# Odds ratio for all outcomes by predictors for partial colectomy-----------
or_table = rbind(
  mean_rating,
  less_than_8,
  mean_lt_8_rating,
  prof_rating,
  prof_rating_lt8,
  operative_mean,
  operative_mean_lt8,
  leadership_rating,
  leadership_rating_lt8
) %>%
  mutate(
    OR = exp(value),
    OR_lower = exp(lower_2.5),
    OR_upper = exp(upper_97.5),
    outcome = str_remove_all(
      outcome,
      "Par_|_all_mean|_prof|_lt8|_operative|_leadership|_mean_ge_8|_never_less_8|_ge8"
    ),
    term = ifelse(term == "IntResponseValue_mean", "overall_mean", term)
  ) 


# plot
or_table %>% 
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
  theme(
    panel.grid   = element_blank(),
    panel.grid.major.y = element_line(color = alpha("firebrick4", 1 /
                                                      4), linetype = 3),
    axis.text.y  = element_text(hjust = 0),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = "Patient Outcomes",
    title = paste0("Odds Ratio Plot for Partial Colectomy - ", n_month, " months")
  )

ggsave(paste0("images/or_bar_", n_month, "months.png"))


