# OR plot and predicted probability plots
library(tidyverse)
library(glmmTMB)
library(mixedup)
library(purrr)
library(ggeffects)
library(kableExtra)

my_color = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")

# load 24 months ------
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc.rdata")

#1. OR ------
# data process----
mean_rating = map_df(results, extract_fixed_effects, .id = "outcome")  %>%
  filter(
    term == "IntResponseValue_mean" |
      term == "prof_rating_mean" |
      term == "operative_rating_mean"|
  term == "leadership_rating_mean")

mean_rating_tbl = mean_rating %>%
  mutate(
    OR = exp(value),
    OR_lower = exp(lower_2.5),
    OR_upper = exp(upper_97.5)
  ) %>%
  mutate(outcome = str_remove_all(outcome, pattern = c("Par_|_all_mean|_prof|_leadership|_operative")),
         term = case_when(term == "IntResponseValue_mean" ~ "overall_mean",
                          term == "prof_rating_mean" ~ "professional",
                          term == "operative_rating_mean" ~ "operative",
                          term == "leadership_rating_mean" ~ "leadership")) %>%
  select(outcome, term, OR, contains("OR"), p_value) 

# or table ------
mean_rating_tbl %>% 
  mutate_if(is.numeric,~round(.,2)) %>% 
  mutate(`95% CI` = paste0("[", OR_lower, ", ", OR_upper, "]")) %>% 
  select(outcome, rating = term, OR, `95% CI`, p_value) %>% 
  kable(digits = 2, caption = "Odds Ratios of Milestone Scores") %>% 
  kable_styling(full_width = F) %>% 
  collapse_rows(columns = 1, valign = "top") 


# or plot -----
ggplot(data = mean_rating_tbl %>%
         mutate(
           outcome = factor(outcome,
                            levels = c(
                              "death", "severe_cmp","any_cmp", "readmit"
                            )),
          
           term = factor(
             term,
             levels = c("overall_mean", "leadership", "operative", "professional")
           )
         ),
       aes(y = OR, x = outcome)) +
  geom_hline(
    aes(fill = term),
    yintercept = 1,
    alpha = 0.25,
    linetype = 2
  ) +
  scale_fill_grey(start = 0.8, end = 0.2) +
  geom_pointrange(aes(ymin = OR_lower, 
                      ymax = OR_upper,
                      shape = outcome,
                      color = outcome),
                  fatten = 5, size = 1) +
  scale_color_manual(values = my_color) +
  facet_wrap(~term, strip.position = "left", ncol = 1) +
  coord_flip() +
  labs(x = "", y = "Odds Ratio (95% CI)",
       title = "Risk-adjusted Odds Ratios") +
  visibly::theme_trueMinimal(center_axis_labels = T) +
  theme(axis.text.y=element_blank(),
        axis.text.x = element_text(size = 10),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(size = 13),
        legend.position="bottom",
        legend.title = element_blank()) 
 
ggsave("images/or_rating_plot.png") 
  

# 2. predicted rates -------
# data process ----
mean_ge8 = map2_df(
  # model
  rlist::list.match(results, 'mean_ge_8'),
  # terms
  "mean_ge_8",
  ggpredict,
  .id = "outcome"
) %>% 
  mutate(term = "overall_mean")

prof_ge8 = map2_df(
  # model
  rlist::list.match(results, 'prof_ge8'),
  # terms
  "prof_rating_ge8",
  ggpredict,
  .id = "outcome"
) %>% 
  mutate(term = "professional")

operative_ge8 = map2_df(
  # model
  rlist::list.match(results, 'operative_ge8'),
  # terms
  "operative_rating_ge8",
  ggpredict,
  .id = "outcome"
) %>% 
  mutate(term = "operative")

leadership = map2_df(
  # model
  rlist::list.match(results, 'leadership_ge8'),
  # terms
  "leadership_rating_ge8",
  ggpredict,
  .id = "outcome"
) %>% 
  mutate(term = "leadership")


bin_mean_pred = rbind(mean_ge8,
                      prof_ge8,
                      operative_ge8,
                      leadership)


bin_mean_pred = bin_mean_pred %>% 
  mutate_at(c("predicted", "conf.low", "conf.high"), ~round(., 2)) %>% 
  select(- std.error, - group)

bin_mean_pred_tbl =  bin_mean_pred %>% 
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
  kable(digits = 2, caption = "Predicted Probabilities of Mean Milestone Scores") %>% 
  kable_styling(full_width = F) %>% 
  collapse_rows(columns = 1:2, valign = "top") %>% 
  save_kable("images/pred_probs.png")
  


# prob plot ------
bin_mean_pred_tbl %>%
  # filter(term == "professional") %>%
  
  ggplot(aes(x = binary_rating, y = predicted)) +
  geom_bar(aes(fill = outcome), stat = "identity",  position = position_dodge(width = 0.6), width = 0.5, show.legend = FALSE) +
  # scale_color_manual(values = my_color) +
  scale_fill_manual(values=my_color) +  
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, shape = outcome), alpha = 0.7, position = position_dodge(width = 0.6)) +
  
  # horizontal 
  facet_wrap(~ term, nrow = 1, strip.position = "bottom") +
  
  # vertical
  # facet_wrap(~ term, nrow = 4, strip.position = "left") +
  # coord_flip() +
  
  labs(title = "Predicted Probabilities of Binay Ratings",
       y = "",
       x = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  visibly::theme_trueMinimal(center_axis_labels = T) 

  
ggsave(filename = "images/predicted_prob_prof_cmp_v.png")  # vertical bars
# ggsave(filename = "images/predicted_prob_prof_cmp_h.png") # horizontal bars


# combine plots -----
library(gridExtra)
combo = grid.arrange(or + theme(legend.position = "none") + labs(title = "") , 
                     v + theme(legend.position = "none",
                               strip.text.y = element_blank()) + labs(title = "", y = "predicted probability"),
                     ncol = 2)

ggsave(combo, filename = "images/or_prob_combo.png")

# geom_segment arrow-----
ci_bars = 
  bin_mean_pred_tbl %>%
  filter(outcome == "any_cmp") %>%
  
  ggplot(aes(x = binary_rating, y = predicted)) +
  geom_pointrange(
    aes(
      ymin = conf.low,
      ymax = conf.high,
      shape = outcome,
      # color = outcome
    ),
    alpha = 0.7,
    color = "gray",
    position = position_dodge(width = 0.6),
    show.legend = FALSE
  ) +
  # geom_point(color = my_color[[1]], show.legend = FALSE, size = 4) +
  geom_point(color = my_color[[1]], show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) 

ci_bars +
  geom_segment(
    data = bin_mean_pred_tbl %>%
      filter(outcome == "any_cmp") %>%
      select(outcome, term, binary_rating, predicted) %>%
      pivot_wider(names_from = binary_rating, values_from = predicted),
    aes(
      x = "<8",
      y = `<8`,
      xend  = ">=8",
      yend = `>=8`
    ),
    arrow = arrow(length=unit(0.30,"cm"), type = "closed"),
    color = my_color[[1]]
  ) +
  scale_color_manual(values = my_color) +
  facet_wrap( ~ term, nrow = 1, strip.position = "bottom") +
  labs(y = "",
       x = "",
       subtitle = "Predicted probabilities (95% CI) for any complication") +
  theme(legend.title = element_blank()) +
  visibly::theme_trueMinimal(center_axis_labels = T) 

ggsave("images/prob_segment_arrow.png")
 

# diff probs ------
bin_mean_pred_tbl %>%
  select(term, outcome, binary_rating, predicted) %>% 
  as_tibble() %>% 
  pivot_wider(names_from = binary_rating, values_from = predicted ) %>% 
  mutate(pred_diff = `<8` - `>=8`) %>% 
  ggplot(aes(x = term, y = pred_diff)) +
  geom_bar(aes(fill = outcome), stat = "identity",  position = position_dodge(width = 0.6), width = 0.5) +
  scale_fill_manual(values=my_color) +
  labs(title = "Predicted Probabilities diff (binary ratings)",
       y = "",
       x = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  visibly::theme_trueMinimal(center_axis_labels = T) 
  

# round 2------
# 1. OR--------
milestone_terms = unique(mean_rating_tbl$term)
p_or = paste0("p_or", 1:4)

for (i in seq_along(milestone_terms)) {
  by_milestone = mean_rating_tbl %>% 
    filter(term == milestone_terms[[i]]) %>% 
    mutate(
      outcome = factor(outcome,
                       levels = c(
                         "death", "severe_cmp","any_cmp", "readmit"
                       )))
  
  p = ggplot(data = by_milestone,
         aes(y = OR, x = outcome)) +
    geom_hline(
      yintercept = 1,
      alpha = 0.25,
      linetype = 2
    ) +
    geom_pointrange(aes(ymin = OR_lower, 
                        ymax = OR_upper,
                        color = outcome),
                    fatten = 5, size = 1, show.legend = F) +
    scale_color_manual(values = my_color) +
    labs(x = "", y = "Odds Ratio (95% CI)",
         title = str_to_title(milestone_terms[[i]])) +
    visibly::theme_trueMinimal(center_axis_labels = T) +
    theme(axis.text.x = element_text(size = 10),
          axis.title.y = element_text(angle = 90, vjust = 0.5, size = 10),
          axis.ticks.y=element_blank(),
          strip.text.y = element_text(size = 13),
          legend.position="bottom",
          legend.title = element_blank()) 
  
  assign(p_or[i], p)
  
  # save image
  ggsave(paste0("images/",milestone_terms[i], "_or.png"))
  
}



# 2. Probability -----
# side by side bar -----
milestone_terms = unique(bin_mean_pred_tbl$term)
p_prob = paste0("p_prob", 1:4)


for (i in seq_along(milestone_terms)) {
 p = bin_mean_pred_tbl %>%
    filter(term == milestone_terms[i]) %>%
    
    ggplot(aes(x = outcome, y = predicted, fill = binary_rating)) +
    
    geom_bar(aes(fill = binary_rating), stat = "identity",
             color="gray50",
             position = position_dodge(width = 0.55), width = 0.5) +
    # scale_color_manual(values = my_color) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.7, position = position_dodge(width = 0.55), width = .2,show.legend = FALSE) +
    scale_fill_manual(values = c('firebrick', 'darkgrey'))+
    
    labs(title = str_to_title(milestone_terms[i]),
         y = "",
         x = "") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = c(0.2, 0.9),
          legend.title = element_blank(),
    ) +
    visibly::theme_trueMinimal(center_axis_labels = T) 
  
  assign(p_prob[i], p)
  
  # save image
  ggsave(paste0("images/",milestone_terms[i], "_prob.png"))
  
}

# 3. combine OR and Prob -----


g = grid.arrange(p_or1, p_prob1 ,ncol=2)
ggsave(file="images/comb_overall_mean.png", g)

g = grid.arrange(p_or2, p_prob2 ,ncol=2)
ggsave(file = "images/comb_professional.png", g)

g = grid.arrange(p_or3, p_prob3 ,ncol=2)
ggsave(file = "images/comb_operative.png", g)

g = grid.arrange(p_or4, p_prob4 ,ncol=2)
ggsave(file = "images/comb_leadership.png", g)


