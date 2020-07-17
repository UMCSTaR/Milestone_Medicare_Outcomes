library(knitr)
library(readr)
library(tidyverse)

load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/pc/rating_model.rdata")

# severe complication ------
severe_cmp = names(results) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "severe_cmp")) %>% 
  pull

severe_cmp_model = map(severe_cmp, ~results[[.]])

names(severe_cmp_model) = str_remove(severe_cmp, "severe_cmp_")

rmarkdown::render(
  "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/code/model_summary/model_summary_report.Rmd",
  params = list(data = severe_cmp_model,
                outcome_name = "Severe Complication (exclude POA)"),
  output_dir = "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/reports",
  output_file = "severe_complication.html"
)



# death ------
load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/pc/rating_model.rdata")

death = names(results) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "death")) %>% 
  pull

death_model = map(death, ~results[[.]])
names(death_model) = str_remove(death, "death_")

rmarkdown::render(
  "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/code/model_summary/model_summary_report.Rmd",
  params = list(data = death_model,
                outcome_name = "Death within 30 days"),
  output_dir = "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/reports",
  output_file = "death_30d.html"
)


