library(knitr)
library(readr)
library(tidyverse)

# choose how many months of practice after graduation
n_month = 36

if (n_month == 12) {
  load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month12_pc.rdata")
} else if (n_month == 24) {
  load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc.rdata")
} else if (n_month > 24) {
  load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_non_limit_pc.rdata")
}

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
                n_month = n_month,
                outcome_name = "Severe Complication (exclude POA)"),
  output_dir = "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/reports",
  output_file = paste0("severe_complication-", n_month, "_months.html")
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
                n_month = n_month,
                outcome_name = "Death within 30 days"),
  output_dir = "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/reports",
  output_file = paste0("death_30days-", n_month, "_months.html")
)


