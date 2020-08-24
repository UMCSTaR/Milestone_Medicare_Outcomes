# master file to run multiple model reports based on months or practice and outcomes
library(knitr)
library(readr)
library(tidyverse)

source("code/functions/multi_reports_by_outcome.R")

# choose how many months of practice after graduation
n_month = 12

if (n_month == 12) {
  load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month12_pc.rdata")
} else if (n_month == 24) {
  load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc.rdata")
} else if (n_month > 24) {
  load("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_non_limit_pc.rdata")
}

# severe complication -----
model_reports_by_outcome(results = results,
                         outcome = "severe_cmp", 
                         outcome_name = "Severe Complication (exclude POA)",
                         n_month = n_month)



# death ------
model_reports_by_outcome(results = results,
                         outcome = "death", 
                         outcome_name = "Death within 30 days",
                         n_month = n_month)

# Any Complication -----
model_reports_by_outcome(results = results,
                         outcome = "any_cmp", 
                         outcome_name = "Any Complication",
                         n_month = n_month)

# Readmission -----
model_reports_by_outcome(results = results,
                         outcome = "readmit", 
                         outcome_name = "Readmission within 30 Days",
                         n_month = n_month)


