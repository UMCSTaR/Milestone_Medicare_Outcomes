# script to run descriptive stats based on months of practice cohorts.

# 12 months in practice
rmarkdown::render(
  "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/code/model_summary/descriptive_statistics.Rmd",
  params = list(n_month = 12),
  output_dir = "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/reports",
  output_file = "descriptive_12_months.html"
)

# 24 months in practice
rmarkdown::render(
  "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/code/model_summary/descriptive_statistics.Rmd",
  params = list(n_month = 24),
  output_dir = "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/reports",
  output_file = "descriptive_24_months.html"
)

# 36 months in practice
rmarkdown::render(
  "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/code/model_summary/descriptive_statistics.Rmd",
  params = list(n_month = 36),
  output_dir = "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/reports",
  output_file = "descriptive_no_limit.html"
)