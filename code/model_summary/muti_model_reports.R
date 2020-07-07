library(knitr)
library(readr)
library(dplyr)

filelist <- list.files("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/pc/")

# loop through the file list to read in data and clean it up

for (file in filelist) {
  
  fp <- paste0("/Volumes/George_Surgeon_Projects/Milestone_vs_Outcomes/model/pc/", file, sep="")
  
  # read in results model
  load(fp)
  
  
  rmarkdown::render(input = "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/code/model_summary/model_summary_report.Rmd", 
                    output_file = paste0("model_", gsub(".rdata","", file), ".html"),
                    output_dir = "/Users/xilinchen/Documents/Repo/Milestone_Medicare_Outcomes/reports")
  
}

