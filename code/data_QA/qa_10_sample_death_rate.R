# find the 10 sample patient to QA death
library(dplyr)
library(tidyr)
library(data.table)


load("Y:/Milestone_vs_Outcomes/sample_10.rdata")
medicare = data.table::fread("Y:/standardized_medicare_data_using_R/analytic/full_data/Archived/analytic_file.csv")

# find facility claim ID ---
sample10_analytic = medicare[id %in% sample_10$id]

# check
all_equal(sample10_analytic %>% distinct(id, member_id),
          sample_10 %>% distinct(id, member_id))

sample10_analytic = sample10_analytic[, .(id, member_id, facility_clm_yr, fac_claim_id)]

# the facility claim ID to find the original claims from MedPAR 2015
# SAS code to filter; didn' save the code.

# SAS code used to get the 10 patients.
# libname medpar "X:/Medicare_100/MedPAR";
# 
# proc sql;
# select MEDPAR_ID
# from medpar.medpar2015
# where MEDPAR_ID
# in ('39891', '40211', '40291', '40411', '40413', '40491', '40493', '42830')
# quit;
# 
# 
# proc sql;
# create table sample10_medpar as
# select *
#   from medpar.medpar2015
# where MEDPAR_ID in ('GGGGGGkzuBkTuqG', 'GGGGGGkzoGVkoTq', 'GGGGGGkzTVGquVT', 'GGGGGGkuzkqzoBz', 'GGGGGGkzTGBzVGF', 'GGGGGGkuGqGqoFV', 'GGGGGGkzBkFVqFo', 'GGGGGGkuuzoqTuz', 'GGGGGGkuGGzoGoV', 'GGGGGGkVFouBGqT');
# quit;
# 
# Proc export data=sample10_medpar
# outfile="Y:/Milestone_vs_Outcomes/sample10_medpar.csv"   /* second = character removed*/
#   dbms=CSV
# replace
# ;
# run;
