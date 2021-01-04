# post hoc power analysis
# never include in manuscript because post-hoc power analysis is just wrong!
# reference: https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12504
# In simr, power is calculated by repeating the following three steps: 
# (i) simulate new values for the response variable using the model provided;
# (ii) refit the model to the simulated response;
# (iii) apply a statistical test to the simulated fit.

library(simr)

load("X:\\George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc.rdata")

data = results$Par_severe_cmp_all_mean$frame

model = lme4::glmer(flg_cmp_po_severe_not_poa ~ IntResponseValue_mean + flg_male + age_at_admit_scale + race_white + flg_admit_emerg + AHRQ_score_scale + ses_2grp + facility_clm_yr + had_assist_surg + hosp_beds_2grp + val_hosp_mcday2inptday_ratio + val_hosp_rn2bed_ratio + (1 | id_physician_npi) + (1 | cpt_cd),
            family=binomial, data = data)

# power for current model
powerSim(model, nsim = 20)

# power curve for the model
pc <-  powerCurve(model)

