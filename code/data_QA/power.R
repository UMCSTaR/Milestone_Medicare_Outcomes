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
model2 = extend(model, along="id_physician_npi", n=700)

powerSim(model2, nsim = 20)


model10 = extend(model, along="id_physician_npi", n=1000)
model20 = extend(model, along="id_physician_npi", n=2000)
model30 = extend(model, along="id_physician_npi", n=3000)
model40 = extend(model, along="id_physician_npi", n=4000)
model50 = extend(model, along="id_physician_npi", n=5000)
model60 = extend(model, along="id_physician_npi", n=6000)
model70 = extend(model, along="id_physician_npi", n=7000)
model80 = extend(model, along="id_physician_npi", n=8000)
model90 = extend(model, along="id_physician_npi", n=9000)
model100 = extend(model, along="id_physician_npi", n=10000)
model110 = extend(model, along="id_physician_npi", n=11000)
model120 = extend(model, along="id_physician_npi", n=12000)
model130 = extend(model, along="id_physician_npi", n=13000)
model140 = extend(model, along="id_physician_npi", n=14000)


ps10 = powerSim(model10, nsim = 10)
ps20 = powerSim(model20, nsim = 10)
ps30 = powerSim(model30, nsim = 10)
ps40 = powerSim(model30, nsim = 10)
ps50 = powerSim(model50, nsim = 10)
ps60 = powerSim(model60, nsim = 10)
ps70 = powerSim(model70, nsim = 10)
ps80 = powerSim(model80, nsim = 10)
ps90 = powerSim(model90, nsim = 10)
ps100 = powerSim(model100, nsim = 10)
ps110 = powerSim(model110, nsim = 10)
ps120 = powerSim(model120, nsim = 10)
ps130 = powerSim(model130, nsim = 10)
ps140 = powerSim(model140, nsim = 10) #80&% power











# power curve for the model
pc <-  powerCurve(model)

plot(pc)


model2 = extend(model, along="id_physician_npi", n=5000)
powerSim(model2, nsim = 5)

pc2 = powerCurve(model, within="id_physician_npi", nsim = 5,breaks=100:101)
plot(pc2)
