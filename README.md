Milestone_Medicare_Outcomes

# Summary

Use Milestone rating data from ACGME to look at the its association with Medicare Patient Outcome

# Dataset

-   ACGME Milestone
-   Medicare Partial Colectomy

# Lab notebooks

## December


-   [PGY4 end year ratings explore](code/PGY4/PGY4_rating_brief.pdf)

Using PGY4 end year evaluation ratings to explore medicare outcomes. Same model and approach as PGY5 ratings.

-   [QA Death rate repot](misc_docs/QA_death_rate_v1.pdf)

Why death rate is high in our partial colectomy cohort report, including 10 sample patients from checking back to MedPAR.

Asana discussion about the death rate is: https://app.asana.com/0/1183110919789797/1199554862780893

-   [QA Multi_surgeon flag](https://app.asana.com/0/1183110919789797/1199373110557573)

Asana discussion

In this discussion, we decided remove the multi_suregon flag. Instead, we created a new had_assistant_surgeon flag to indicate if the case has assistant surgeon participation. We also decided to exclude cases performed by assistant surgeon, i.e. only keep primary cases performed by milestone graduates.

-   [Had_assist_surg interation](misc_docs/interaction_asssit_surg.pdf)

Explore if had assistant surgeon in the case and milestone ratings had significant different effects on the patient outcome

-   [Milestone ratings distribution](misc_docs/milestone_rating_distribution.pdf)

Descriptive stats to look at continuous milestone ratings distributions by table and plot. This report also looks at the distribution of binary milestone ratong by "≥8" and "≥9". Critical deficiency and outliers descriptive are also included.

- [model summary for (=9 vs <9) ratings](images/model_cutoff9_coef_summary.png)

ORs and p values for binary ratings =9 vs. <9. Same covariates as other models.

- [Visualization](https://app.asana.com/0/1183110919789797/1199373110557577)

Odds ratio and predicted probabilities plots based on the model outcomes


