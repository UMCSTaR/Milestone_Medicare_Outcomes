Milestone_Medicare_Outcomes

# Summary

Use Milestone rating data from ACGME to look at the its association with Medicare Patient Outcomes

# Dataset

-   ACGME Milestone
-   Medicare Partial Colectomy

# Lab notebooks

This section summarize the major reports from this repo, and detailed discussions on Asana project.

## Publication ready 

### Figures

- 1. Fig1: Cohort selection plot

![](misc_docs/cohort_flow_2-25.png)

- 2. Fig3: Estimated Probability of Complications based on Graduating Milestone Proficiency

![](images/prob_2by2_binary_rating_gray_include_no_case_vol.png)

- 3. Fig4. Estimated Probability of Complications based on Presence of Assisting Surgeon in Relationship to Milestone Proficiency Status

![with error bar](images/interaction_assist_surg_overall_rating_any_cmp.png)


### Tables

- 1. Table1: cohort description by ≥8 vs. <8

[table1](manuscripts/table1.docx)

[table1 suregon level]("manuscripts/table1_surgeon_level.docx)

- 2. example regression table

[regression table for any complication](reports/any_cmp_regression_table_v1.docx)

- 3. supplemental: Combined ORs Table of continuous and binary Milestone Ratings

[summary OR table](reports/or_table_no_case_vol_v1.docx)

*code used to reproduce the table is [2by2_plot.R](code/visual/2by2_plot.R)*




## Main Results

- [Cohort Definition](misc_docs/Milestone_Data_Process/update_12_18.png)

Data flow diagram showing the details of how we defined our cohort.

- [Cohort descriptive stats report](code/model_summary/descriptives_analytic_data.pdf)

Descriptive statistics for the cohort we uses for modeling. In the report, we also compared the variable differences between the binary groups(cutoff 8)


- Model results

Model results details are save on [google drive](https://drive.google.com/drive/u/0/folders/1BoRqENjl6QqQ0gr72hrAZaJzUxb5UXgW) by each patient outcome.

If you are interested to see only milestone ratings stats form the models, the summary tables are[Binary rating summary](images/bin_model_summary.png), [Continuous rating summary](images/mean_model_summary.png). After all, it's hard to go through all 64 models to check the coefficients of milestone ratings.

- Visualization

[Predicted probabilities plot](images/final_prob_2by2_binary_rating_gray.png) at images/final_prob_2by2_binary_rating_gray.png

[Actual predicted probability values used in the plots](images/pred_probs.png)


- [Had_assist_surg interation](misc_docs/interaction_asssit_surg.pdf)

[the interaction plot](images/interaction_assist_surg_overall_rating.png)

Explore if had assistant surgeon in the case and milestone ratings had significant different effects on the patient outcome. All interaction terms are summarized in one table. One example model was used to show more model details.

## Quality Assurance and Explore Ideas

-   [PGY4 end year ratings explore](code/PGY4/PGY4_rating_brief.pdf)

Using PGY4 end-year evaluation ratings to explore medicare outcomes. The same approach as PGY5 ratings. Even though PGY4 has a wider rating distribution, no significant association was found between ratings as outcome.

-   [QA Death rate report](misc_docs/QA_death_rate_v1.pdf)

Why death rate is high in our partial colectomy cohort report, including 10 sample patients from checking back to MedPAR.

Asana discussion about the death rate is: https://app.asana.com/0/1183110919789797/1199554862780893

-   [Asana discussion: Multi_surgeon flag](https://app.asana.com/0/1183110919789797/1199373110557573)

In this discussion, we decided remove the multi_surgeon flag. Instead, we created a new had_assistant_surgeon flag to indicate if the case has assistant surgeon participation. We also decided to exclude cases performed by assistant surgeon, i.e. only keep primary cases performed by milestone graduates.

-   [Milestone ratings distribution](misc_docs/milestone_rating_distribution.pdf)

Descriptive stats to look at continuous milestone ratings distributions by table and plot. This report also looks at the distribution of binary milestone rating by "≥8" and "≥9". Critical deficiency and outliers descriptive are also included.


- [model summary for (=9 vs <9) ratings](images/model_cutoff9_coef_summary.png)

ORs and p values for binary ratings =9 vs. <9. Same covariates as other models. Models are saved on maize: George_Surgeon_Projects/Milestone_vs_Outcomes/model/models_month24_pc_cutoff9.rdata


- [Asana discussion: Post-hoc power analysis](https://app.asana.com/0/1183110919789797/1199379804396909)

Several research and R simulation code to show the post-hoc power analyses were not recommended.

- [straigtlining ratings](code/data_QA/straightline.pdf)

Explore what the percentages of having same ratings for trainees 16 ratings are. Looked at analysis cohort, whole cohort at end-year eval for PGY5, and also mid-year PGY5 evals.
