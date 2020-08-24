# descriptive summary table
sum_table <- function(name = "overall_mean",
                      rating_var = IntResponseValue_mean) {
  rating_var = enquo(rating_var)
  
  milestone_medicare_pc %>%
    mutate(!!name := round(!!rating_var)) %>%
    group_by(!!!syms(name)) %>%
    mutate(
      n_surgeons = length(unique(id_physician_npi)),
      n_cases = n(),
      case_per_surg = round(n_cases/n_surgeons, 2),
      n_severe_cmp_poa = sum(flg_cmp_po_severe_not_poa),
      
      # pt
      male_pt_rate = mean(flg_male, na.rm = T),
      white_pt_rate = mean(race_white, na.rm = T),
      emerg_admit_rate = mean(flg_admit_emerg, na.rm = T),
      open_colectomy_rate = mean(open_colectomy, na.rm = T),
      hosp_bed_gt350_rate = mean(hosp_beds_2grp, na.rm = T),
      
      rate_severe_cmp_poa = n_severe_cmp_poa / n_cases,
      n_death = sum(flg_death_30d),
      rate_death = n_death / n_cases,
      n_reop = sum(flg_util_reop),
      rate_reop = n_reop / n_cases,
      n_readmit = sum(flg_readmit_30d),
      rate_readmit = n_readmit / n_cases
    ) %>% 
    ungroup() %>%
    distinct(
      !!!syms(name),
      n_surgeons,
      n_cases,
      case_per_surg,
      across(contains("rate"))
    ) %>% 
    arrange(!!!syms(name)) %>% 
    mutate_at(vars(contains("rate")), scales::percent_format()) %>% 
    datatable(rownames = F, options = list(scrollX = T))
}
