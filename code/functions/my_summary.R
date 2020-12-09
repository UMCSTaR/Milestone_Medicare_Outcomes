# summary tbl for death QA

my_summary <- function(data) {
  data %>%
    summarise(
      n_case = n(),
      n_surgeon = length(unique(id_physician_npi)),
      n_hosp = length(unique(facility_prvnumgrp)),
      n_patient = length(unique(member_id)),
      POA_severe_cmp_rate = mean(flg_cmp_po_severe_not_poa, na.rm = T),
      # severe_cmp_rate = sum(flg_cmp_po_severe)/n_case,
      # any_cmp_rate = sum(flg_cmp_po_any)/n_case,
      death_rate = sum(flg_death_30d) / n_case,
      readmit_rate = sum(flg_readmit_30d) / n_case,
      reop_rate = sum(flg_util_reop) / n_case
    ) %>%
    mutate_at(vars(contains("rate")), scales::percent_format())
}


# hash ID helper
hashed_id <- function(x, salt) {
  y <- paste(x, salt)
  y <- sapply(y, function(X) digest::digest(X, algo="sha1"))
  as.character(y)
}
