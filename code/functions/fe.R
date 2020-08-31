fe = function(model, pdf = TRUE) {
  # fe 
  fe = extract_fixed_effects(model) %>% 
    mutate(OR = exp(value),
           OR_lower = exp(lower_2.5),
           OR_upper = exp(upper_97.5),
           term = ifelse(term == "IntResponseValue_mean", "overall mean", term)
    ) %>% 
    select(-z, -se) %>% 
    rename(Estimate = value) 
  
  if (pdf == TRUE){
    fe %>% 
      select(term, starts_with('OR'), everything())  %>% 
      kable_df() %>% 
      pack_rows('Milestone Rating', 2, 2) %>% 
      pack_rows('Patient', 3, 9) %>%
      pack_rows('Case', 10, 12)  %>% 
      pack_rows('Hospital',13, 15) 
  } else {
    fe
  }
  
}
