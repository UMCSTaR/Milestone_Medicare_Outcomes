fe = function(model) {
  # fe 
  fe = extract_fixed_effects(model) %>% 
    mutate(OR = exp(value),
           OR_lower = exp(lower_2.5),
           OR_upper = exp(upper_97.5)
    ) %>% 
    select(-z, -se) %>% 
    rename(Estimate = value) 
  
  fe %>% 
    select(term, starts_with('OR'), everything())  %>% 
    kable_df() %>% 
    pack_rows('Milestone Rating', 2, 2) %>% 
    pack_rows('Patient', 3, 9) %>%
    pack_rows('Case', 10, 12)  %>% 
    pack_rows('Hospital',13, 15) 
}
