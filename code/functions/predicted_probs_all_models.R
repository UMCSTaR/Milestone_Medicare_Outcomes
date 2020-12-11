# get predicted probabilities for 4 patient outcomes based on 4 categories of binary milestone ratings
# this function defined using map will be used in map again (interesting...)

predicted_probs_all_models <- function(model_name, rating_name, term_name_list) {
  map_df(
    # model
    rlist::list.match(results, model_name),
    # terms
    ggeffects::ggpredict,
    rating_name,
    .id = "outcome"
  ) %>% 
    mutate(term = term_name_list)
}
