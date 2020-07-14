model_vis <- function(model,
                      predict_term,
                      binary_outcome = FALSE) {
  if (binary_outcome == T) {
      
      pointrange_predict = plot(ggpredict(model, terms = predict_term)) 
    
      plot(pointrange_predict)

      ggpredict(model, terms = paste(predict_term)) %>% 
        mutate(quart = c("Q1", "median", "Q3")) %>% 
        rename(milestone_rating = x) %>% 
        select(quart, everything(), -group) %>% 
        kable_df() %>% 
      return()
      
    } else {
      
      pointrange_predict = ggpredict(model, terms = predict_term) %>% 
        ggplot(aes(x = factor(x), y = predicted)) +
        geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
        xlab("Less than 7 Rating") +
        theme_classic() 
      
      plot(pointrange_predict)
      
      ggpredict(model, terms = predict_term) %>% 
        rename(Less_7_rating = x) %>% 
        kable_df() %>% 
        return()
    }
}

