# create or plot by miletorne ratings
# each milestone rating category includes 4 outocmes: death, severe complication, any complication 
# and re-admission
or_plot <- function(data, title) {
  ggplot(data = data,
         aes(y = OR, x = outcome),
         alpha = 0.8) +
    geom_hline(
      yintercept = 1,
      alpha = 0.25,
      linetype = 2
    ) +
    geom_pointrange(aes(ymin = OR_lower, 
                        ymax = OR_upper),
                    fatten = 5, size = 1, 
                    
                    color = my_color,
                    show.legend = F) +
    geom_text(aes(label = round(OR,2), hjust = -0.3, vjust = 0.5)) +
    labs(x = "", y = "Odds Ratio (95% CI)",
         subtitle = str_to_title(title)) +
    # theme ---
    visibly::theme_clean(center_axis_labels = T) +
    theme(plot.title = element_text(size=12),
          axis.text.x = element_text(size = 10),
          axis.title.y = element_text(angle = 90, vjust = 0.5, size = 10),
          axis.ticks.y=element_blank(),
          strip.text.y = element_text(size = 13),
          legend.position="bottom",
          legend.title = element_blank()) 
  
}
