# create or plot by miletorne ratings
# each milestone rating category includes 4 outocmes: death, severe complication, any complication 
# and re-admission
or_plot <- function(data, title) {
  
  data$term = factor(data$term,
                     levels = c("overall_mean", "professional", "operative", "leadership"))
  
  ggplot(data = data,
         aes(y = OR, x = outcome),
         alpha = 0.8) +
    geom_hline(
      yintercept = 1,
      alpha = 0.25,
      linetype = 2
    ) +
    geom_pointrange(aes(ymin = OR_lower, 
                        ymax = OR_upper,
                        shape = outcome,
                        fill = outcome,
                        color = outcome),
                    fatten = 5, size = 1, 
                    show.legend = F) +
    geom_text(aes(label = round(OR,2), hjust = -0.5, vjust = 0.5), size = 3) +
    scale_shape_manual(values = c(21:24)) +
    scale_color_manual(values = my_color, aesthetics = c("fill", "color")) +
    labs(x = "", y = "Odds Ratio (95% CI)",
         subtitle = str_to_title(title)) +
    # theme ---
    visibly::theme_clean(center_axis_labels = T) +
    theme(plot.title = element_text(size=12),
          axis.text.x = element_text(size = 10),
          # axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(angle = 90, vjust = 0.5, size = 10),
          axis.ticks.y=element_blank(),
          strip.text.y = element_text(size = 13),
          legend.position="bottom",
          legend.title = element_blank())
  
}


