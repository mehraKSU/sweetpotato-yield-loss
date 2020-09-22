# create a function to plot by different variables
plot_market_loss_by <- function(var){
  yieldloss %>% 
    dplyr::filter(!is.na({{var}})) %>% 
    dplyr::group_by({{var}}) %>% 
    summarise(avg_marketable_loss = mean(marketable_loss, na.rm = TRUE),
              se_marketable_loss = std.error(marketable_loss, na.rm = TRUE)) %>% 
    ggplot(mapping = aes(x = reorder(as_factor({{var}}), avg_marketable_loss), y = avg_marketable_loss,
                         color = avg_marketable_loss)) +
    geom_point(size = 3,
               color = "red"
    ) +
    geom_errorbar(mapping = aes(ymin = avg_marketable_loss - se_marketable_loss,
                                ymax = avg_marketable_loss + se_marketable_loss),
                  size = 0.3,
                  color = "red",
                  alpha = 0.4
    ) +
    ylab("Marketable yield loss (%)") +
    xlab(str_to_title(as_label(enquo(var)))) +
    coord_flip() +
    theme_bw(base_size = 14)
}