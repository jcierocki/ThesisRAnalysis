#### plotting funs

plot_comparison <- function(df, .end_with, .y_lab, .lab_names) {
  df %>% 
    select(pcontact, ends_with(.end_with)) %>% 
    pivot_longer(!contains("pcontact"), names_to = "type") %>% 
    ggplot(aes(x = pcontact, y = value, colour = type)) +
    geom_line() +
    geom_point() + 
    labs(x = "Prawodpobieństwo kontaktu", 
         y = .y_lab,
         colour = "Typ Grafu") + 
    scale_colour_hue(labels = .lab_names)
}