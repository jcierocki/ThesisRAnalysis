#### plotting funs

plot_comparison <- function(df, .ends_with, .y_lab, .lab_names) {
  df %>% 
    select(pcontact, ends_with(.ends_with)) %>% 
    rename_all(~ str_remove(.x, .ends_with)) %>% 
    pivot_longer(!contains("pcontact"), names_to = "type") %>% 
    ggplot(aes(x = pcontact, y = value, colour = type)) +
    geom_line() +
    geom_point() + 
    labs(x = "Prawodpobieństwo kontaktu", 
         y = .y_lab,
         colour = "Typ Grafu") +
    scale_colour_hue(labels = .lab_names)
}

plot_multi <- function(df, .var_names, .lab_names) {
  df %>% 
    select(all_of(.var_names)) %>% 
    rowid_to_column("time") %>% 
    pivot_longer(!contains("time"), names_to = "type") %>% 
    ggplot(aes(x = time, y = value, colour = type)) +
    geom_line() +
    labs(x = "Czas", 
         y = "Liczba agentów",
         colour = "Stan") + 
    scale_colour_hue(labels = .lab_names)
}