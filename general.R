#### general model analysis

library(tidyverse)
library(stringr)

rm(list = ls())

source("plot_funs.R")

var_names <- c("carrier" = "Niezdiagnozowany", "infected" = "Zdiagnozowany", "exposed" = "Inkubacja", "dead" = "Zmarły")

model_dfs <- "data/julia/tot_res_0" %>% 
  str_c(c("09", "11", "13", "15")) %>% 
  str_c(".csv") %>% 
  map2(~ read_csv(.x)) 

model_dfs2 <- model_dfs %>% map2(c(0.09, 0.11, 0.13, 0.15), function(model_df, alpha) {
  higher_inf_times <- map_int(1:9, ~ which.max(
    model_df %>% filter(idx == .x) %>% pull(infected)
  ))
  
  idx_median <- which(median(higher_inf_times) == higher_inf_times)[1]
  
  model_df %>%
    filter(idx == idx_median) %>%
    select(-idx) %>%
    add_column(model = rep(alpha, 180)) %>%
    rowid_to_column("time") %>%
    identity()
})
  
do.call(bind_rows, model_dfs2) %>%
  mutate(model = str_c("alpha = ", model)) %>% 
  select(model, time, all_of(names(var_names))) %>% 
  pivot_longer(all_of(names(var_names)), names_to = "type") %>% 
  ggplot(aes(x = time, y = value, colour = type)) +
  geom_line() +
  labs(x = "Czas", 
       y = "Liczba agentów",
       colour = "Stan",
       shape = "Model") + 
  scale_colour_hue(labels = var_names) + 
  facet_wrap(~ model, nrow = 2)

ggsave("figures/median_2x2.eps", device = "eps")

var_df <- do.call(bind_rows, map2(model_dfs, c(0.09, 0.11, 0.13, 0.15), function(model_df, alpha) {
  model_df %>% 
    add_column(model = rep(alpha, 180*9), time = rep(seq(1,180),9)) %>% 
    filter(time %in% c(100, 150, 180))
}))

var_df %>%
  mutate(model = as.factor(model)) %>% 
  ggplot(aes(x = time, y = infected, group = model)) +
    geom_boxplot() +
    scale_fill_hue()
