#### alpha param shocks analysis

library(tidyverse)
library(stringr)

rm(list = ls())

source("plot_funs.R")

shock_df1 <- read_csv("data/julia/results_0125_0075_shoct_85_85.csv") %>% 
  add_column(model = rep(1L, 170))
shock_df2 <- read_csv("data/julia/results_012_006_0085_85_45_40.csv") %>% 
  add_column(model = rep(2L, 170))
shock_df3 <- read_csv("data/julia/results_015_012_006_0085_40_40_40_50.csv") %>% 
  add_column(model = rep(3L, 170))

######################

var_names <- c("carrier" = "Niezdiagnozowany", "infected" = "Zdiagnozowany", "exposed" = "Inkubacja")

plt1 <- shock_df1 %>% 
  plot_multi(names(var_names), var_names)

ggsave("figures/shock1.eps")

plt2 <- shock_df2 %>% 
  plot_multi(names(var_names), var_names)

ggsave("figures/shock2.eps")

plt3 <- shock_df3 %>% 
  plot_multi(names(var_names), var_names)

ggsave("figures/shock3.eps")

######################

do.call(
  bind_rows,
  list(shock_df1, shock_df2, shock_df3) %>% 
    map(~ rowid_to_column(.x, "time"))
) %>%
  mutate(model = as.factor(model)) %>% 
  select(model, time, all_of(names(var_names))) %>% 
  pivot_longer(all_of(names(var_names)), names_to = "type") %>% 
  ggplot(aes(x = time, y = value, colour = type)) +
  geom_line() +
  labs(x = "Czas", 
       y = "Liczba agentów",
       colour = "Stan",
       shape = "Model") + 
  scale_colour_hue(labels = var_names) + 
  facet_wrap(~ model, nrow = 1)

ggsave("figures/merged_shocks.eps")
