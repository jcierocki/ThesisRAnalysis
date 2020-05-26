#### real coronavirus data plot

library(tidyverse)
library(stringr)

rm(list = ls())

source("plot_funs.R")

# 8 x 30 : 0.15, 0.1, 0.055, 0.75, 0.6, 0.75, 0.65, 0.75

df_real <- read_csv("data/coronavirus_poland.csv") %>% 
  rowid_to_column("time") %>% 
  add_column(type = rep("Dane rzeczywiste", 83)) %>% 
  rename(new_inf = "New cases")
df_model <- read_csv("data/julia/results_with_delta_fixed5.csv") %>% 
  rowid_to_column("time") %>%
  add_column(type = rep("Model", 240)) %>% 
  bind_rows(df_real)

df_model %>% 
  select(time, new_inf, type) %>% 
  ggplot(aes(x = time, y = new_inf, color = type)) +
    geom_line() +
    labs(color = "Typ") +
    scale_color_hue() +
    labs(x = "Czas (dni)", y = "Liczba nowych przypadków")

ggsave("figures/emp_vs_teor.eps", device = "eps")
