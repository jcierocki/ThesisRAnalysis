#### stock indexes

library(tidyverse)
library(stringr)

rm(list = ls())

source("plot_funs.R")

df_full <- inner_join(
  read_csv("data/wig.csv"),
  read_csv("data/sp500.csv"),
  by = "Data",
  suffix = c(".WIG", ".S&P500")
) %>% 
  select(Data, starts_with("Zamkniecie")) %>% 
  rename_all(~ str_remove(.x, "Zamkniecie."))

ind_dif <- mean(df_full$WIG)/mean(df_full$`S&P500`)

df_full %>% 
  mutate(`S&P500` = `S&P500` * ind_dif) %>% 
  pivot_longer(all_of(c("WIG", "S&P500")), names_to = "Indeks") %>% 
  ggplot(aes(x = Data, y = value, color = Indeks)) + 
    geom_line() +
    labs(x = "Czas", y = "Wartość indeksu") + 
    scale_colour_hue() + 
    scale_y_continuous(sec.axis = dup_axis(~ . / ind_dif, name = NULL))

ggsave("figures/indeses.eps", device = "eps")
