##### full factorial design for 4 graph models

library(tidyverse)
library(stringr)
library(knitr)
library(kableExtra)
library(RColorBrewer)

rm(list = ls())

source("plot_funs.R")

full_mean_df <- read_csv("data/comp_mean_df.csv")
full_var_df <- read_csv("data/comp_var_df.csv")

graph_names <- expand_grid(c("ABCD ", "WS "), c("const", "var")) %>% 
  pmap_dfr(~ tibble(agr = str_c(...))) %>% 
  unlist %>% 
  `names<-`(NULL)

###################################################################

full_mean_df %>% 
  plot_comparison("I_70", "Średnia liczba aktywnych przypadków po 70 dniach", graph_names)

ggsave("figures/I70_mean.eps", device = "eps")

full_mean_df %>% 
  plot_comparison("S_70", "Średnia liczba niezarażonych po 70 dniach", graph_names)

ggsave("figures/S70_mean.eps", device = "eps")

full_mean_df %>% 
  plot_comparison("R_70", "Średnia liczba wyleczonych po 70 dniach", graph_names)

ggsave("figures/R70_mean.eps", device = "eps")

full_mean_df %>% 
  plot_comparison("D_70", "Średnia liczba zmarłych po 70 dniach", graph_names)

ggsave("figures/D70_mean.eps", device = "eps")

full_var_df %>% 
  plot_comparison("S_70", "Wariancja liczby aktywnych przypadków po 70 dniach", graph_names)

ggsave("figures/S70_var.eps", device = "eps")
