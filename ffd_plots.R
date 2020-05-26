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
  unlist

graph_names <- graph_names %>% set_names(
  graph_names %>% map_chr(function(graph_name) {
    splitted <- (graph_name %>% str_split(" "))[[1]]
    str_c(tolower(splitted[1]), "_", splitted[2])
  })
)

###################################################################

full_mean_df %>% 
  plot_comparison("_I_70", "Liczba agentów", graph_names)

ggsave("figures/I70_mean.eps", device = "eps")

full_mean_df %>% 
  plot_comparison("_S_70", "Liczba agentów", graph_names)

ggsave("figures/S70_mean.eps", device = "eps")

full_mean_df %>% 
  plot_comparison("_R_70", "Liczba agentów", graph_names)

ggsave("figures/R70_mean.eps", device = "eps")

full_mean_df %>% 
  plot_comparison("_D_70", "Liczba agentów", graph_names)

ggsave("figures/D70_mean.eps", device = "eps")

full_var_df %>% 
  plot_comparison("_I_70", "Wariancja liczby agentów", graph_names)

ggsave("figures/I70_var.eps", device = "eps")
