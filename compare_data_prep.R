#### full factorial design analysis for 4 graph variants

library(tidyverse)
library(stringr)

rm(list = ls())

compare_dfs <- "data/julia/compare_pcontact_01_02_" %>% 
  str_c(c("abcd_", "ws_")) %>% 
  map(~ str_c(.x, "333_" %>% str_c(c("const_pop", "var_pop")))) %>% 
  c %>% unlist %>% 
  str_c(".csv") %>% 
  map(read_csv)

compare_dfs2 <- compare_dfs %>% map(~ group_by(.x, pcontact) %>% 
                                      summarise_all(list(mean = mean, variance = var)))

compare_df2 <- expand_grid(c("abcd_", "ws_"), c("const_", "var_")) %>% 
  pmap_dfr(~ tibble(agr = str_c(...))) %>% 
  unlist %>% 
  map2(compare_dfs2, function(name_fix, df) {
    df %>% rename_at(vars(-pcontact), ~ str_c(name_fix, .x))
  })

full_mean_df <- compare_df2 %>% 
  map_dfc(~ select(.x, ends_with("_mean"))) %>% 
  rename_all(~ str_remove(.x, "_mean"))

full_mean_df <- tibble(pcontact = compare_df2[[1]]$pcontact) %>% bind_cols(full_mean_df)

full_var_df <- compare_df2 %>% 
  map_dfc(~ select(.x, ends_with("_variance"))) %>% 
  rename_all(~ str_remove(.x, "_variance"))

full_var_df <- tibble(pcontact = compare_df2[[1]]$pcontact) %>% bind_cols(full_var_df)

write_csv(full_mean_df, "data/comp_mean_df.csv")
write_csv(full_var_df, "data/comp_var_df.csv")
