#### data download (scraping)

library(rvest)
library(tidyverse)
library(stringr)
library(knitr)

rm(list=ls())

# demography_page <- read_html("https://www.polskawliczbach.pl/Powiaty")
# row_list <- demography_page %>% html_nodes("tbody tr")
# 
# df <- row_list %>% map_dfr(function(r) {
#   node_list <- html_nodes(r, "td")
#   county_name <- html_node(node_list[[2]], "a") %>% html_attr("href")
#   numb_of_citizens <- node_list[[4]] %>% html_text() %>% str_remove_all(" ") %>% as.integer
#   urbanization_rate <- node_list[[6]] %>% html_text() %>% 
#     str_remove("%") %>% str_replace(",", ".") %>% str_replace("-", "0") %>% as.numeric
#   
#   tibble(county_name, numb_of_citizens, urbanization_rate = urbanization_rate/100)
# })
# 
# df %>% write_csv2("data/powiaty.csv")

cfr_page <- read_html("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7104689/table/t1-ophrp-11-74/?report=objectonly")

main_table <- cfr_page %>% html_node("table") %>% html_table() %>% as_tibble() %>% slice(16:40)
main_table2 <- main_table %>% 
  mutate_at(vars("Case fatality rate", "Recovery response"), ~ str_replace(.x, "/", ".")) %>% 
  mutate_all(~ str_remove(.x, ",")) %>% 
  mutate_at(vars(-Country), as.numeric) %>% 
  na.omit() %>% 
  dplyr::select(-GDP, -Population, -"Recovery response") %>% 
  `colnames<-`(c("Kraj", "Miejsca na oddziałach intensywnej terapii (na 100 tys. mieszkańców)",
                 "Zarażenia", "Zgony", "Wyzdrowienia", "Aktywne przypadki", 
                 "Przypadki krytyczne", "Zarażenia na mln mieszkańców", "CFR")) %>% 
  dplyr::select("Kraj", "Zarażenia", "Zgony", "Aktywne przypadki", "Zarażenia na mln mieszkańców", "CFR") %>% 
  filter(Zgony >= 20)

main_table2 %>% kable(format = "latex")
