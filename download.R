#### data download (scraping)

library(rvest)
library(tidyverse)
library(stringr)

rm(list=ls())

# df <- tibble(.rows = 0)

main_page <- read_html("https://www.polskawliczbach.pl/Powiaty")
row_list <- main_page %>% html_nodes("tbody tr")

df <- row_list %>% map_dfr(function(r) {
  node_list <- html_nodes(r, "td")
  county_name <- html_node(node_list[[2]], "a") %>% html_attr("href")
  numb_of_citizens <- node_list[[4]] %>% html_text() %>% str_remove_all(" ") %>% as.integer
  urbanization_rate <- node_list[[6]] %>% html_text() %>% 
    str_remove("%") %>% str_replace(",", ".") %>% str_replace("-", "0") %>% as.numeric
  
  tibble(county_name, numb_of_citizens, urbanization_rate = urbanization_rate/100)
})

df %>% write_csv2("data/powiaty.csv")
