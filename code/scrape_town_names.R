library(rvest)
library(tidyverse)

url <- 'https://fr.wikipedia.org/wiki/Liste_des_communes_italiennes_par_province'
provinces <- rvest::read_html(url) |> 
  html_elements('div.mw-parser-output li a') |> 
  html_attr(name = 'href')
provinces <- provinces[1:106]

town_lists <- map_chr(provinces, ~ paste0('https://fr.wikipedia.org', .x))
test <- town_lists[1]

extract_all_towns <- function(url){
  page <- read_html(url)
  city_names <- page |>
    html_elements('div.mw-parser-output ul li a') |> 
    html_attr('title')
  
  province_names <- page |> 
    html_elements("div.mw-content-container span.mw-page-title-main") |> 
    html_text()
    
    return(list(city_names, province_names))
}

data <- map(town_lists, ~ extract_all_towns(.x))
communes <- map(data, .f = ~ .x[[1]])
provinces <- map(data, .f = ~ .x[[2]])

together <- map2(communes, provinces, ~ data.frame(communes = .x, provinces = .y))
done <- together |> bind_rows()

patterns <- "Communes de la province d'|Communes de la province de|Communes de la ville métropolitaine de |Liste des communes de la ville métropolitaine de |Communes de la province du |Liste des communes de la "
final_data <- done |> 
  filter(!str_detect(communes, "Portail")) |> 
  mutate(communes = str_trim(str_replace(communes, "\\(Italie\\)", "")),
         provinces = str_trim(str_replace(provinces, patterns, "")))

write_csv2(final_data, '../data/list_municipalities_provinces.csv')
