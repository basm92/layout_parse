library(selenider); library(tidyverse); library(rvest)
# Connect to a Chrome instance
session <- selenider_session(
  "chromote",
  timeout = 15
)

# Open URL and Initialize empty data set
open_url('https://privilegien.patentamt.at/search/-/-/1/-/-/')
data <- tibble()
session$driver$view()
s(css='button[data-set="cookie-banner-accept"]') |>
  elem_click()
Sys.sleep(3)

# For each page, do this (992 pages)
for(i in 953:992){
  # Select 100 entries per page
  for(j in 1:3){
    s(css='select#j_idt483\\:hitsPerPageSelect') |> 
      elem_click() |>
      elem_send_keys(keys$down)
    Sys.sleep(1)
    # Go to the bottom of the page
    s(css='nav[aria-label="Pagination below"] a[aria-label="more"]') |>
      elem_scroll_to()
    Sys.sleep(2.5)
  }
  # Find the titles
  titles <- session |> 
    read_html() |>
    html_elements('div.search-list__hit h3 a') |>
    html_attr('title')
  # Find the text
  text <- session |>
    read_html() |>
    html_elements('div.search-list__hit div.search-list__hit-type') |>
    html_text2()

  data_one_page <- tibble(title=titles, text=text)
  # Save it
  data <- bind_rows(data, data_one_page)
  # Then, find the bottom next button
  session |>
    find_element('nav[aria-label="Pagination below"] li.numeric-paginator__navigate a[aria-label="more"]') |>
    elem_click()
  print(i)
  Sys.sleep(5)
  
}

data_ready <- data |>
  mutate(inventor = str_remove(str_extract(text, "Inventor: (.+)\n"), "Inventor: "),
         shelfmark = str_remove(str_extract(text, "Shelfmark: (.+)\n"), "Shelfmark: "),
         granted_on = str_remove(str_extract(text, "granted on: (.+)"), "granted on: ")) |>
  select(-text) |>
  mutate(across(c(inventor, shelfmark, granted_on), ~ str_squish(.x))) |>
  distinct()

# Merge with the geocoded data
library(sf)
geocoded_data <- st_read('./data/austrian_patent_data_geocoded.geojson') |>
  janitor::clean_names()

final_patent_data <- full_join(geocoded_data, data_ready, by=c("shelfmark", "inventor"))
write_sf(final_patent_data, './data/austrian_patent_data_geocoded_matched.geojson')
