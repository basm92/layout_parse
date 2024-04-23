library(selenider); library(tidyverse)
# Connect to a Chrome instance
session <- selenider_session(
  "chromote",
  timeout = 10
)

# Open URL and Initialize empty data set
open_url('https://privilegien.patentamt.at/search/-/-/1/-/-/')
data <- tibble()
session$driver$view()
s(css='button[data-set="cookie-banner-accept"]') |>
  elem_click()
Sys.sleep(3)

# For each page, do this (992 pages)
for(i in 1:992){
  # Select 100 entries per page
  for(j in 1:3){
    Sys.sleep(3)
    s(css='select#j_idt483\\:hitsPerPageSelect') |> 
      elem_click() |>
      elem_send_keys(keys$down)
    # Go to the bottom of the page
    s(css='nav[aria-label="Pagination below"] a[aria-label="more"]') |>
      elem_scroll_to()
  }
  # Find the titles
  elems <- session |>
    find_elements('div.search-list__hit h3 a') 
  titles <- as.list(elems) |>
    map(~elem_attr(.x, 'title'))
  
  # Find the rest
  elems <- session |>
    find_elements('div.search-list__hit div.search-list__hit-type') |>
    as.list()
  text <- elems |>
    map(~ elem_text(.x, timeout=10))
  
  # Combine the data
  data_one_page <- tibble(data=text, title=titles) |> 
    unnest(cols=c(data,title)) |>
    mutate(data = str_squish(str_replace_all(data, "\n\t|\n\n\t", ""))) |>
    separate_wider_delim(cols=data,names=c("a","inventor"), delim="Inventor:") |>
    separate_wider_delim(cols=inventor,names=c("inventor", "shelfmark"),delim="Shelfmark:") |>
    separate_wider_delim(cols=shelfmark, names=c("shelfmark", "granted"), delim="granted on:")
  
  # Save it
  data <- bind_rows(data, data_one_page)
  # Then, find the bottom next button
  session |>
    find_element('nav[aria-label="Pagination below"] li.numeric-paginator__navigate a') |>
    elem_click()
  
  Sys.sleep(5)
  
}
