library(selenider); library(tidyverse); library(rvest)
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
    s(css='select#j_idt483\\:hitsPerPageSelect') |> 
      elem_click() |>
      elem_send_keys(keys$down)
    Sys.sleep(0.5)
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
  # Find the titles
  #elems <- session |>
  #  find_elements('div.search-list__hit h3 a') 
  #titles <- as.list(elems) |>
  #  map(~elem_attr(.x, 'title', timeout=15))
  text <- session |>
    read_html() |>
    html_elements('div.search-list__hit div.search-list__hit-type') |>
    html_text2()
  # Find the rest
  #elems <- session |>
  #  find_elements('div.search-list__hit div.search-list__hit-type') |>
  #  as.list()
  #text <- elems |>
  #  map(~ elem_text(.x, timeout=15))
  
  # Combine the data
  #data_one_page <- tibble(data=text, title=titles) |> 
  #  unnest(cols=c(data,title)) |>
  #  mutate(data = str_squish(str_replace_all(data, "\n\t|\n\n\t", ""))) |>
  #  separate_wider_delim(cols=data,names=c("a","inventor"), delim="Inventor:", too_few = "align_start") |>
  #  separate_wider_delim(cols=inventor,names=c("inventor", "shelfmark"),delim="Shelfmark:", too_few = "align_start") |>
  #  separate_wider_delim(cols=shelfmark, names=c("shelfmark", "granted"), delim="granted on:", too_few = "align_start") 
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
