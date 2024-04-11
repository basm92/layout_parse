library(rvest); library(tidyverse); library(chromote)
begin <- session('https://privilegien.patentamt.at/search/-/-/1/-/-/')
condition <- TRUE
out <- list()

while(condition){
# Do this on every page
links_on_page <- begin |>
  html_elements('div.search-list__hit a') |>
  html_attr('href') |>
  unique()

output_page <- links_on_page |>
  map(~ .x |> 
        read_html() |>
        html_elements('h2 ~ dl') |>
        html_text2())

out <- c(out, output_page)
# Switch page to the next if possible
# If not terminate while loop
switch <- try({
  begin <- begin |> 
    session_follow_link(xpath="//a[@aria-label = 'more']")
  
  # Check if try_result is an error
  if (inherits(switch, "try-error")) {
    # Set looper to FALSE to exit the loop
    condition <- FALSE
    }
  })
}


