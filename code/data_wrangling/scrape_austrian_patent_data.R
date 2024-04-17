library(rvest); library(tidyverse); library(chromote); library(osmdata); library(sf)
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
Sys.sleep(2)
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

# Write raw data
#write_rds(out, './data/austrian_patent_data_raw.RData')

# Clean the data
filtered_data <- map(out, ~ str_split(.x, pattern='\n\n')) |>
  keep(~ length(.x) > 0) 

split_and_convert <- function(x) {
  data.frame(do.call(rbind, str_split(x, ":")))
}

# Applying the function using map
data_frames <- map(filtered_data, ~ map_dfr(.x, split_and_convert))

# Pivot the data and delete the \n characters
pivoted_data_frames <- data_frames |>
  map(~ .x |>
        select(X1, X2) |>
        pivot_wider(names_from=X1, values_from=X2))
  
# Bind the rows to make it 1 data.frame
df <- pivoted_data_frames |>
  reduce(bind_rows)

final <- df |>
  mutate(across(everything(), ~ str_remove_all(.x, "\n")))

# Export to csv
#write_csv2(final, './data/austrian_patent_data_cleaned.csv')

# Read in and geocode
data <- read_csv2('./data/austrian_patent_data_cleaned.csv')

get_point <- function(row){
  place <- row$`Place of publication`
  country <- row$Country
  
  x <- NA
  y <- NA
  
  tryCatch({
    if (place != "-") {
      loc <- getbb(paste0(place, ", ", country))
      x <- (loc[1, 1] + loc[1, 2]) / 2
      y <- (loc[2, 1] + loc[2, 2]) / 2
    }
  }, error = function(e) {
    x <- NA
    y <- NA
  })
  
  return(data.frame(x=x, y=y))
  
}

data_geocoded <- data |>
  rowwise() |>
  mutate(coordinates = list(get_point(cur_data())))

#write_rds(data_geocoded, './data/austrian_patent_data_geocoded.RData')
# Finish this
library(sf)
data_sf <- data_geocoded |> unnest_wider(coordinates)

# Transform into sf data.frame
data_sf <- st_as_sf(data_sf |> mutate(
  x = if_else(is.na(x), 0, x),
  y = if_else(is.na(y), 0, y),
  not_available = if_else(x == 0 & y == 0, 1, 0)),
  coords=c("x", "y"), crs='wgs84')

# Write
write_sf(data_sf, './data/austrian_patent_data_geocoded.geojson')

test <- read_sf("./data/austrian_patent_data_geocoded.geojson")
# Also scrape the dates at which they were granted, and the names
pages <- paste0('https://privilegien.patentamt.at/search/-/-/', 1:9914, '/RELEVANCE/-/')

scrape_page <- function(page){
  # Read all the entries on a page
  out <- tryCatch({
    print(page)
    html <- read_html(page)
    elements_on_page <- html |>
      html_elements('div.search-list__hit')
    # Extract the data from these entries
    out_page <- map(elements_on_page, 
                    ~ {
                      title <- html_element(.x, 'h3 a') |>
                        html_attr('title')
                      text <- html_elements(.x, 'div.search-list__hit-text') |>
                        html_text2()
                      return(c(title, text))
                      })
  }, error = function(e, page){
    Sys.sleep(sample(10, 1) * 0.5)
    print("Error Encountered")
    out_page <- scrape_page(page)
  })
  return(out)
}

scraped_metadata <- pages |> map(scrape_page)



