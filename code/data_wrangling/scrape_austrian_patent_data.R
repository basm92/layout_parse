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
library(osmdata)
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

write_rds(data_geocoded, './data/austrian_patent_data_geocoded.RData')
# Finish this
library(sf)
data_sf <- st_as_sf(data_geocoded |> unnest_wider(coordinates), coords=c("x", "y"), crs='wgs84')
