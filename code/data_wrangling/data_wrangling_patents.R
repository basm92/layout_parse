# italy_create_1856_1900_patents
library(sf); library(tidyverse); library(selenider); library(rvest); library(osmdata)

# Scrape Austrian Patents
# Connect to a Chrome instance
session <- selenider_session(
  "selenium",
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
for(i in 1:992){
  # Select 100 entries per page
  if(i==1){
  for(j in 1:3){
    s(xpath="//select[contains(@id, 'hitsPerPageSelect')]") |> 
      elem_click() |>
      elem_send_keys(keys$down)
    Sys.sleep(3)
    # Go to the bottom of the page
    s(css='nav[aria-label="Pagination below"] a[aria-label="more"]') |>
      elem_scroll_to()
    Sys.sleep(2.5)
  }
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

# Geocode the Obtained Data Frame
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

data_geocoded <- data_ready |>
  rowwise() |>
  mutate(coordinates = list(get_point(cur_data())))

#write_rds(data_geocoded, './data/austrian_patent_data_geocoded.RData')
data_sf <- data_geocoded |> unnest_wider(coordinates)

# Transform into sf data.frame
data_sf <- st_as_sf(data_sf |> mutate(
  x = if_else(is.na(x), 0, x),
  y = if_else(is.na(y), 0, y),
  not_available = if_else(x == 0 & y == 0, 1, 0)),
  coords=c("x", "y"), crs='wgs84')

# Create variables for each quarter in the timeframe 1856-1900
dates_start <- seq(as.Date("1856-01-01"), as.Date("1900-10-01"), by = "quarter")
dates_end <- dates_start + months(3)

# Create a df with a start and end for each municipality
grid <- expand_grid(relevant_part$LAU_ID, tibble(ds=dates_start, de=dates_end)) |>
  rename(LAU_ID = `relevant_part$LAU_ID`)

# Merge this with the relevant_part data.frame to obtain the polygon panel
polygon_panel <- grid |>
  left_join(relevant_part, by = "LAU_ID")

# Import the Austrian patent data
patents <- read_sf("./data/austrian_patent_data_geocoded_matched.geojson")

# Make a rowwise() function to extract the already geocoded patents in location i in quarter t
count_patents <- function(row, patents){
  date_start <- row$ds
  date_end <- row$de
  polygon <- row$`_ogr_geometry_` |> 
    st_buffer(dist=1500)
  found_patents <- patents |> 
    filter(between(granted_on, date_start, date_end)) |>
    st_filter(polygon)
  
  return(nrow(found_patents))
}

pp <- polygon_panel |>
  rowwise() |> 
  mutate(patents = count_patents(cur_data(), patents))

pp <- pp |> 
  mutate(year = year(ds))

# Integrate the Piedmontese data
pp_austria <- read_csv2('./data/patent_data/interim_patent_data/austrian_patent_data_cleaned.csv')

# Import the Piedmontese Data
piedmont <- readxl::read_xlsx('./data/Patents_Piedmont_1856_1862.xlsx') |>
  group_by(Period, Location) |> 
  count() |>
  ungroup()

# Geomatch these data as well
geocode_place <- function(row){
  loc <- row$Location
  out <- tryCatch({
    box <- getbb(loc)
    centroid <- data.frame(x=(box[1,1]+box[1,2])/2, y = (box[2,1]+box[2,2])/2)
    out <- st_as_sf(centroid, coords = c('x', 'y'), crs='wgs84')
  }, 
  error=function(e){
    centroid <- data.frame(x=0, y=0)
    out <- st_as_sf(centroid, coords=c('x', 'y'), crs='wgs84')
  })
  
  return(out)
}

test <- piedmont |> 
  rowwise() |> 
  mutate(coordinate = geocode_place(cur_data()))

test2 <- test |> 
  unnest_wider(coordinate) |>
  st_as_sf(crs='wgs84') |>
  mutate(year = as.numeric(str_extract(Period, "\\d{4}")),
         quarter = as.numeric(str_extract(Period, "\\d$")))

grid <- expand_grid(relevant_part$LAU_ID, year=1856:1862,quarter=1:4) |>
  rename(LAU_ID = `relevant_part$LAU_ID`)

polygon_panel <- grid |> left_join(relevant_part, by = "LAU_ID")

# Make a rowwise() function to extract the patents in location i in quarter t
# patents argument pertains to test2 data.frame
count_patents_piedmont <- function(row, patents){
  yr <- row$year
  qrt <- row$quarter
  polygon <- row$`_ogr_geometry_` |> 
    st_buffer(dist=1500)
  found_patents <- patents |> 
    filter(year == yr, quarter == qrt) |>
    st_filter(polygon)
  
  out <- found_patents |>
    summarize(total = sum(n, na.rm=T)) |>
    pull(total)
  return(out)
}

pp <- polygon_panel |>
  rowwise() |> 
  mutate(patents = count_patents_piedmont(cur_data(), test2))

# Incorporate the Piedmontese patents and the Austrian patents together
pp_austria_to_be_merged <- pp_austria |>
  filter(is.element(year, 1856:1862)) |>
  group_by(LAU_ID, year) |>
  mutate(quarter = row_number()) |>
  rename(patents_austria = patents) |>
  ungroup()

pp_piedmont_to_be_merged <- pp |>
  rename(patents_piedmont = patents) |>
  ungroup() |>
  st_drop_geometry() |>
  select(c(LAU_ID, year, quarter, patents_piedmont))

# Merge them together
together <- left_join(pp_austria_to_be_merged,  pp_piedmont_to_be_merged,
                      by=c("LAU_ID", "year", "quarter")) |>
  mutate(patents = patents_austria + patents_piedmont)

library(fixest)
reg1 <- feols(patents ~ group*post, data = together |> 
                mutate(post = if_else(year > 1859 & ds > ymd("1859-06-01"), 1, 0),
                       patents = patents*1000))

reg2 <- feols(patents ~ as.factor(year)*group + MOUNT_TYPE + COAST_TYPE + URBN_TYPE, data = together |> 
                mutate(patents = patents*1000), vcov='hc1')

# Export the together dataset
# Assuming your tibble is named 'together'

# Extract the coordinates from `_ogr_geometry_` column and convert to sf
together_sf <- left_join(together |>
                           select(-`_ogr_geometry_`), 
                         italy_municipalities |>
                           select(LAU_ID), by= 'LAU_ID') |> 
  st_as_sf() |>
  select(-c(FID.y)) |>
  rename(FID = FID.x)

# Write this to geojson
st_write(together_sf, "./data/italy_austrian_piedmontese_patents.geojson")
