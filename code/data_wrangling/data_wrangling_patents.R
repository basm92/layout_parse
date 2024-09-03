# italy_create_1856_1900_patents
library(sf); library(tidyverse); library(selenider); library(rvest); library(osmdata); library(tidygeocoder)

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

# Afterwards, clean the data

data_ready <- data |>
  mutate(inventor = str_remove(str_extract(text, "Inventor: (.+)\n"), "Inventor: "),
         shelfmark = str_remove(str_extract(text, "Shelfmark: (.+)\n"), "Shelfmark: "),
         granted_on = str_remove(str_extract(text, "granted on: (.+)"), "granted on: ")) |>
  select(-text) |>
  mutate(across(c(inventor, shelfmark, granted_on), ~ str_squish(.x))) |>
  distinct()

# Save interim data
write_csv2(data_ready, './data/patent_data/interim_patent_data/austrian_patent_data_cleaned.csv')

# For geocoding, scrape each URL:
all_urls <- map(1837:1899, ~ {
  url <- paste0("https://privilegien.patentamt.at/sitelinks/", .x, "/")
  out <- read_html(url) |>
    html_elements("div.sitelinks__hits a") |>
    html_attr("href")
  
  return(out)
}
)

all_urls <- all_urls |> list_c()

data_for_urls <- map(all_urls, ~ {
  doc <- read_html(.x)
  data_box <- doc |>
    html_elements("div.metadata__wrapper div.metadata__ungrouped-wrapper, div.metadata__wrapper div.metadata__element-wrapper-single") |>
    html_text2()
  
  # Titles and Values
  titles <- map_chr(data_box, ~ str_extract(.x, "(.+):"))
  values <- map_chr(data_box, ~ str_extract(.x, ":\n(.+)"))

  out <- tibble(titles=titles, data=values) |>
    pivot_wider(names_from=titles, values_from = data)
  
  return(out)
  
})

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

# Merge data_ready with the location information and gecoded

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

# 2. Import the Piedmontese and Italian Data
piedmont <- readxl::read_xlsx('./data/patent_data/raw_patent_data/Patents_Piedmont_1855_1862.xlsx') |>
  janitor::clean_names() |>
  mutate(year = as.numeric(str_extract(period, "\\d{4}"))) |>
  group_by(year, location) |> 
  count() |>
  ungroup()
italy1863_1867 <- readxl::read_xlsx("./data/patent_data/raw_patent_data/Patents_Italy_1863_1867.xlsx")
italy_1878 <- readxl::read_xlsx("./data/patent_data/raw_patent_data/Patents_Italy_1878.xlsx")
italy_1889 <- readxl::read_xlsx("./data/patent_data/raw_patent_data/Patents_Italy_1889.xlsx") |>
  janitor::clean_names() |>
  group_by(location, country) |> 
  count() |>
  mutate(year = 1889)

italy_1902_1911 <- readxl::read_xlsx("./data/patent_data/raw_patent_data/Patents_Italy_1902_1911.xlsx") |>
  janitor::clean_names() |>
  filter(nazione_1 == "Italia") |>
  select(benchmark, residenza_luogo_1, residenza_provincia_1) |>
  group_by(benchmark, residenza_luogo_1, residenza_provincia_1) |> 
  count() |>
  mutate(residenza_luogo_1 = if_else(is.na(residenza_luogo_1), residenza_provincia_1, residenza_luogo_1)) |>
  ungroup() |>
  select(-residenza_provincia_1) |>
  rename(year = benchmark, location = residenza_luogo_1)


italy_together <- bind_rows(italy1863_1867, italy_1878) |>
  rename(year=anno, location=comune_1871, n = n_patents, comune_code = n_istat_1871) |>
  bind_rows(italy_1889) |>
  bind_rows(italy_1902_1911) |>
  bind_rows(piedmont) |>
  distinct()

# Geomatch these data as well
sf::sf_use_s2(FALSE)
municipalities <- read_sf('./shapefiles_images/italy_admin_borders/Limiti1991/Com1991/Com1991_WGS84.shp') |>
  st_transform("wgs84") 

# Use 2 different API's: for google, use usethis::edit_r_environ() and set GOOGLEGEOCODE_API_KEY variable
osm_geocoded <- italy_together |>
  tidygeocoder::geocode(address=location, method="osm")
google_geocoded <- italy_together |>
  tidygeocoder::geocode(address=location, method="google")

# Keep the geomatch from google if there, otherwise, use OSM
osm_geocoded <- osm_geocoded |>
  rename(lat2 = lat, long2 = long)

together <- google_geocoded |>
  left_join(osm_geocoded, by = c('year', 'comune_code', 'n', 'location', 'country')) |>
  mutate(lat = if_else(is.na(lat), lat2, lat),
         long = if_else(is.na(long), long2, long)) |>
  select(-c(lat2, long2)) |>
  distinct()

# Match to a municipality number

geocode_place <- function(row){
  lat <- row$lat
  long <- row$long
  out <- tryCatch({
    point <- st_point(c(long, lat))
    
    # Check which municipality contains this point
    out <- municipalities |>
      filter(st_contains(geometry, point, sparse = FALSE)) |>
      select(COMUNE, PRO_COM) |>
      st_drop_geometry()
  }, 
  error=function(e){
    out <- tibble(COMUNE=NA, PRO_COM=NA)
  })
  
  return(out)
}

together <- together |>
  rowwise() |>
  mutate(exp = list(geocode_place(pick(everything()))))
together <- together |> 
  unnest_wider(exp)


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

# Export the together dataset
# Assuming your tibble is named 'together'


# Write this to geojson
st_write(together_sf, "./data/italy_austrian_piedmontese_patents.geojson")
