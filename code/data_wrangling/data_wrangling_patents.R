# italy_create_1856_1900_patents
library(sf); library(tidyverse); library(selenider); library(rvest); library(osmdata); library(tidygeocoder)

# Scrape Austrian Patents
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

# Scrape the metadata for all URLS
## This is better done in batches 
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

# The output has to be cleaned
#data_for_urls |> writeRDS("./data/patent_data/interim_patent_data/austrian_patent_data.RDS")
#data_for_urls <- readRDS("./data/patent_data/interim_patent_data/austrian_patent_data.RDS")
data_for_urls |>
  list_rbind()

clean <- function(df){
  if(class(df$`Shelfmark:`)=="list"){
    out <- unnest(df) |>
      distinct()
  } else{
    out <- df
  }
  return(out)
}

data_for_urls <- data_for_urls |>
  map(clean) |>
  list_rbind()

data_for_urls <- data_for_urls |>
  janitor::clean_names() |>
  mutate(across(everything(), ~ str_remove(.x, ":\n")))

# Geocode the Obtained Data Frame - Geocode only Italy
## OSM
austrian_geocoded_osm <- data_for_urls |>
  select(shelfmark, country, granted_on, place_of_publication) |> 
  filter(str_detect(country, "Italy")) |>
  mutate(temp = paste0(place_of_publication, ", ", country)) |>
  tidygeocoder::geocode(address=temp, method="osm")
## Google
austrian_geocoded_google <- data_for_urls |>
  select(shelfmark, country, granted_on, place_of_publication) |> 
  filter(str_detect(country, "Italy")) |>
  mutate(temp = paste0(place_of_publication, ", ", country)) |>
  tidygeocoder::geocode(address=temp, method="google")

austrian_geocoded_patents_italy <- left_join(austrian_geocoded_google, austrian_geocoded_osm,
          by = c("shelfmark", "country", "granted_on", "place_of_publication", "temp")) |>
  mutate(lat = if_else(is.na(lat.x), lat.y, lat.x),
         long = if_else(is.na(long.x), long.y, long.x)) |>
  mutate(lat = if_else(str_detect(place_of_publication, "-"), NA, lat),
         long = if_else(str_detect(place_of_publication, "-"), NA, long)) |>
  select(-c(lat.x, long.x, lat.y, long.y)) |>
  mutate(year = as.numeric(str_extract(granted_on, "\\d{4}")))

# Use the municipalities data.frame to put this inside a polygon
sf::sf_use_s2(FALSE)
municipalities <- read_sf('./shapefiles_images/italy_admin_borders/Limiti1991/Com1991/Com1991_WGS84.shp') |>
  st_transform("wgs84") 

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

austrian_geocoded_patents_italy <- austrian_geocoded_patents_italy |>
  rowwise() |>
  mutate(exp = list(geocode_place(pick(everything()))))

austrian_geocoded_patents_italy <- austrian_geocoded_patents_italy |>
  unnest_wider(exp)

# Put in the form Comune, year, count:
austrian_geocoded_patents_italy <- austrian_geocoded_patents_italy |>
  group_by(PRO_COM, COMUNE, year) |>
  count()

# Write to csv
#write_csv2(austrian_geocoded_patents_italy, 
#           "./data/patent_data/interim_patent_data/austrian_patent_data_cleaned_geocoded.csv")

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

# Write to csv
#together |>
#  write_csv2("./data/patent_data/interim_patent_data/italian_patent_data_cleaned_geocoded.csv")
# read_csv2("./data/patent_data/interim_patent_data/italian_patent_data_cleaned_geocoded.csv")

# 3. Incorporate the Piedmontese/Italian patents and the Austrian patents together
italy <- read_csv2("./data/patent_data/interim_patent_data/italian_patent_data_cleaned_geocoded.csv")
austria <- read_csv2("./data/patent_data/interim_patent_data/austrian_patent_data_cleaned_geocoded.csv")

patents_together <- full_join(italy, austria,
          by = c("year", "PRO_COM", "COMUNE")) |>
  rename(patents_italy = n.x, 
         patents_austria = n.y) |>
  mutate(patents_italy = if_else(is.na(patents_italy), 0, patents_italy),
         patents_austria = if_else(is.na(patents_austria), 0, patents_austria),
         patents_together = patents_italy + patents_austria)

# Write this to final dataset
patents_together <- patents_together |>
  select(year, location, COMUNE, lat, long, PRO_COM, comune_code, patents_austria, patents_italy, patents_together)
write_csv2(patents_together, "./data/patents_final_dataset.csv")
