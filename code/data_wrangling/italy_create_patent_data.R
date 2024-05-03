# italy_create_1856_1900_patents
library(sf); library(tidyverse)
# Import the border
border <- st_read('./data/shapefiles_images/old_province_borders_lines/shapefile_italy_1860.shp') |>
  filter(id == 5)

# Load the municipalities
italy_municipalities <- giscoR::gisco_get_lau(country='Italy', year='2016')

# Load the provinces - to filter the relevant municipalities
italy_provinces <- giscoR::gisco_get_nuts(country='Italy', nuts_level='3', year='2016') |> 
  mutate(prov_code =  str_sub(NUTS_ID, 1, 4)) |> 
  filter(is.element(prov_code, c('ITC4', 'ITH3')))

# Filter the relevant municipalities
# Combine overlaps and within
filter_indication <- st_within(italy_municipalities, italy_provinces, sparse=TRUE) 
filter_indication2 <- st_overlaps(italy_municipalities, italy_provinces, sparse=TRUE) 
mask <- apply(filter_indication, 1, any)
mask2 <- apply(filter_indication2, 1, any)
final_mask <- map2_lgl(mask, mask2, ~ any(.x + .y))
relevant_part <- italy_municipalities[final_mask,]

# Plot to check
ggplot() + geom_sf(data=relevant_part) + geom_sf(data=border, color='blue')

# Spatial join
relevant_part <- st_join(relevant_part, italy_provinces, join=st_nearest_feature)
# Calculate centroids of polygons in relevant_part
relevant_centroids <- st_centroid(relevant_part)

# Calculate nearest point on the line for each centroid
nearest_points <- st_nearest_points(relevant_centroids, border)

# Calculate the angle between the line connecting centroids and their nearest points on the border
angles <- numeric(length(nearest_points))
for (i in seq_along(nearest_points)) {
  centroid_coords <- st_coordinates(relevant_centroids[i,])
  nearest_coords <- st_coordinates(nearest_points[i])
  
  # Calculate the X and Y differences (endpoint - origin)
  delta_y <- nearest_coords[2, 2] - nearest_coords[1, 2]
  delta_x <- nearest_coords[2, 1] - nearest_coords[1, 1]
  
  # Calculate the angle using atan2
  angle_radians <- atan2(delta_y, delta_x)
  angles[i] <- angle_radians * (180 / pi)
}

# Add angles to relevant_part
relevant_part$angle_to_line <- angles
relevant_part$group <- if_else(abs(relevant_part$angle_to_line) > 90, "Veneto", "Lombardia")

# Isolate only the relevant variables
relevant_part <- relevant_part |>
  select(-c(contains(c("CNTR_CODE", "NUTS_NAME", "2016", "YEAR", "GISCO_ID"))))

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

# Make a rowwise() function to extract the patents in location i in quarter t
count_patents <- function(row, patents){
  date_start <- row$ds
  date_end <- row$de
  polygon <- row$`_ogr_geometry_`
  patents |> 
    filter(between(granted_on, date_start, date_end)) |>
    st_intersection(polygon) |>
    nrow()
}

pp <- polygon_panel |>
  rowwise() |> 
  mutate(patents = count_patents(cur_data(), patents))
