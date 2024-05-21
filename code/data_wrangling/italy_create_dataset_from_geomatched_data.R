# italy_create_dataset_from_geomatched_data.R
library(tidyverse)
library(sf)
library(rdrobust)
library(fixest)
library(modelr)
library(terra)
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

# Import the data and group by municipality
innovations_per_municipality_1867 <- read_csv2("./data/1867_italy_chatgpt_geomatched_manually_edited.csv") |> 
  group_by(LAU_ID) |> 
  count()

# Also include the 1878 
innovations_per_municipality_1878 <- read_csv2('./data/1878_italy_chatgpt_geomatched_manually_edited.csv') |>
  group_by(LAU_ID) |>
  count()

innovations <- relevant_part |> 
  left_join(innovations_per_municipality_1867, by = "LAU_ID") |>
  mutate(n_1867 = if_else(is.na(n), 0, n)) |>
  select(-n)

innovations <- innovations |>
  left_join(innovations_per_municipality_1878, by = "LAU_ID") |>
  mutate(n_1878 = if_else(is.na(n), 0, n)) |>
  select(-n)

dist <- st_distance(st_centroid(innovations$`_ogr_geometry_`), border) |> as.numeric()

innovations <- innovations |> 
  mutate(abs_distance = dist) |> 
  mutate(distance = as.numeric(if_else(group == "Lombardia", abs_distance, -abs_distance)))

## Add elevation
elevations <- geodata::elevation_30s("Italy", path='./')
values <- terra::extract(elevations, innovations)

mean_elevation_per_comune <- values|> as_tibble() |>
  group_by(ID) |> 
  summarize(mean = mean(ITA_elv_msk, na.rm=T)) |> 
  pull(mean)

innovations <- innovations |> 
  mutate(mean_elevation = mean_elevation_per_comune)

## Import 1855 data
data_1855 <- readxl::read_xlsx('./data/Exhibitions_Lombardo_Veneto.xlsx') |> 
  janitor::clean_names() |> 
  group_by(location) |> 
  count() |> 
  rename(n_1855 = n) |>
  ungroup()

innovations <- innovations |> 
  left_join(data_1855,
            by=c('LAU_NAME' = 'location')) |> 
  mutate(n_1855 = if_else(is.na(n_1855), 0, n_1855))

# Create the did dataset
dataset <- innovations |> 
  pivot_longer(cols=c(n_1855, n_1867, n_1878), 
               names_to = 'year',
               names_transform=~ as.numeric(str_extract(.x, "\\d{4}")), 
               values_to='number_of_innovations')

# Create the share variables
shares <- dataset |>
  st_drop_geometry() |>
  group_by(group, year) |>
  summarize(total = sum(number_of_innovations))

dataset <- dataset |> 
  left_join(shares, by = c('group', 'year')) |>
  mutate(share_in_group = number_of_innovations / total) |>
  select(-c(total))

# Create longitude and latitude variables
# Compute district centroids for Conley standard errors
coords <- dataset |> 
  st_centroid() |> 
  mutate(longitude = st_coordinates(`_ogr_geometry_`)[,1],
         latitude = st_coordinates(`_ogr_geometry_`)[,2]) |>
  dplyr::select(LAU_NAME, year, longitude, latitude) |>
  st_drop_geometry()

# Integrate them in dataset
# Standardize ratio for interpretability
dataset <- dataset |> 
  left_join(coords, by = c("LAU_NAME", "year"))

# Drop a couple of irrelevant variables
dataset <- dataset |> 
  select(-c(FID.y, CNTR_CODE.y)) |>
  rename(FID = FID.x, CNTR_CODE = CNTR_CODE.x)

# Because this is a "panel" dataset, we can not incorporate geometry
dataset <- dataset |>
  st_drop_geometry()

write_csv2(dataset, "./data/final_datasets/italy.csv")

