library(tidyverse); library(sf); library(rdrobust); library(fixest); library(modelr)
# Import the border
border <- st_read('./shapefiles_images/old_province_borders_lines/shapefile_italy_1860.shp') |>
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
.
# Plot to check
ggplot() + geom_sf(data=relevant_part) + geom_sf(data=border, color='blue')

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
  
  
  # Calculate the angle using atan2
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
innovations_per_municipality <- read_csv2("./data/1867_italy_chatgpt_geomatched_manually_edited.csv") |> 
  group_by(LAU_ID) |> 
  count()

innovations <- relevant_part |> 
  left_join(innovations_per_municipality, by = "LAU_ID") |>
  mutate(n = if_else(is.na(n), 0, n))

innovations <- innovations |> 
  mutate(abs_distance = st_distance(st_centroid(innovations$`_ogr_geometry_`), border)) |> 
  mutate(distance = as.numeric(if_else(group == "Lombardia", -abs_distance, abs_distance)))

innovations$depres <- feols(log(1+n) ~ NAME_LATN, data = innovations)$resid

rdrobust::rdrobust(y=innovations$depres, x=innovations$distance, covs=innovations$AREA_KM2) |> summary()
rdplot(y=innovations$depres, x=innovations$distance)
