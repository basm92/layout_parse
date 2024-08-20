library(sf); library(tidyverse)

## Part 1: Create the border
compartimenti_1861 <- read_sf('./shapefiles_images/italy_admin_borders/Limiti_1861/Compartimenti_1861/Compartimenti_1861.shp')
compartimenti_1871 <- read_sf('./shapefiles_images/italy_admin_borders/Limiti_1871/Compartimenti_1871/Compartimenti_1871.shp')
province_1861 <- read_sf('./shapefiles_images/italy_admin_borders/Limiti_1861/Province_1861/Province_1861.shp')
province_1871 <- read_sf('./shapefiles_images/italy_admin_borders/Limiti_1871/Province_1871/Province_1871.shp')
circondari_1861 <- read_sf('./shapefiles_images/italy_admin_borders/Limiti_1861/Circondari_1861/Circondari_1861.shp')
circondari_1871 <- read_sf('./shapefiles_images/italy_admin_borders/Limiti_1871/Circondari_ 1871/Circondari_1871.shp')
communi_1991 <- read_sf('./shapefiles_images/italy_admin_borders/Limiti1991/Com1991/Com1991_WGS84.shp')

# Border in two ways: North of Mantova, rely on intersection between Veneto and Lombardia
north_border <- st_intersection(compartimenti_1861 |>
                                  filter(COD_COMP == 3), 
                                compartimenti_1871 |> 
                                  filter(COD_COMP == 5))

# Border in two ways: South of Mantova, explicitly select the Communi
province_mantova <- province_1871[province_1871$DEN_PROV == "Mantova", ]
communi_mantova <- st_intersection(communi_1991, province_mantova)
communi_mantova <- communi_mantova |>
  mutate(border_municipality = if_else(is.element(COMUNE, 
                                                  c("Ponti sul Mincio", "Monzambano",
                                                    "Volta Mantovana", "Goito", "Rodigo", 
                                                    "Castellucchio", "Marcaria",
                                                    "Viadana", "Dosolo")), 
                                       TRUE,
                                       FALSE))

coordinates <- communi_mantova |> 
  st_centroid() |>
  select(geometry) |>
  st_coordinates() %>%
  as.data.frame()

communi_mantova <- communi_mantova |>
  mutate(coordinate_x = coordinates$X, 
         coordinate_y = coordinates$Y)

communi_mantova <- communi_mantova |>
  mutate(in_old_italy = if_else(coordinate_x < max(coordinate_x[border_municipality]) | border_municipality,
                                1,
                                0))

communi_mantova |> ggplot(aes(fill=in_old_italy)) + geom_sf()

# Compute the Mantova Border as the intersection between the two parts
south_border <- st_intersection(communi_mantova |> filter(in_old_italy == 1) |> st_boundary(),
                communi_mantova |> filter(in_old_italy == 0) |> st_boundary()) |>
  st_cast("MULTILINESTRING") 

# Append it with a small part of intersection between Mantova and Verona
missing_part <- st_intersection(communi_mantova |> filter(in_old_italy == 1),
                province_1871 |> filter(DEN_PROV == "Verona")) 

# Unite the border with the North Border
final_border <- st_union(north_border, south_border) |>
  st_union(missing_part) |>
  st_union()

# Save the border as a shapefile
st_write(final_border, "final_border.geojson")
#final_border <- read_sf("shapefiles_images/final_border.geojson")

## Part 2: Create the map with the different administrational units
# Start off with the selecting the relevant communes

## Match Compartimenti to Province
compartimenti_1871_updated <- compartimenti_1871 |>
  filter(is.element(DEN_COMP, c("Lombardia", "Veneto")))

province_compartimenti_1871 <- st_join(province_1871, 
                                       compartimenti_1871_updated,
                                       join = st_within) |>
  filter(!is.na(COD_COMP))

# Match Province (+ Compartimenti) to Circondari
circondari_province_compartimenti_1871 <- st_join(circondari_1871, province_compartimenti_1871, join = st_within) |>
  filter(!is.na(COD_PROV))

circondari_province_compartimenti_1871 |> ggplot() + geom_sf()

# Match Circondari-Province-Compartimenti to Commune
## Filter only the relevant communes
communi_1991_lv <- communi_1991 |>
  filter(is.element(COD_REG, c(3,5,6))) 
communi_1991_lv |> 
  ggplot() + geom_sf()
## Match the circondari-province-compartimenti based on surface overlap
matched <- communi_1991_lv |>
  st_intersection(circondari_province_compartimenti_1871) |>
  mutate(area_of_intersection = as.numeric(st_area(geometry))) |>
  group_by(PRO_COM, COMUNE) |>
  mutate(overlap = area_of_intersection/Shape_Area) |>
  filter(overlap == max(overlap), overlap > 0.95)

ggplot() + 
  geom_sf(data=matched, color='green') +
  geom_sf(data=circondari_province_compartimenti_1871, alpha=0.01,color='orange')

# Clean and exclude these circondari:
exclude <- c("Lomellina", "Voghera", "Bobbio")
matched <- matched |>
  select(-c(COMUNE_A, COD_CIRC, COD_PROV.1, COD_COMP)) |>
  ungroup() |>
  filter(!is.element(DEN_CIRC, exclude))

matched |> 
  ggplot(aes(fill=DEN_COMP)) + geom_sf()

## Part 3: Create distance to the border
centroids_of_municipalities <- matched |> 
  st_centroid()
nearest_points <- st_nearest_points(centroids_of_municipalities, final_border)

# Calculate the angle between the line connecting centroids and their nearest points on the border
angles <- numeric(length(nearest_points))
for (i in seq_along(nearest_points)) {
  centroid_coords <- st_coordinates(centroids_of_municipalities[i,])
  nearest_coords <- st_coordinates(nearest_points[i])
  
  # Calculate the X and Y differences (endpoint - origin)
  delta_y <- nearest_coords[2, 2] - nearest_coords[1, 2]
  delta_x <- nearest_coords[2, 1] - nearest_coords[1, 1]
  
  # Calculate the angle using atan2
  angle_radians <- atan2(delta_y, delta_x)
  angles[i] <- angle_radians * (180 / pi)
}

matched <- matched |>
  mutate(angle_to_line = angles,
         allegiance_1861 = if_else(abs(angle_to_line) > 90, "Veneto", "Lombardia"))

ggplot() + 
  geom_sf(data = matched, aes(fill=DEN_COMP, color=allegiance_1861), alpha=0.4) + 
  scale_color_viridis_d(option = 'E')

# Create distance to the border
matched <- matched |>
  mutate(
    abs_distance_to_border = as.numeric(st_distance(matched, final_border)),
    running = if_else(allegiance_1861 == "Lombardia", 
                      abs_distance_to_border,
                      -abs_distance_to_border
                      ))

matched |> ggplot() + geom_sf(aes(fill=running))

## Part 4: 
matched |> 
  write_sf('./data/shapefiles_images/geofile.geojson')

#matched <- read_sf('./data/shapefiles_images/geofile.geojson')
