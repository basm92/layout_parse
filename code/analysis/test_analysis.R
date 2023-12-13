library(tidyverse); library(sf); library(rdrobust)
border <- st_read('./shapefiles_images/old_province_borders_lines/shapefile_italy_1860.shp') |>
  filter(id == 5)
border |> ggplot() + geom_sf()

# Check spatial relationship between polygons and border
relation <- st_relate(polygons, border, pattern = "F**1****")

# Create a new variable in polygons indicating right/left relationship
polygons$right_left <- ifelse(relation == "F**1****", "Right", "Left")

# Write the updated polygons shapefile
st_write(polygons, "path_to_output_shapefile.shp")  # Repla