# data_wrangling_final_ds
## In this file, on the basis of:
### ./data/shapefiles_images/geofile.geojson (resulting from data_wrangling_geofile.R)
### ./data/patents_final_dataset.csv (resulting from data_wrangling_patents.R)
### ./data/exhibitions_final_dataset.csv (resulting from data_wrangling_exhibitions.R)
### We build the final dataset for analysis integrating all of these to a municipality, year dataset
### And we add several control variables to this dataset
library(sf); library(tidyverse)
# Import the basic dataset
geofile <- sf::read_sf("./data/shapefiles_images/geofile.geojson") |>
  select(-c(POP_1991, contains("Shape"), overlap, CC_P))
# Create a grid for the patent years and exhibition years
years <- 1855:1911
base <- expand_grid(PRO_COM=geofile$PRO_COM, year=years)
base <- base |>
  left_join(geofile |> 
              st_drop_geometry() |> 
              select(PRO_COM, PRO_COM_T))

patents <- read_csv2("./data/patents_final_dataset.csv") |>
  select(PRO_COM, year, contains("patents"))
exhibitions <- read_csv2("./data/exhibitions_final_dataset.csv") |>
  select(PRO_COM_T, year, count, contains("complexity"))

# Add the dependent variables to the dataset
base <- base |>
  left_join(patents)
base <- base |>
  left_join(exhibitions)
# Now add the base dataset to the geofile to make the final dataset (without control variables)
final <- merge(geofile, base)

# Add zero's for observations we observe, but not for "gap years" in the exhibition
exhibition_years <- c(1855, 1867, 1878, 1889, 1900, 1911)
final <- final |> 
  mutate(across(contains("patents"), ~ if_else(is.na(.x), 0, .x))) |>
  mutate(count = if_else(is.element(year, exhibition_years) & is.na(count), 0, count))

coords <- geofile |>
  st_transform('wgs84') |>
  st_centroid() |> 
  mutate(longitude = st_coordinates(geometry)[,1],
         latitude = st_coordinates(geometry)[,2]) |>
  dplyr::select(PRO_COM, longitude, latitude) |>
  st_drop_geometry()

# Set the data to the correct years
final <- final |>
  left_join(coords,by="PRO_COM") |>
  mutate(allegiance_1861 = factor(allegiance_1861, levels=c("Veneto", "Lombardia")))

# Export dataset to csv
final |> 
  st_drop_geometry() |> 
  write_csv2("./data/final_dataset.csv")

