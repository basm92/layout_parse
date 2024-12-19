library(tidyverse); library(sf)
geofile <-   read_sf('./data/shapefiles_images/geofile.geojson')
source("./code/data_wrangling/data_wrangling_final_ds.R")

# Mutate
final <- final |>
  mutate(patents_together_verz_italy = if_else(is.na(patents_together_verz_italy), 0, patents_together_verz_italy),
         patents_together_verz_italy_pc = if_else(is.na(patents_together_verz_italy_pc), 0, patents_together_verz_italy_pc))


final |> 
  group_by(PRO_COM) |>
  summarize()