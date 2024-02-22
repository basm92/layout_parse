# Source for the shapefile
# https://rug.maps.arcgis.com/home/item.html?id=4aad29d1008649deb3217e85c1ef0bdd
library(sf); library(tidyverse); library(geodata)
rw <- st_read('./Downloads/it_sh/ItalyRailways.shp') |> st_as_sf()
munip <- geodata::gadm("Italy", level=2, path=tempdir()) |> st_as_sf()
st_crs(munip) <- "WGS84"

munip |> 
  ggplot() + geom_sf() +
  geom_sf(data=rw |> filter(YearConstr < 1870, YearConstr != 0), color = 'blue')
  
rw |>
  ggplot() + geom_sf()

