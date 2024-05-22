#rd_patents_pre_post
#did_patents_pre_post
library(tidyverse)
library(fixest)
library(sf) 
library(geodata)
library(marginaleffects)
library(modelsummary)
library(knitr)
library(kableExtra)
library(osmdata)
library(GAEZr)

# Regression settings
source('./code/analysis/regression_settings.R')

# Data
dataset <- read_sf('./data/italy_austrian_piedmontese_patents.geojson') |>
  mutate(post = if_else(year > 1859 & ds > ymd("1859-06-01"), 1, 0),
         patents = patents*1000) 

# Add some controls
coords <- dataset |> 
  st_centroid() |>
  mutate(longitude = st_coordinates(geometry)[,1],
         latitude = st_coordinates(geometry)[,2]) |>
  dplyr::select(LAU_ID, longitude, latitude) |>
  st_drop_geometry() |>
  unique()

dataset <- left_join(dataset, coords, by = "LAU_ID")

# Distance to river
rivers_sf <- st_read(file.path("./data/shapefiles_images/rivers/", "HydroRIVERS_v10_eu.gdb")) |>
  st_filter(dataset)
rivers_sf_filtered <- rivers_sf |>
  filter(ORD_FLOW < 5)
distance_to_navigable_river <- dataset |>
  st_distance(rivers_sf_filtered)
distance_to_navigable_river <- apply(distance_to_navigable_river, 1, min)
dataset <- dataset |>
  mutate(distance_to_navigable_river = distance_to_navigable_river)

# Elevation
mean_rm <- function(x){mean(x, na.rm=T)}
elevation_italy <- geodata::elevation_30s("Italy", path=tempdir())
values <- terra::extract(elevation_italy, dataset)
elevation <- values |>
  group_by(ID) |>
  summarize(elevation = mean(ITA_elv_msk, na.rm=T)) |>
  pull(elevation)

# Ruggedness
ruggedness_italy <- terra::terrain(elevation_italy, v="TRI")
values <- terra::extract(ruggedness_italy, dataset)
ruggedness <- values |>
  group_by(ID) |>
  summarize(ruggedness = mean(TRI, na.rm=T)) |>
  pull(ruggedness)

dataset <- dataset |>
  mutate(elevation = elevation,
         ruggedness = ruggedness)

# Agricultural suitability
agr_suit <- terra::rast('./data/shapefiles_images/suit/w001001.adf')
values <- terra::extract(agr_suit, dataset)

agr_suit <- values |>
  group_by(ID) |>
  summarize(agr_suit = mean(w001001, na.rm=T)) |>
  pull(agr_suit)

dataset <- dataset |>
  mutate(agr_suit = agr_suit)


distance_to_border <- read_csv2('./data/final_datasets/italy.csv') |>
  select(id, distance) |>
  distinct()

# Merge it
dataset <- dataset |>
  left_join(distance_to_border, by = 'id')

# Collapse the dataset
dataset_collapsed <- dataset |>
  st_drop_geometry() |>
  group_by(LAU_ID, post) |>
  reframe(
    group = group,
    NAME_LATN = NAME_LATN,
    patents = sum(patents),
    distance = mean(distance),
    AREA_KM2 = mean(AREA_KM2),
    angle_to_line = mean(angle_to_line),
    latitude = mean(latitude),
    longitude = mean(longitude),
    COAST_TYPE = mean(COAST_TYPE),
    MOUNT_TYPE = mean(MOUNT_TYPE),
    URBN_TYPE = mean(URBN_TYPE),
    distance_to_navigable_river = mean(distance_to_navigable_river),
    elevation = mean(elevation),
    ruggedness = mean(ruggedness),
    agr_suit = mean(agr_suit)) |>
  distinct()

dataset_dv <- dataset_collapsed |>
  group_by(LAU_ID) |>
  mutate(patents_dv = patents[post == 1] - patents[post == 0]) |>
  ungroup() |>
  select(-post) |>
  distinct()

# Analysis
controls <- c('AREA_KM2', 'angle_to_line', 'latitude', 
              'longitude', 'MOUNT_TYPE', 'COAST_TYPE', 'URBN_TYPE',
              'distance_to_navigable_river', 'elevation', 'ruggedness',
              'agr_suit')

model1 <- rdd(data = dataset_dv, 
              control_variables = controls, 
              running_var = 'distance', 
              dep_var = 'patents_dv')
model2 <- rdd(data = dataset_dv, 
              control_variables = controls, 
              fixed_effects = 'NAME_LATN',
              running_var = 'distance', 
              dep_var = 'patents_dv')
model3 <- rdd(data = dataset_dv, 
              control_variables = controls, 
              running_var = 'distance', 
              dep_var = 'ihs(1+patents_dv)')
model4 <- rdd(data = dataset_dv, 
              control_variables = controls, 
              fixed_effects = 'NAME_LATN',
              running_var = 'distance', 
              dep_var = 'ihs(1+patents_dv)')


models <- list(model1, model2, model3, model4)
knitr::opts_current$set(label = "rd_patents_pre_post")
notes <- "Table showing coefficient estimates and bias-corrected standard errors 
of the impact of Italian Unification on patents. The dependent variable 
is log or ihs no. of innovations and the independent (running) variable is 
distance to the border. The bandwidth is estimated using the MSE-optimal 
bandwidth from \\\\cite{cattaneo2019practical}. The estimates control for area, 
angle to border and innovation. Models (2) and (4) are conditional on province 
fixed effects. *: p < 0.10, **: p < 0.05, ***: p < 0.01."
notes <- notes |> str_remove_all("\n")

make_table(models,
           out = "kableExtra",
           output = "latex",
           title = "Estimates of Italian Unification on Innovative Activity") |>
  add_header_above(c(" " = 1, "No FE" = 1, "FE" = 1, "No FE" = 1, "FE" = 1)) |>
  add_header_above(c(" " = 1, "Count Innovations" = 2, "Ihs Innovations" = 2)) |>
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
  kableExtra::save_kable("./tables/rd_patents_pre_post.tex")
