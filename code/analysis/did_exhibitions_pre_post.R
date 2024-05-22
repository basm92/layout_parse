#did_exhibitions_pre_post
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


# Merge it with the exhibitions data
exhibitions <- read_csv2('./data/final_datasets/italy.csv') 
to_be_merged <- dataset |>
  st_drop_geometry() |>
  select(LAU_ID, distance_to_navigable_river, elevation, ruggedness, agr_suit) |>
  distinct()
exhibitions <- exhibitions |>
  left_join(to_be_merged, by="LAU_ID")

exhibitions <- exhibitions |>
  mutate(post = if_else(year > 1866, 1, 0))

# Diff-in-diff analysis
formula_did <- as.formula(
  log(1+number_of_innovations) ~ i(group, ref="Veneto")*post
)

formula_did_controls <- as.formula(
  log(1+number_of_innovations) ~ i(group, ref="Veneto")*post +
    longitude + latitude + AREA_KM2 + MOUNT_TYPE + COAST_TYPE + URBN_TYPE + 
    angle_to_line + distance_to_navigable_river + elevation + ruggedness + agr_suit
)

fe <- formula_did <- as.formula(
  log(1+number_of_innovations) ~ i(group, ref="Veneto")*post | NUTS_ID
)

fe_controls <-  as.formula(
  log(1+number_of_innovations) ~ i(group, ref="Veneto")*post +
    longitude + latitude + AREA_KM2 + MOUNT_TYPE + COAST_TYPE + URBN_TYPE + 
    angle_to_line + distance_to_navigable_river + elevation + ruggedness + agr_suit | NUTS_ID
)

model1 <- feols(formula_did, data = exhibitions, vcov='hc1')
model2 <- feols(formula_did_controls, data = exhibitions, vcov='hc1')
model3 <- feols(fe, data = exhibitions, vcov ='hc1')
model4 <- feols(fe_controls, data = exhibitions, vcov='hc1')


# Table
notes <- "Dependent variables: Log (1 + Number of patents) in municipality $i$. 
The coefficient of interest is the Post x Treated variable.
The control variables are latitude, longitude, elevation, ruggedness, area, distance to river,
agricultural suitability (models 2 and 4) and regional-level coastal-type, urban-type and mountain-type (model 2). 
Heteroskedasticity-robust standard errors in parentheses."
notes <- gsub('[\n]', ' ', notes) |> str_squish()

rows <- tribble(~term,         ~"(1)", ~"(2)", ~"(3)", ~"(4)",
                'Region FE', "No", "No", "Yes", "Yes",
                'Controls', "No", "Yes", "No", "Yes")


knitr::opts_current$set(label = "did_exhibitions_pre_post")
modelsummary(
  list(model1, model2, model3, model4),
  stars=stars,
  gof_map = gm_did, 
  coef_map = cm_did,
  title = "Difference-in-difference Estimates of Unification on Patents",
  add_rows = rows,
  coef_omit = "Intercept",
  out = "kableExtra",
  output = "latex")   |>
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
  kableExtra::save_kable(file="./tables/did_exhibitions_pre_post.tex")


