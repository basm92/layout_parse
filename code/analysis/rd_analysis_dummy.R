#rd_analysis_dummy
library(tidyverse)
library(rdrobust)
library(fixest)
library(sf)
library(kableExtra)
library(knitr)
library(modelsummary)

source('code/analysis/regression_settings.R')

dataset <- st_read('./data/final_datasets/italy.geojson') |>
  mutate(year = as.factor(year),
         dummy = if_else(number_of_innovations > 0, 1, 0)) |>
  filter(year == 1867)

controls <- c('AREA_KM2', 'angle_to_line', 'latitude', 'longitude', 'mean_elevation')

model1 <- rdd(data = dataset, 
              control_variables = controls, 
              running_var = 'distance', 
              dep_var = 'dummy')
model2 <- rdd(data = dataset, 
              control_variables = controls, 
              fixed_effects = 'NAME_LATN',
              running_var = 'distance', 
              dep_var = 'dummy')

models <- list(model1, model2)
knitr::opts_current$set(label = "rd_analysis_dummy")
notes <- "Table showing coefficient estimates and bias-corrected standard errors 
of the impact of Italian Unification on innovative activity. The dependent variable 
is Dummy(no. of innovations > 0) and the independent (running) variable is 
distance to the border. The bandwidth is estimated using the MSE-optimal 
bandwidth from \\\\cite{cattaneo2019practical}. The estimates control for area, 
angle to border and innovation. Models (2) and (4) are conditional on province 
fixed effects. *: p < 0.10, **: p < 0.05, ***: p < 0.01."
notes <- notes |> str_remove_all("\n")

make_table(models,
           out = "kableExtra",
           output = "latex",
           title = "Estimates of Italian Unification on Innovative Activity") |>
  add_header_above(c(" " = 1, "No FE" = 1, "FE" = 1)) |>
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
  kableExtra::save_kable("./tables/rd_analysis_dummy.tex")

