# Regression Discontinuity Analysis
library(tidyverse)
library(rdrobust)
library(fixest)
library(sf)
library(kableExtra)
library(knitr)
library(modelsummary)

source('code/analysis/regression_settings.R')

dataset <- read_csv2('./data/final_datasets/italy.csv') |>
  mutate(year = as.factor(year)) |>
  filter(year == 1878 | year == 1855)

dataset <- dataset |>
  group_by(LAU_ID) |>
  mutate(number_of_innovations = number_of_innovations[year == 1878] - number_of_innovations[year == 1855], ) |>
  group_by(LAU_ID) |>
  slice_min(year) |>
  ungroup()

controls <- c('AREA_KM2', 'angle_to_line', 'latitude', 'longitude', 'MOUNT_TYPE', 'COAST_TYPE', 'URBN_TYPE')

model1 <- rdd(data = dataset,
              control_variables = controls, 
              running_var = 'distance', 
              dep_var = 'number_of_innovations*1000')
model2 <- rdd(data = dataset, 
              control_variables = controls, 
              fixed_effects = 'NAME_LATN',
              running_var = 'distance', 
              dep_var = 'number_of_innovations*1000')
model3 <- rdd(data = dataset, 
              control_variables = controls, 
              running_var = 'distance', 
              dep_var = 'number_of_innovations*1000')
model4 <- rdd(data = dataset, 
              control_variables = controls, 
              fixed_effects = 'NAME_LATN',
              running_var = 'distance', 
              dep_var = 'number_of_innovations*1000')

models <- list(model1, model2, model3, model4)
knitr::opts_current$set(label = "rd_analysis_number_increase")
notes <- "Table showing coefficient estimates and bias-corrected standard errors 
of the impact of Italian Unification on innovative activity. The dependent variable 
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
  add_header_above(c(" " = 1, "Log Innovations" = 2, "Ihs Innovations" = 2)) |>
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
  kableExtra::save_kable("./tables/rd_analysis_number_increase.tex")

