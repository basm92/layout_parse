# Diff in diff analysis
library(tidyverse)
library(fixest)
library(sf) 
library(marginaleffects)
library(modelsummary)
library(knitr)
library(kableExtra)

# Regression settings
source('./code/analysis/regression_settings.R')

# Data
dataset <- st_read('./data/final_datasets/italy.geojson') |>
  mutate(year = as.factor(year))

# Diff-in-diff analysis
formula_did <- as.formula(
  log(1+number_of_innovations) ~ year + group + year*group + 
    longitude + latitude + mean_elevation + AREA_KM2 + angle_to_line | NUTS_NAME 
  )

formula_did_no_fe <- as.formula(
  log(1+number_of_innovations) ~ year + group + year*group + 
    longitude + latitude + mean_elevation + AREA_KM2 + angle_to_line
)

model_lin <- feols(formula_did_no_fe, data = dataset, vcov=~id)
model_lin2 <- feols(formula_did, data = dataset, vcov=~id)
model_pois <- fepois(formula_did_no_fe, data = dataset, vcov =~id)
model_pois2 <- fepois(formula_did, data = dataset, vcov =~id)


# Table
notes <- "Dependent variables: Number of innovations in municipality $i$. 
The coefficient of interest is the Year x Group{Veneto} variable.
The control variables are latitude, longitude, elevation, and the 
analysis is conditional on provinde fixed-effects. Standard errors are clustered at 
the municipality level."
notes <- gsub('[\n]', ' ', notes) |> str_squish()

rows <- tribble(~term,         ~"(1)", ~"(2)", ~"(3)", ~"(4)",
                'Province FE', "No", "Yes", "No", "Yes")


knitr::opts_current$set(label = "did_analysis_number")
modelsummary(
  list(model_lin,
       model_lin2,
       model_pois,
       model_pois2),
  stars=stars,
  gof_map = gm_did, 
  coef_map = cm_did,
  title = "Difference-in-difference Estimates of Unification on Innovation",
  add_rows = rows,
  coef_omit = "Intercept",
  out = "kableExtra",
  output = "latex")   |>
  kableExtra::add_header_above(c(" " = 1, "OLS" = 2, "Poisson" = 2)) |>
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size=9) |>
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
  kableExtra::save_kable(file="./tables/did_analysis_number.tex")

