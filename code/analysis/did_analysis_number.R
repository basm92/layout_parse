# Diff in diff analysis
library(tidyverse)
library(rdrobust)
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
  number_of_innovations ~ year + group + year*group + 
    longitude + latitude + mean_elevation + AREA_KM2 + angle_to_line | NUTS_NAME 
  )

model_lin <- feols(formula_did, data = dataset, vcov=~id)
model_pois <- fepois(formula_did, data = dataset, vcov =~id)


# Table
notes <- "Dependent variables: Number of innovations in municipality $i$. 
The coefficient of interest is the Year x Group{Veneto} variable.
The control variables are latitude, longitude, elevation, and the 
analysis is conditional on provinde fixed-effects. Standard errors are clustered at 
the municipality level. The F Statistic and p-value report the outcome of 
a test for the equality $\\beta_1=\\beta_6$." 
notes <- gsub('[\n]', ' ', notes) |> str_squish()

rows <- tribble(~term,         ~"(1)", ~"(2)",
                'Province FE', '\\Checkmark', '\\Checkmark')


knitr::opts_current$set(label = "did_analysis_number")
modelsummary(
  list(model_lin,
       model_pois),
  stars=stars,
  gof_map = gm_did, 
  coef_map = cm_did,
  title = "Difference-in-difference Estimates of Unification on Innovation",
  add_rows = rows,
  coef_omit = "Intercept",
  out = "kableExtra",
  output = "latex")   |>
  kableExtra::add_header_above(c(" " = 1, "OLS" = 1, "Poisson" = 1)) |>
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size=9) |>
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F) |>
  kableExtra::save_kable(file="./tables/did_analysis_number.tex")

