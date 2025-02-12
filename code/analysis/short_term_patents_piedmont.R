# Patents 1855-1866 
library(fixest); library(tidyverse); library(modelsummary); library(tinytable)
source("./code/data_wrangling/data_wrangling_final_ds.R")
source('code/analysis/regression_settings.R')
# Set the global bandwidth
bw <- 100000

final <- final |>
  mutate(patents_italy_pc = if_else(is.na(patents_italy_pc), 0, patents_italy_pc))

## Placebo's: before 1859 Annexation of Lombardy
patents1858 <- feols(patents_italy_pc*1e7 ~ allegiance_1861, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                     vcov=~PRO_COM)
patents1858pois <- fepois(patents_italy_pc*1e7 ~ allegiance_1861, 
                          data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                          vcov=~PRO_COM)
patents1858cv <- feols(patents_italy_pc*1e7 ~ allegiance_1861 + interpolated_population + area_of_intersection + abs_distance_to_border, 
                       data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                       vcov=~PRO_COM)
patents1858poiscv <- fepois(patents_italy_pc*1e7 ~ allegiance_1861 + area_of_intersection + abs_distance_to_border + interpolated_population, 
                            data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                            vcov=~PRO_COM)
patents1858cvfe <- feols(patents_italy_pc*1e7 ~ allegiance_1861 + interpolated_population + area_of_intersection + abs_distance_to_border | year, 
                         data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                         vcov=~PRO_COM)
patents1858poiscvfe <- fepois(patents_italy_pc*1e7 ~ allegiance_1861 + area_of_intersection + abs_distance_to_border + interpolated_population | year, 
                              data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                              vcov=~PRO_COM)
#modelsummary(list(patents1858, patents1858pois, patents1858cv, patents1858poiscv, patents1858cvfe, patents1858poiscvfe), stars=T)

## Real tests: After the 1859 Annexation of Lombardy
patents1867 <- feols(patents_italy_pc*1e7 ~ allegiance_1861, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                     vcov=~PRO_COM)
patents1867pois <- fepois(patents_italy_pc*1e7 ~ allegiance_1861, 
                          data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                          vcov=~PRO_COM)
patents1867cv <- feols(patents_italy_pc*1e7 ~ allegiance_1861 + area_of_intersection + abs_distance_to_border + interpolated_population, 
                       data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                       vcov=~PRO_COM)
patents1867poiscv <- fepois(patents_italy_pc*1e7 ~ allegiance_1861 + area_of_intersection + abs_distance_to_border + interpolated_population, 
                            data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                            vcov=~PRO_COM)
patents1867cvfe <- feols(patents_italy_pc*1e7 ~ allegiance_1861 + area_of_intersection + abs_distance_to_border + interpolated_population | year, 
                         data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                         vcov=~PRO_COM)
patents1867poiscvfe <- fepois(patents_italy_pc*1e7 ~ allegiance_1861 + area_of_intersection + abs_distance_to_border + interpolated_population | year, 
                              data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                              vcov=~PRO_COM)

#modelsummary(list(patents1867, patents1867pois, patents1867cv, patents1867poiscv, patents1867cvfe, patents1867poiscvfe), stars=T)

# Table notes
n <- "Table reports estimates of the difference in patent count in Veneto relative to Lombardy. 
The dependent variable is patents per 100,000 population. 
Panel A focuses on the pre-unification period (1855 to 1858), and 
Panel B on the unification period for Lombardy (1860-1866), but not for Veneto. 
The estimates are conducted at the \\textit{Comune} level. 
The estimates in Columns 1, 3, and 5 are OLS estimates, and the estimates in Columns 2, 4, and 6 are Poisson estimates. 
The estimates control for area, distance to the border and population
, and Models 5 and 6 are also conditional on year fixed-effects. Heteroskedasticity-robust standard errors are clustered at the Comune-level. $*: p<0.1, **: p<0.05, ***: p<0.01$."

coef_map <- c("allegiance_1861Veneto"="Veneto",
              "allegiance_1861Lombardia" = "Lombardia")

panel_a <- list(
  #  "Panel A: Pre-Unification"
  'OLS'=patents1858,
  'Poisson'=patents1858pois,
  'OLS'=patents1858cv,
  'Poisson'=patents1858poiscv,
  'OLS'=patents1858cvfe,
  'Poisson'=patents1858poiscvfe)

panel_b <- list(
  #  "Panel B: Post-Lombardy Unification, Pre-Veneto Unification"=list(
  'OLS'=patents1867,
  'Poisson'=patents1867pois,
  'OLS'=patents1867cv,
  'Poisson'=patents1867poiscv,
  'OLS'=patents1867cvfe,
  'Poisson'=patents1867poiscvfe
)

tt1 <- modelsummary(panel_a,
                    coef_map=coef_map,
                    stars=c("*"=0.1, "**"=0.05, "***"=0.01),
                    gof_map = tibble(raw=c("adj.r.squared", "nobs"), 
                                     clean=c("Adj. $R^2$", "N"),
                                     fmt=c(3, 0)),
                    title="Estimates of Unification on Patenting Activity\\label{tab:patents_short_term_piedmont}",
                    estimate = "{estimate}{stars}",
                    #notes = n, 
                    output = "tinytable",
                    width=c(0.3, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1), 
                    add_rows = as_tibble_row(c("Year FE", rep("No", 4), rep("Yes", 2)), .name_repair = "unique") |>
                      bind_rows(as_tibble_row(c("Controls", rep("No", 2), rep("Yes", 4)), .name_repair = "unique"))
)

tt2 <-  modelsummary(panel_b,
                     coef_map=coef_map,
                     stars=c("*"=0.1, "**"=0.05, "***"=0.01),
                     gof_map = tibble(raw=c("adj.r.squared", "nobs"), 
                                      clean=c("Adj. $R^2$", "N"),
                                      fmt=c(3, 0)),
                     title="Testimates of Unification on Patenting Activity\\label{tab:patents_short_term_piedmont}",
                     estimate = "{estimate}{stars}",
                     notes = n, 
                     output = "tinytable",
                     width=c(0.3, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1), 
                     add_rows = as_tibble_row(c("Year FE", rep("No", 4), rep("Yes", 2)), .name_repair = "unique") |>
                       bind_rows(as_tibble_row(c("Controls", rep("No", 2), rep("Yes", 4)), .name_repair = "unique"))
) 

rbind2(tt1, tt2) |>
  group_tt(i=list("Panel A: Pre-Unification"=1, "Panel B: Post-Lombardy, Pre-Veneto Unification"=7)) |>
  style_tt(
    i=c(1, 8, 9), bold=T) |>
  style_tt(i = 3, line = "b") |>
  style_tt(i = 7, line = "b") |>
  style_tt(i = 11, line = "b") |>
  style_tt(i = 15, line = "b") |>
  save_tt("./tables/patents_short_term_piedmont.tex", overwrite = T)

