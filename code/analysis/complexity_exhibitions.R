# 2. Exhibitions
# Patents 1855-1866 
library(fixest); library(tidyverse); library(modelsummary); library(tinytable)
source("./code/data_wrangling/data_wrangling_final_ds.R")
bw <- 150000

final_filtered <- final |>
  filter(is.element(year, c(1822, 1833, 1844, 1855, 1867, 1878, 1889, 1900, 1911)),
         abs(running) < bw)

final_filtered |>
  group_by(allegiance_1861, year) |>
  summarize(cpc = sum(count, na.rm=T)) |>
  filter(is.element(year, c(1855, 1867, 1878, 1889, 1900, 1911)))

## Placebo's: before 1859 Annexation of Lombardy
patents1858 <- feols(patents_together_verz_italy_pc*1e7 ~ allegiance_1861, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                     vcov=~PRO_COM)
patents1858pois <- fepois(patents_together_verz_italy_pc*1e7 ~ allegiance_1861, 
                          data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                          vcov=~PRO_COM)
patents1858cv <- feols(patents_together_verz_italy_pc*1e7 ~ allegiance_1861 + interpolated_population + area_of_intersection + abs_distance_to_border, 
                       data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                       vcov=~PRO_COM)
patents1858poiscv <- fepois(patents_together_verz_italy_pc*1e7 ~ allegiance_1861 + area_of_intersection + abs_distance_to_border + interpolated_population, 
                            data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                            vcov=~PRO_COM)
patents1858cvfe <- feols(patents_together_verz_italy_pc*1e7 ~ allegiance_1861 + interpolated_population + area_of_intersection + abs_distance_to_border | year, 
                         data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                         vcov=~PRO_COM)
patents1858poiscvfe <- fepois(patents_together_verz_italy_pc*1e7 ~ allegiance_1861 + area_of_intersection + abs_distance_to_border + interpolated_population | year, 
                              data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                              vcov=~PRO_COM)
#modelsummary(list(patents1858, patents1858pois, patents1858cv, patents1858poiscv, patents1858cvfe, patents1858poiscvfe), stars=T)

## Real tests: After the 1859 Annexation of Lombardy
patents1867 <- feols(patents_together_verz_italy_pc*1e7 ~ allegiance_1861, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                     vcov=~PRO_COM)
patents1867pois <- fepois(patents_together_verz_italy_pc*1e7 ~ allegiance_1861, 
                          data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                          vcov=~PRO_COM)
patents1867cv <- feols(patents_together_verz_italy_pc*1e7 ~ allegiance_1861 + area_of_intersection + abs_distance_to_border + interpolated_population, 
                       data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                       vcov=~PRO_COM)
patents1867poiscv <- fepois(patents_together_verz_italy_pc*1e7 ~ allegiance_1861 + area_of_intersection + abs_distance_to_border + interpolated_population, 
                            data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                            vcov=~PRO_COM)
patents1867cvfe <- feols(patents_together_verz_italy_pc*1e7 ~ allegiance_1861 + area_of_intersection + abs_distance_to_border + interpolated_population | year, 
                         data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                         vcov=~PRO_COM)
patents1867poiscvfe <- fepois(patents_together_verz_italy_pc*1e7 ~ allegiance_1861 + area_of_intersection + abs_distance_to_border + interpolated_population | year, 
                              data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                              vcov=~PRO_COM)


panel_a <- list('OLS'=exhibitions1855, 'Poisson'=exhibitions1855pois, 'OLS'=exhibitions1855cv, 'Poisson'=exhibitions1855poiscv)
panel_b <- list('OLS'=exhibitions1867, 'Poisson'=exhibitions1867pois, 'OLS'=exhibitions1867cv, 'Poisson'=exhibitions1867poiscv)

n2 <- "Table reports estimates of the difference in exhibition count per 100,000 inhabitants in Lombardy relative to Veneto. 
Panel A focuses on the pre-unification period (the 1855 Exhibition), and 
Panel B on the unification period for Lombardy but not for Veneto (the 1867 Exhibition).  
The estimates are conducted at the \\textit{Comune} level. 
The estimates in Columns 1 and 3 are OLS estimates, and the estimates in Columns 2 and 4 are Poisson estimates. 
The estimates control for area, distance to the border, and population. 
Heteroskedasticity-robust standard errors are clustered at the province-level. $*: p<0.1, **: p<0.05, ***: p<0.01$."

coef_map <- c("allegiance_1861Veneto"="Veneto",
              "allegiance_1861Lombardia" = "Lombardia")

tt1 <- modelsummary(panel_a,
                    coef_map=coef_map,
                    stars=c("*"=0.1, "**"=0.05, "***"=0.01),
                    gof_map = tibble(raw=c("adj.r.squared", "nobs"), 
                                     clean=c("Adj. $R^2$", "N"),
                                     fmt=c(3, 0)),
                    title="Estimates of Unification on Exhibition Activity\\label{tab:exhibition}",
                    estimate = "{estimate}{stars}",
                    #notes = n, 
                    output = "tinytable",
                    width=c(0.3, 0.1, 0.1, 0.1, 0.1), 
                    add_rows = as_tibble_row(c("Controls", rep("No", 2), rep("Yes", 2)), .name_repair = "unique")
)

tt2 <-  modelsummary(panel_b,
                     coef_map=coef_map,
                     stars=c("*"=0.1, "**"=0.05, "***"=0.01),
                     gof_map = tibble(raw=c("adj.r.squared", "nobs"), 
                                      clean=c("Adj. $R^2$", "N"),
                                      fmt=c(3, 0)),
                     title="Testimates of Unification on Exhibition Activity\\label{tab:exhibition}",
                     estimate = "{estimate}{stars}",
                     notes = n2, 
                     output = "tinytable",
                     width=c(0.3, 0.1, 0.1, 0.1, 0.1), 
                     add_rows = as_tibble_row(c("Controls", rep("No", 2), rep("Yes", 2)), .name_repair = "unique")
) 


rbind2(tt1, tt2) |>
  group_tt(i=list("Panel A: Pre-Unification"=1, "Panel B: Post-Lombardy, Pre-Veneto Unification"=6)) |>
  style_tt(
    i=c(1, 7, 8), bold=T) |>
  style_tt(i = 6, line = "b") |>
  style_tt(i = 8, line = "b") |>
  style_tt(i = 13, line = "b") |>
  save_tt("./tables/complexity_exhibitions.tex", overwrite = T)

