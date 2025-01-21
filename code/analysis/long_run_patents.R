#long_run_patents.R
#long_term_exhibitions
# 2. Exhibitions
# Patents 1855-1866 
library(fixest); library(tidyverse); library(modelsummary); library(tinytable)
source("./code/data_wrangling/data_wrangling_final_ds.R")
bw <- 150000

final <- final |> 
  mutate(patents_together_verz_italy = if_else(is.na(patents_together_verz_italy), 0, patents_together_verz_italy),
         patents_together_verz_italy_pc = if_else(is.na(patents_together_verz_italy_pc), 0, patents_together_verz_italy_pc))

# OLS
feols(patents_together_verz_italy_pc*1e6 ~  i(as.factor(year), allegiance_1861, ref="1867", ref2 = "Veneto")  + interpolated_population | year + as.factor(PRO_COM),
      data=final |>
        filter(
          is.element(year, c(1855, 1867, 1878, 1889, 1902, 1911)),
          abs(running) < 200000),
      vcov='hc1')

# How many patents in each of these years?
final |> 
  filter(is.element(year, c(1855, 1867, 1878, 1889, 1902, 1911))) |>
  group_by(year) |> 
  summarize(count = sum(patents_together_verz_italy))

# Poisson
exhibitions1855poiscv <- fepois(count_pc*1e6 ~ allegiance_1861 + interpolated_population + area_of_intersection + abs_distance_to_border, 
                                data = final |> filter(abs(running) < bw, is.element(year, 1855)),
                                vcov='hc1')
exhibitions1867poiscv <- fepois(count_pc*1e6 ~ allegiance_1861 + interpolated_population + area_of_intersection + abs_distance_to_border, 
                                data = final |> filter(abs(running) < bw, is.element(year, 1867)),
                                vcov='hc1')
exhibitions1878poiscv <- fepois(count_pc*1e6 ~ allegiance_1861 + interpolated_population + area_of_intersection + abs_distance_to_border, 
                                data = final |> filter(abs(running) < bw, is.element(year, 1878)),
                                vcov='hc1')
exhibitions1889poiscv <- fepois(count_pc*1e6 ~ allegiance_1861 + interpolated_population + area_of_intersection + abs_distance_to_border, 
                                data = final |> filter(abs(running) < bw, is.element(year, 1889)),
                                vcov='hc1')
exhibitions1900poiscv <- fepois(count_pc*1e6 ~ allegiance_1861 + interpolated_population + area_of_intersection + abs_distance_to_border, 
                                data = final |> filter(abs(running) < bw, is.element(year, 1900)),
                                vcov='hc1')
exhibitions1911poiscv <- fepois(count_pc*1e6 ~ allegiance_1861 + interpolated_population + area_of_intersection + abs_distance_to_border, 
                                data = final |> filter(abs(running) < bw, is.element(year, 1911)),
                                vcov='hc1')

panel_a <- list('1855'=exhibitions1855cv, '1867'=exhibitions1867cv, '1878'=exhibitions1878cv, 
                '1889'=exhibitions1889cv, '1900'=exhibitions1900cv, '1911'=exhibitions1911cv)

panel_b <- list('1855'=exhibitions1855poiscv, '1867'=exhibitions1867poiscv, '1878'=exhibitions1878poiscv, 
                '1889'=exhibitions1889poiscv, '1900'=exhibitions1900poiscv, '1911'=exhibitions1911poiscv)

n2 <- "Table reports estimates of the difference in exhibition count per 100,000 inhabitants in Lombardy relative to Veneto. 
Panel A reports OLS estimates, Panel B reports Poisson estimates in various exhibition years from 1855 to 1911. 
The estimates are conducted at the \\textit{Comune} level. 
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
                    title="Estimates of Unification on Exhibition Activity\\label{tab:exhibition_long_term}",
                    estimate = "{estimate}{stars}",
                    #notes = n, 
                    output = "tinytable",
                    width=c(0.3, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1), 
                    add_rows = as_tibble_row(c("Controls", rep("Yes", 6)), .name_repair = "unique")
)

tt2 <-  modelsummary(panel_b,
                     coef_map=coef_map,
                     stars=c("*"=0.1, "**"=0.05, "***"=0.01),
                     gof_map = tibble(raw=c("adj.r.squared", "nobs"), 
                                      clean=c("Adj. $R^2$", "N"),
                                      fmt=c(3, 0)),
                     title="Testimates of Unification on Exhibition Activity\\label{tab:exhibition_long_term}",
                     estimate = "{estimate}{stars}",
                     notes = n2, 
                     output = "tinytable",
                     width=c(0.3, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1), 
                     add_rows = as_tibble_row(c("Controls", rep("Yes", 6)), .name_repair = "unique")
) 


rbind2(tt1, tt2) |>
  group_tt(i=list("Panel A: OLS"=1, "Panel B: Poisson"=6)) |>
  style_tt(
    i=c(1, 7, 8), bold=T) |>
  style_tt(i = 6, line = "b") |>
  style_tt(i = 8, line = "b") |>
  style_tt(i = 13, line = "b") |>
  save_tt("./tables/exhibitions_long_term.tex", overwrite = T)

