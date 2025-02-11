#long_term_exhibitions
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

# OLS
model1 <- feols(count_pc*1e6 ~  i(as.factor(year), allegiance_1861, ref="1855", ref2 = "Veneto") | year,
                data=final_filtered,
                vcov=~PRO_COM)
model2 <- feols(count_pc*1e6 ~  i(as.factor(year), allegiance_1861, ref="1855", ref2 = "Veneto") + area_of_intersection + abs_distance_to_border + interpolated_population | year,
              data=final_filtered,
              vcov=~PRO_COM)
model3 <- feols(count_pc*1e6 ~  i(as.factor(year), allegiance_1861, ref="1855", ref2 = "Veneto") + area_of_intersection + abs_distance_to_border + interpolated_population | as.factor(PRO_COM) + year,
                data=final_filtered,
                vcov=~year)

# Poisson
model4 <- fepois(count_pc*1e6 ~  i(as.factor(year), allegiance_1861, ref="1855", ref2 = "Veneto") | year,
                data=final_filtered,
                vcov=~PRO_COM)
model5 <- fepois(count_pc*1e6 ~  i(as.factor(year), allegiance_1861, ref="1855", ref2 = "Veneto") + interpolated_population | year,
                data=final_filtered,
                vcov=~PRO_COM)
model6 <- fepois(count_pc*1e6 ~  i(as.factor(year), allegiance_1861, ref="1855", ref2 = "Veneto") + area_of_intersection + abs_distance_to_border + interpolated_population | year,
                data=final_filtered,
                vcov=~year)


panel_a <- list(model1, model2, model3, model4, model5, model6)

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

