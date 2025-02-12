#long_term_exhibitions
# 2. Exhibitions
# Patents 1855-1866 
library(fixest); library(tidyverse); library(modelsummary); library(tinytable)
source("./code/data_wrangling/data_wrangling_final_ds.R")
source('code/analysis/regression_settings.R')

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
The first 3 models report OLS estimates, the last three models report Poisson estimates. 
The estimates are conducted at the \\textit{Comune} level. 
The estimates control for population(Partial), in addition to area, distance to the border (Full). 
Heteroskedasticity-robust standard errors are clustered at the Comune-level. $*: p<0.1, **: p<0.05, ***: p<0.01$."

tt1 <- modelsummary(panel_a,
                    coef_map=coef_map_lt,
                    stars=c("*"=0.1, "**"=0.05, "***"=0.01),
                    gof_map = tibble(raw=c("adj.r.squared", "nobs"), 
                                     clean=c("Adj. $R^2$", "N"),
                                     fmt=c(3, 0)),
                    title="Estimates of Unification on Exhibition Activity\\label{tab:exhibition_long_term}",
                    estimate = "{estimate}{stars}",
                    notes = n2, 
                    output = "tinytable",
                    width=c(0.3, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1), 
                    add_rows = bind_rows(
                      as_tibble_row(c("Controls", "No", "Full", "Full", "No", "Partial", "Full"), .name_repair = "unique"),
                      as_tibble_row(c("Municipal FE", rep("No", 2), "Yes", rep("No", 3)), .name_repair = "unique")
                    )
                    ) |>
  group_tt(j=list("OLS"=2:4, "Poisson"=5:7)) |>
  save_tt("./tables/exhibitions_long_term.tex", overwrite = T)


