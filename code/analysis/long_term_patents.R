#long_run_patents.R
#long_term_exhibitions
# 2. Exhibitions
# Patents 1855-1866 
library(fixest); library(tidyverse); library(modelsummary); library(tinytable)
source("./code/data_wrangling/data_wrangling_final_ds.R")
source('code/analysis/regression_settings.R')
#bw <- 150000

# How many patents do we have in total in each year?
total_patents_per_year <- final |>
  group_by(year) |>
  summarize(sum_p = sum(patents_together_verz_italy))

# Just Years
model1 <- feols(patents_together_verz_italy_pc*1e6 ~  i(as.factor(year), allegiance_1861, ref="1855", ref2 = "Veneto") | year,
      data=final |> filter(is.element(year, c(1822, 1833, 1844, 1855, 1867, 1878, 1889, 1902, 1911))),
      vcov='hc1')

model2 <- feols(patents_together_verz_italy_pc*1e6 ~  i(as.factor(year), allegiance_1861, ref="1855", ref2 = "Veneto") + interpolated_population | year + as.factor(PRO_COM),
                data=final |> filter(is.element(year, c(1822, 1833, 1844, 1855, 1867, 1878, 1889, 1902, 1911))),
                vcov='hc1')

model3 <- fepois(patents_together_verz_italy_pc*1e6 ~  i(as.factor(year), allegiance_1861, ref="1855", ref2 = "Veneto") + interpolated_population | year,
                data=final |> filter(is.element(year, c(1822, 1833, 1844, 1855, 1867, 1878, 1889, 1902, 1911))),
                vcov='hc1')

# Years +- To Eliminate Noise
model4 <- feols(patents_together_verz_italy_pc*1e6 ~  i(as.factor(year_group), allegiance_1861, ref="1855", ref2 = "Veneto") | year_group,
                data=final |> filter(!is.na(year_group)),
                vcov='hc1')

model5 <- feols(patents_together_verz_italy_pc*1e6 ~  i(as.factor(year_group), allegiance_1861, ref="1855", ref2 = "Veneto") + interpolated_population | year_group + as.factor(PRO_COM),
              data=final |> filter(!is.na(year_group)),
              vcov='hc1')

model6 <- fepois(patents_together_verz_italy_pc*1e6 ~  i(as.factor(year_group), allegiance_1861, ref="1855", ref2 = "Veneto") + interpolated_population | year_group,
                data=final |> filter(!is.na(year_group)),
                vcov='hc1')


panel_a <- list(model1, model2, model3, model4, model5, model6)
n2 <- "Table reports estimates of the difference in patent count per 100,000 inhabitants in Lombardy relative to Veneto. 
Equations 1,2, 4 and 5 report OLS estimates unconditional (1 and 4), and with municipal fixed effects (2 and 5). 
Additionally, Models 2 and 5 control for population. Equations 3 and 6 report Poisson estimates without municipal FE.
The estimates are conducted at the \\textit{Comune} level. 
Heteroskedasticity-robust standard errors in parentheses. $*: p<0.1, **: p<0.05, ***: p<0.01$."

tt1 <- modelsummary(panel_a,
                    coef_map=coef_map_lt,
                    stars=c("*"=0.1, "**"=0.05, "***"=0.01),
                    gof_map = tibble(raw=c("adj.r.squared", "nobs"), 
                                     clean=c("Adj. $R^2$", "N"),
                                     fmt=c(3, 0)),
                    title="Estimates of Unification on Long-Run Patenting Activity\\label{tab:patents_long_term}",
                    estimate = "{estimate}{stars}",
                    notes = n2, 
                    output = "tinytable",
                    width=c(0.3, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1), 
                    add_rows = bind_rows(
                      as_tibble_row(c("Controls", rep("Yes", 6)), .name_repair = "unique"),
                      as_tibble_row(c("Comune FE", rep(c("No", "Yes", "No"), 2)), .name_repair = "unique"))
) |>
  save_tt("./tables/patents_long_term.tex")

