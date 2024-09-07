# Long Run: Patents and Exhibitions
library(fixest); library(tidyverse)
source("./code/data_wrangling/data_wrangling_final_ds.R")

# Set the global bandwidth
bw <- 100000

## DV: Log(PatentCount_{Italy, Austria}), 
## Part 1: Patents: {1855-1867-1878-1890-1902-1911}
## Also idea: Make use of the 5 preceding years
patents1855 <- feols(patents_together*1e4 ~  allegiance_1861 + area_of_intersection + running | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1855)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)
patents1867 <- feols(patents_together*1e4 ~ allegiance_1861 + area_of_intersection + running | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1867)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)
patents1878 <- feols(patents_together*1e4 ~ allegiance_1861 + area_of_intersection + running  | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1878)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)
patents1889 <- feols(patents_together*1e4 ~ allegiance_1861 + area_of_intersection + running   | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1889)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)
patents1902 <- feols(patents_together*1e4 ~ allegiance_1861 + area_of_intersection + running   | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1902)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)

coef_map <- c("allegiance_1861Veneto"="Veneto")

# Table notes
n <- "Table reports estimates of the difference in patent count in Veneto relative to Lombardy, 
focusing on 5 years for which patent counts are available: 1855, 1867, 1878, 1889, and 1900. 
The coefficient size is multiplied by 1000 for interpretability. 
The coefficients are estimated by OLS with a bandwidth of 100 kilometer, and are conducted at the \\textit{Comune} level. 
The estimates are weighted by the inverse absolute distance to the border, and control for area and distance to the border, 
and are also conditional on year fixed-effects. Heteroskedasticity-robust standard errors are clustered at the province-level. $*: p<0.1, **: p<0.05, ***: p<0.01$."


panels <- list(
  '1855'=patents1855, 
  '1867'=patents1867, 
  '1878'=patents1878,
  '1889'=patents1889,
  '1902'=patents1902)

modelsummary(panels,
             coef_map=coef_map,
             stars=c("*"=0.1, "**"=0.05, "***"=0.01),
             gof_map = tibble(raw=c("adj.r.squared", "nobs"), 
                              clean=c("Adj. $R^2$", "N"),
                              fmt=c(3, 0)),
             title="Estimates of Unification on Long-term Patenting Activity\\label{tab:patent_lt}",
             estimate = "{estimate}{stars}",
             notes = n, 
             output = "tinytable",
             width=c(0.20, 0.15, 0.15, 0.15, 0.15, 0.15)) |>
  style_tt(i=0, j=1:6, bold=T) |> 
  save_tt("./tables/patents_long_term.tex", overwrite = T)


## Part 2: Exhibitions: {1855-1867-1878-1889-1900-1911}
## DV: Log(ExhibitionCount), AvgComplexity, TopComplexity
## Weights: Stuff by Population
exhibitions1855 <- feols(count ~  allegiance_1861 + area_of_intersection + running | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1855)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)
exhibitions1867 <- feols(count ~ allegiance_1861 + area_of_intersection + running | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1867)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)
exhibitions1878 <- feols(count ~ allegiance_1861 + area_of_intersection + running  | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1878)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)
exhibitions1889 <- feols(count ~ allegiance_1861 + area_of_intersection + running   | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1889)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)
exhibitions1900 <- feols(count ~ allegiance_1861 + area_of_intersection + running   | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1900)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)


# Table notes
n <- "Table reports estimates of the difference in exhibitions count in Veneto relative to Lombardy, 
focusing on 5 successive years in which exhibitions took place: 1855, 1867, 1878, 1889, and 1900. 
The coefficients are estimated by OLS with a bandwidth of 100 kilometer, and are conducted at the \\textit{Comune} level. 
The estimates are weighted by the inverse absolute distance to the border, and control for area and distance to the border, 
and are also conditional on year fixed-effects. Heteroskedasticity-robust standard errors are clustered at the province-level. $*: p<0.1, **: p<0.05, ***: p<0.01$."


panels <- list(
  '1855'=exhibitions1855, 
  '1867'=exhibitions1867, 
  '1878'=exhibitions1878,
  '1889'=exhibitions1889,
  '1900'=exhibitions1900)

modelsummary(panels,
             coef_map=coef_map,
             stars=c("*"=0.1, "**"=0.05, "***"=0.01),
             gof_map = tibble(raw=c("adj.r.squared", "nobs"), 
                              clean=c("Adj. $R^2$", "N"),
                              fmt=c(3, 0)),
             title="Estimates of Unification on Long-term Exhibition Activity\\label{tab:exhibition_lt}",
             estimate = "{estimate}{stars}",
             notes = n, 
             output = "tinytable",
             width=c(0.20, 0.15, 0.15, 0.15, 0.15, 0.15)) |>
  style_tt(i=0, j=1:6, bold=T) |> 
  save_tt("./tables/exhibitions_long_term.tex", overwrite = T)

## Robustness: Extensive vs. Intensive Margin; Unit of Measurement




