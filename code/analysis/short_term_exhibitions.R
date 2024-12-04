# 2. Exhibitions
# Patents 1855-1866 
library(fixest); library(tidyverse); library(modelsummary); library(tinytable)
source("./code/data_wrangling/data_wrangling_final_ds.R")
bw <- 80000


# Pre-Unification (1855)
exhibitions1855 <- feols(count_pc*1e6 ~ allegiance_1861, 
                         data = final |> filter(abs(running) < bw, is.element(year, 1855)),
                         vcov='hc1')
exhibitions1855pois <- fepois(count_pc*1e6 ~ allegiance_1861, 
                         data = final |> filter(abs(running) < bw, is.element(year, 1855)),
                         vcov='hc1')
exhibitions1855cv <- feols(count_pc*1e6 ~ allegiance_1861 + interpolated_population + area_of_intersection + abs_distance_to_border, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1855)),
                     vcov='hc1')
exhibitions1855poiscv <- fepois(count_pc*1e6 ~ allegiance_1861 + interpolated_population + area_of_intersection + abs_distance_to_border, 
                           data = final |> filter(abs(running) < bw, is.element(year, 1855)),
                           vcov='hc1')

## After the 1859 Annexation of Lombardy (1867)
exhibitions1867 <- feols(count_pc*1e6 ~ allegiance_1861, 
                         data = final |> filter(abs(running) < bw, is.element(year, 1867)),
                         vcov='hc1')
exhibitions1867pois <- fepois(count_pc*1e6 ~ allegiance_1861, 
                              data = final |> filter(abs(running) < bw, is.element(year, 1867)),
                              vcov='hc1')
exhibitions1867cv <- feols(count_pc*1e6 ~ allegiance_1861 + interpolated_population + area_of_intersection + abs_distance_to_border, 
                           data = final |> filter(abs(running) < bw, is.element(year, 1867)),
                           vcov='hc1')
exhibitions1867poiscv <- fepois(count_pc*1e6 ~ allegiance_1861 + interpolated_population + area_of_intersection + abs_distance_to_border, 
                                data = final |> filter(abs(running) < bw, is.element(year, 1867)),
                                vcov='hc1')


beta1 <- coef(exhibitions1855)["allegiance_1861Lombardia"]
beta2 <- coef(exhibitions1867)["allegiance_1861Lombardia"]
se1 <- sqrt(vcov(exhibitions1855)["allegiance_1861Lombardia", "allegiance_1861Lombardia"])
se2 <- sqrt(vcov(exhibitions1867)["allegiance_1861Lombardia", "allegiance_1861Lombardia"])
Z <- (beta1 - beta2) / sqrt(se1^2 + se2^2)

# Compute p-value (two-tailed test)
p_value <- 2 * (1 - pnorm(abs(Z)))

test <- feols(count_pc*1e6 ~ as.factor(year) + allegiance_1861 + allegiance_1861:as.factor(year), 
              data = final |> filter(abs(running) < bw, is.element(year, c(1855, 1867))),
              vcov=~PRO_COM)

test2 <- feols(count_pc*1e6 ~ as.factor(year) + allegiance_1861 + latitude + longitude + allegiance_1861:as.factor(year)  + interpolated_population:as.factor(year) + abs_distance_to_border:as.factor(year) + area_of_intersection:as.factor(year)| DEN_PROV, 
              data = final |> filter(abs(running) < bw, is.element(year, c(1855, 1867))),
              vcov=~PRO_COM)

library(car)
test3<-linearHypothesis(test, "as.factor(year)1867 = as.factor(year)1867:allegiance_1861Lombardia")
test4<-linearHypothesis(test2, "as.factor(year)1867 = as.factor(year)1867:allegiance_1861Lombardia")
panels <- list(
  "Panel A: Pre-Unification"=list(
    'OLS'=exhibitions1855circ,
    'Neg. Bin.'=exhibitions1855circpois,
    'OLS'=exhibitions1855,
    'Neg. Bin.'=exhibitions1855pois),
  "Panel B: Post-Lombardy Unification, Pre-Veneto Unification"=list(
    'OLS'=exhibitions1867circ, 
    'Neg. Bin.'=exhibitions1867circpois,
    'OLS'=exhibitions1867,
    'Neg. Bin.'=exhibitions1867pois
  )
)

n2 <- "Table reports estimates of the difference in exhibition count in Veneto relative to Lombardy. 
Panel A focuses on the pre-unification period (the 1855 Exhibition), and 
Panel B on the unification period for Lombardy (the 1867 Exhibition).  
The estimates in Columns 1-2 are conducted at the \\textit{Circondario} level, 
whereas the estimates in Columns 3-4 are conducted at the \\textit{Comune} level. 
The estimates in Columns 1 and 3 are OLS estimates, and the estimates in Columns 2 and 4 are Negative Binomial estimates. 
The estimates are weighted by the inverse absolute distance to the border, and control for area, distance to the border, latitude and longitude
, and are also conditional on year fixed-effects. Heteroskedasticity-robust standard errors are clustered at the province-level. $*: p<0.1, **: p<0.05, ***: p<0.01$."

modelsummary(panels,
             shape="rbind",
             coef_map=coef_map,
             stars=c("*"=0.1, "**"=0.05, "***"=0.01),
             gof_map = tibble(raw=c("adj.r.squared", "nobs"), 
                              clean=c("Adj. $R^2$", "N"),
                              fmt=c(3, 0)),
             title="Estimates of Unification on Exhibition Activity\\label{tab:exhibition}",
             estimate = "{estimate}{stars}",
             notes = n2, 
             output = "tinytable",
             width=c(0.3, 0.15, 0.15, 0.15, 0.15)
) |>
  group_tt(
    j=list("Circondario"=2:3, "Comune"=4:5)) |>
  style_tt(
    i=c(1, 6), bold=T) |>
  save_tt("./tables/exhibitions_short_term.tex", overwrite = T)

