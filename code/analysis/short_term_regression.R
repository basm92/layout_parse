# Patents 1855-1866 
library(fixest); library(tidyverse); library(modelsummary); library(tinytable)
source("./code/data_wrangling/data_wrangling_final_ds.R")

compute_optimal_bw <- function(dv, 
                               iv, 
                               control_eq, # of the form "dv ~ x | fe"
                               dataset,
                               bwselect='msetwo'){
  dataset <- dataset |>
    mutate(dv = eval(parse(text = dv)),
           iv = eval(parse(text = iv))) |>
    filter(!is.na(dv), !is.na(iv))
  
  if(!is.null(control_eq)){
    result <- feols(as.formula(control_eq), data = dataset)
    dataset <- modelr::add_residuals(dataset, result, var = "resid")
    dataset <- dataset |>
      select(-dv) |>
      rename(dv = resid)
  }
  
  out <- rdrobust::rdbwselect(y=dataset$dv, x=dataset$iv, c=0, bwselect=bwselect)
  left_bw <- out$bws[1]
  right_bw <- out$bws[2]
  fr <- c(left_bw, right_bw)
  return(fr)
}

# Set the global bandwidth
bw <- 100000
## DV: {Sum_patents, Patents_Piedmonte, Patents_Austria}
## Treatment: Lombardia
## Robustness: Bandwidth around the border (Poisson-RDD)

#1. Patents
# Aggregate to the Circondario-level
aggregated_circ <- final |> 
  st_drop_geometry() |> 
  group_by(DEN_CIRC, year) |> 
  mutate(sum_patents = sum(patents_together),
         mean_patents = mean(patents_together),
         running=mean(running),
         mean_count = mean(count),
         sum_count = sum(count),
         area_of_intersection = sum(area_of_intersection),
         no_municipalities=n()) |>
  distinct(DEN_CIRC, year, .keep_all = T)
  

## Placebo's: before 1859 Annexation of Lombardy
# Circondare level
patents1858circ <- feols(sum_patents ~ allegiance_1861 + area_of_intersection + no_municipalities + running | year, 
                         data = aggregated_circ |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                         weights=~1/abs(running),
                         vcov=~DEN_PROV)
patents1858circpois <- fenegbin(sum_patents ~ allegiance_1861 + abs_distance_to_border | year, 
                         data = aggregated_circ |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                         weights=~1/abs(running),
                         vcov=~DEN_PROV)

# Municipality level
patents1858 <- feols(patents_together*1e3 ~ allegiance_1861 + area_of_intersection + running | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)
patents1858pois <- fenegbin(patents_together ~ allegiance_1861 + abs_distance_to_border | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)

## Real tests: After the 1859 Annexation of Lombardy
# Circondare Level
patents1867circ <- feols(sum_patents ~ allegiance_1861  + running + area_of_intersection + no_municipalities | year, 
                     data = aggregated_circ |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)
patents1867circpois <- fenegbin(sum_patents ~ allegiance_1861 + running + no_municipalities | year, 
                         data = aggregated_circ |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                         weights=~1/abs(running),
                         cluster=~DEN_PROV)

# Municipality level
patents1867 <- feols(patents_together*1e3 ~ allegiance_1861 + area_of_intersection + running | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)
patents1867pois <- fenegbin(patents_together ~ allegiance_1861 + abs_distance_to_border | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)

coef_map <- c("allegiance_1861Veneto"="Veneto",
              "allegiance_1861Lombardia" = "Lombardia")

# Table notes
n <- "Table reports estimates of the difference in patent count in Veneto relative to Lombardy. 
Panel A focuses on the pre-unification period (1855 to 1858), and 
Panel B on the unification period for Lombardy (1860-1866), but not for Veneto. 
The estimates in Columns 1-2 are conducted at the \\textit{Circondario} level, 
whereas the estimates in Columns 3-4 are conducted at the \\textit{Comune} level. 
The estimates in Columns 1 and 3 are OLS estimates, and the estimates in Columns 2 and 4 are Negative Binomial estimates. 
The estimates are weighted by the inverse absolute distance to the border, and control for area, distance to the border, latitude and longitude
, and are also conditional on year fixed-effects. Heteroskedasticity-robust standard errors are clustered at the province-level. $*: p<0.1, **: p<0.05, ***: p<0.01$."


panels <- list(
  "Panel A: Pre-Unification"=list(
    'OLS'=patents1858circ, 
    'Neg. Bin.'=patents1858circpois,
    'OLS'=patents1858,
    'Neg. Bin.'=patents1858pois),
  "Panel B: Post-Lombardy Unification, Pre-Veneto Unification"=list(
    'OLS'=patents1867circ, 
    'Neg. Bin.'=patents1867circpois,
    'OLS'=patents1867,
    'Neg. Bin.'=patents1867pois
  )
)

modelsummary(panels,
             shape="rbind",
             coef_map=coef_map,
             stars=c("*"=0.1, "**"=0.05, "***"=0.01),
             gof_map = tibble(raw=c("adj.r.squared", "nobs"), 
                              clean=c("Adj. $R^2$", "N"),
                              fmt=c(3, 0)),
             title="Estimates of Unification on Patenting Activity\\label{tab:patent}",
             estimate = "{estimate}{stars}",
             notes = n, 
             output = "tinytable",
             width=c(0.3, 0.15, 0.15, 0.15, 0.15)
) |>
  group_tt(
    j=list("Circondario"=2:3, "Comune"=4:5)) |>
  style_tt(
    i=c(1, 6), bold=T) |>
  save_tt("./tables/patents_short_term.tex", overwrite = T)

# 2. Exhibitions
# Circondare Level
exhibitions1855circ <- feols(sum_count ~ allegiance_1861 + area_of_intersection + no_municipalities + running | year, 
                         data = aggregated_circ |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                         weights=~1/abs(running),
                         vcov=~DEN_PROV)
exhibitions1855circpois <- fenegbin(sum_count ~ allegiance_1861 + no_municipalities + abs_distance_to_border | year, 
                              data = aggregated_circ |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                              weights=~1/abs(running),
                              vcov=~DEN_PROV)

# Municipality level
exhibitions1855 <- feols(count ~ allegiance_1861 + area_of_intersection + running | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)
exhibitions1855pois <- fenegbin(count ~ allegiance_1861 + abs_distance_to_border | year, 
                          data = final |> filter(abs(running) < bw, is.element(year, 1855:1858)),
                          weights=~1/abs(running),
                          vcov=~DEN_PROV)

## Real tests: After the 1859 Annexation of Lombardy
# Circondare Level
exhibitions1867circ <- feols(sum_count ~ allegiance_1861 + area_of_intersection + running + no_municipalities | year, 
                         data = aggregated_circ |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                         weights=~1/abs(running),
                         vcov=~DEN_PROV)
exhibitions1867circpois <- fenegbin(sum_count ~ allegiance_1861 + abs_distance_to_border + no_municipalities | year, 
                              data = aggregated_circ |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                              weights=~1/abs(running),
                              vcov=~DEN_PROV)
# Municipality level
exhibitions1867 <- feols(count ~ allegiance_1861 + area_of_intersection + running | year, 
                     data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                     weights=~1/abs(running),
                     vcov=~DEN_PROV)
exhibitions1867pois <- fenegbin(count ~ allegiance_1861 + area_of_intersection + abs_distance_to_border | year, 
                          data = final |> filter(abs(running) < bw, is.element(year, 1860:1867)),
                          weights=~1/abs(running),
                          vcov=~DEN_PROV)

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


# 3. Patents with Optimal Bandwidth
bws_circ_pre <- compute_optimal_bw("patents_together", "running", "dv ~ 1 | as.factor(year)", p_aggregated_circ_pre)
bws_munip_pre <- compute_optimal_bw("patents_together", "running", "dv ~ 1 | as.factor(year)", p_municipality_pre)
bws_circ_post <- compute_optimal_bw("patents_together", "running", "dv ~ 1 | as.factor(year)", p_aggregated_circ_post)
bws_munip_post <- compute_optimal_bw("patents_together", "running", "dv ~ 1 | as.factor(year)", p_municipality_post, bwselect='mserd')

