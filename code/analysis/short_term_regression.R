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
bw <- 80000
## DV: {Sum_patents, Patents_Piedmonte, Patents_Austria}
## Treatment: Lombardia
## Robustness: Bandwidth around the border (Poisson-RDD)

#1. Patents
# Aggregate to the Circondario-level
p_aggregated_circ_pre <- final |> 
  st_drop_geometry() |> 
  group_by(DEN_CIRC, year) |> 
  mutate(sum_patents = sum(patents_together),
         running=mean(running),
         count = mean(count)) |>
  distinct(DEN_CIRC, year, .keep_all = T) |>
  filter(between(year, 1855, 1858)) |>
  mutate(running_abs = abs(running))

p_aggregated_circ_post <- final |> 
  st_drop_geometry() |> 
  group_by(DEN_CIRC, year) |> 
  mutate(sum_patents = sum(patents_together),
         running=mean(running),
         count=mean(count)) |>
  distinct(DEN_CIRC, year, .keep_all = T) |>
  filter(between(year, 1860, 1866)) |>
  mutate(running_abs = abs(running))

p_municipality_pre <- final |>
  st_drop_geometry() |>
  filter(between(year, 1855, 1858)) 

p_municipality_post <- final |>
  st_drop_geometry() |>
  filter(between(year, 1860, 1866)) 

## Placebo's: before 1859 Annexation of Lombardy
# Circondare level
placebo_model_ols_circ <- feols(sum_patents ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude | as.factor(year),
                                data=p_aggregated_circ_pre |> filter(abs(running) < bw),
                                weights=~1/abs(running),
                                vcov=~DEN_PROV)
placebo_model_pois_circ <- fepois(sum_patents ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude  | as.factor(year),
                                  data=p_aggregated_circ_pre |> filter(abs(running) < bw),
                                  weights=~1/abs(running),
                                  vcov=~DEN_PROV)
# Municipality level
placebo_model_ols_munip <- feols(patents_together ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude  | as.factor(year), 
                                 data = p_municipality_pre |> filter(abs(running) < bw),
                                 weights=~1/abs(running),
                                 vcov=~DEN_PROV)
placebo_model_pois_munip <- fepois(patents_together ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude  | as.factor(year),
                                   data = p_municipality_pre |> filter(abs(running) < bw),
                                   weights=~1/abs(running),
                                   vcov=~DEN_PROV)

## Real tests: After the 1859 Annexation of Lombardy
# Circondare Level
post_model_ols_circ <- feols(sum_patents ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude  | as.factor(year),
                                data=p_aggregated_circ_post |> filter(abs(running) < bw),
                             weights=~1/abs(running),
                             vcov=~DEN_PROV)
post_model_pois_circ <- fepois(sum_patents ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude  | as.factor(year),
                                  data=p_aggregated_circ_post |> filter(abs(running) < bw), 
                               weights=~1/abs(running),
                               vcov=~DEN_PROV)
# Municipality level
post_model_ols_munip <- feols(patents_together ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude | as.factor(year), 
                                 data = p_municipality_post |> filter(abs(running) < bw),
                              weights=~1/abs(running),
                              vcov=~DEN_PROV)
post_model_pois_munip <- fepois(patents_together ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude  | as.factor(year),
                                data = p_municipality_post |> filter(abs(running) < bw),
                                weights=~1/abs(running),
                                vcov=~DEN_PROV)


coef_map <- c("allegiance_1861Veneto"="Veneto")

# Table notes
n <- "Table reports estimates of the difference in patent count in Veneto relative to Lombardy. 
Panel A focuses on the pre-unification period (1855 to 1858), and 
Panel B on the unification period for Lombardy (1860-1866), but not for Veneto. 
The estimates in Columns 1-2 are conducted at the \\textit{Circondario} level, 
whereas the estimates in Columns 3-4 are conducted at the \\textit{Comune} level. 
The estimates in Columns 1 and 3 are OLS estimates, and the estimates in Columns 2 and 4 are Poisson estimates. 
The estimates are weighted by the inverse absolute distance to the border, and control for area, distance to the border, latitude and longitude
, and are also conditional on year fixed-effects. Heteroskedasticity-robust standard errors are clustered at the province-level. $*: p<0.1, **: p<0.05, ***: p<0.01$."


panels <- list(
  "Panel A: Pre-Unification"=list(
    'OLS'=placebo_model_ols_circ, 
    'Poisson'=placebo_model_pois_circ,
    'OLS'=placebo_model_ols_munip,
    'Poisson'=placebo_model_pois_munip),
  "Panel B: Post-Lombardy Unification, Pre-Veneto Unification"=list(
    'OLS'=post_model_ols_circ, 
    'Poisson'=post_model_pois_circ,
    'OLS'=post_model_ols_munip,
    'Poisson'=post_model_pois_munip
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
## 2.1 Make the appropriate datasets (Exhibition of 1855 vs. later exhibitions)
e_municipality_pre <- p_municipality_pre
e_aggregated_circ_pre <- p_aggregated_circ_pre
e_municipality_post <- final |>
  st_drop_geometry() |>
  filter(between(year, 1866, 1868)) 

e_aggregated_circ_post <- final |> 
  st_drop_geometry() |> 
  group_by(DEN_CIRC, year) |> 
  mutate(sum_patents = sum(patents_together),
         running=mean(running),
         count=mean(count)) |>
  distinct(DEN_CIRC, year, .keep_all = T) |>
  filter(between(year, 1866, 1868)) |>
  mutate(running_abs = abs(running))

bw<-1e9
# Circondare Level
placebo_model_ols_circ <- feols(count ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude | as.factor(year),
                                data=e_aggregated_circ_pre |> filter(abs(running) < bw),
                                weights=~1/abs(running),
                                vcov=~DEN_PROV)
placebo_model_pois_circ <- fepois(count ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude  | as.factor(year),
                                  data=e_aggregated_circ_pre |> filter(abs(running) < bw),
                                  weights=~1/abs(running),
                                  vcov=~DEN_PROV)
# Municipality level
placebo_model_ols_munip <- feols(count ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude  | as.factor(year), 
                                 data = e_municipality_pre |> filter(abs(running) < bw),
                                 weights=~1/abs(running),
                                 vcov=~DEN_PROV)
placebo_model_pois_munip <- fepois(count ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude  | as.factor(year),
                                   data = e_municipality_pre |> filter(abs(running) < bw),
                                   weights=~1/abs(running),
                                   vcov=~DEN_PROV)

## Real tests: After the 1859 Annexation of Lombardy
# Circondare Level
post_model_ols_circ <- feols(count ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude  | as.factor(year),
                             data= e_aggregated_circ_post |> filter(abs(running) < bw),
                             weights=~1/abs(running),
                             vcov=~DEN_PROV)
post_model_pois_circ <- fepois(count ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude  | as.factor(year),
                               data=e_aggregated_circ_post |> filter(abs(running) < bw),
                               weights=~1/abs(running),
                               vcov=~DEN_PROV)
# Municipality level
post_model_ols_munip <- feols(count ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude  | as.factor(year), 
                              data = e_municipality_post |> filter(abs(running) < bw),
                              weights=~1/abs(running),
                              vcov=~DEN_PROV)
post_model_pois_munip <- fepois(count ~ allegiance_1861 + area_of_intersection + angle_to_line + longitude  | as.factor(year),
                                data = e_municipality_post |> filter(abs(running) < bw),
                                weights=~1/abs(running),
                                vcov=~DEN_PROV)

panels <- list(
  "Panel A: Pre-Unification"=list(
    'OLS'=placebo_model_ols_circ, 
    'Poisson'=placebo_model_pois_circ,
    'OLS'=placebo_model_ols_munip,
    'Poisson'=placebo_model_pois_munip),
  "Panel B: Post-Lombardy Unification, Pre-Veneto Unification"=list(
    'OLS'=post_model_ols_circ, 
    'Poisson'=post_model_pois_circ,
    'OLS'=post_model_ols_munip,
    'Poisson'=post_model_pois_munip
  )
)

n2 <- "Table reports estimates of the difference in exhibition count in Veneto relative to Lombardy. 
Panel A focuses on the pre-unification period (the 1855 Exhibition), and 
Panel B on the unification period for Lombardy (the 1867 Exhibition).  
The estimates in Columns 1-2 are conducted at the \\textit{Circondario} level, 
whereas the estimates in Columns 3-4 are conducted at the \\textit{Comune} level. 
The estimates in Columns 1 and 3 are OLS estimates, and the estimates in Columns 2 and 4 are Poisson estimates. 
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

