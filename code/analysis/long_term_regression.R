# Long Run: Patents and Exhibitions
library(fixest); library(tidyverse); library(did); library(didimputation); library(rdrobust); library(modelr)
source("./code/data_wrangling/data_wrangling_final_ds.R")
## Estimator: Staggered DiD - Use the Borusyak-Jaravel estimator or Callaway-Sant'Anna
## DV: Log(PatentCount_{Italy, Austria}), 
## Part 1: Patents: {1855-1867-1878-1890-1902-1911}
## Robustness: Extensive vs. Intensive Margin; Unit of Measurement


## Part 2: Exhibitions: {1855-1867-1878-1889-1900-1911}
## DV: Log(ExhibitionCount), AvgComplexity, TopComplexity
## Weights: Stuff by Population
## Robustness: Extensive vs. Intensive Margin; Unit of Measurement
final <- final |>
  mutate(treated = if_else((year > 1855 & allegiance_1861 == "Lombardia") | (year > 1867 & allegiance_1861 == "Veneto"), 1, 0),
         treatment = case_when(allegiance_1861 == "Lombardia" ~ 1867, 
                               allegiance_1861 == "Veneto"~ 1878, 
                               TRUE ~ NA))

# 1855 placebo
data <- final  |>
  filter(between(year, 1860, 1865)) |>
  mutate(lc = log(patents_together + 1)) |>
  filter(!is.na(lc))

rdrobust(y=data$lc, x=data$running, cluster=data$DEN_CIRC) |>
  summary()

out <- feols(log(count) ~ 1, data = data)
data <- data |> 
  add_residuals(out) |>
  rename(dv = resid) |>
  filter(!is.na(dv), !is.na(running)) |>
  mutate(lc = log(count))





