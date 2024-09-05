# Long Run: Patents and Exhibitions
library(fixest); library(tidyverse); library(did); library(didimputation)
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

feols(log(1+count) ~ 1 + i(year, ref=1855) + i(year, treated) | PRO_COM, 
      data = final ,
      vcov=~DEN_CIRC)

did::att_gt(yname="count", 
            gname="treatment",
            idname="PRO_COM",
            tname="year",
            xformla=~1,
            data=final,
            est_method="reg",
            control_group = "notyettreated"
            )
