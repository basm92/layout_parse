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
                     data = final |> group_by(PRO_COM) |> filter(abs(running) < bw, is.element(year, 1867)),
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

modelsummary(list(patents1855, patents1867, patents1878, patents1889, patents1902), stars=T)

## Part 2: Exhibitions: {1855-1867-1878-1889-1900-1911}
## DV: Log(ExhibitionCount), AvgComplexity, TopComplexity
## Weights: Stuff by Population
## Robustness: Extensive vs. Intensive Margin; Unit of Measurement




