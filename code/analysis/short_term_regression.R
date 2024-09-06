# Patents 1855-1866 
library(fixest); library(tidyverse)
## DV: {Sum_patents, Patents_Piedmonte, Patents_Austria}
## Treatment: Lombardia
## Robustness: Bandwidth around the border (Poisson-RDD)

aggregated_circ <- final |> 
  st_drop_geometry() |> 
  group_by(DEN_CIRC, year) |> 
  summarize(sum_patents = sum(patents_together),
            running=mean(running),
            allegiance_1861 = allegiance_1861,
            area = area_of_intersection) |>
  distinct() |>
  filter(between(year, 1855, 1858)) |>
  mutate(running_abs = abs(running))

# Circondare level
## Placebo's: before 1859 Annexation of Lombardy
fepois(sum_patents ~ allegiance_1861 + area | as.factor(year), data=aggregated_circ |> 
         filter(abs(running) < 70000))

feols(sum_patents ~ allegiance_1861 + area | as.factor(year), data=aggregated_circ |> 
        filter(abs(running) < 70000))

fepois(patents_together ~ allegiance_1861 + area_of_intersection | as.factor(year), data = final |>
         filter(between(year, 1860, 1867), abs(running) < 70000))

fepois(count ~ allegiance_1861 + area_of_intersection | as.factor(year), data = final |>
         filter(between(year, 1855, 1855), abs(running) < 70000))

# Municipality level


