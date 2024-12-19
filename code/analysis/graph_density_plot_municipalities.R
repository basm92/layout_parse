# graph_density_plot_municipalities
library(fixest); library(tidyverse); library(modelsummary); library(tinytable)
source("./code/data_wrangling/data_wrangling_final_ds.R")

final <- final |>
  mutate(patents_together_verz_italy = if_else(is.na(patents_together_verz_italy), 0, patents_together_verz_italy),
         patents_together_verz_italy_pc = if_else(is.na(patents_together_verz_italy_pc), 0, patents_together_verz_italy_pc))

avg_pop_time <- final |>
  filter(pop != 0) |>
  group_by(year, allegiance_1861) |>
  summarize(avg_pop = mean(pop, na.rm=F)) 


# Inhabitants per square kilometer
pop_dens_time <- final |>
  filter(pop != 0) |>
  mutate(pop_dens = pop/area_of_intersection) |>
  group_by(year, allegiance_1861) |>
  summarize(avg_pop_dens = mean(pop_dens, na.rm=F)*1e4)
  
table_data <- avg_pop_time |>
  merge(pop_dens_time) |>
  pivot_wider(names_from=allegiance_1861, values_from=c(avg_pop, avg_pop_dens)) |>
  select(c(year, contains("Lombardia"), contains("Veneto")))

n <- "Table shows average population and average population density for all municipalities in Lombardy and Veneto in various census years."

table_data |>
  setNames(c("Year", "Avg Pop.", "Avg Pop. Dens.", "Avg Pop.", "Avg Pop. Dens.")) |>
  tt(notes = n, caption="Population in Lombardy and Veneto\\label{tab:population}") |>
  group_tt(
    j=list("Lombardy"=2:3,
           "Veneto"=4:5)) |>
  format_tt(j=2:5, digits = 3) |>
  save_tt("tables/population_count_density.tex", overwrite = T)
