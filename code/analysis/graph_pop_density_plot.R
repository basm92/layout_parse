# graph_density_plot_municipalities
library(fixest); library(tidyverse); library(modelsummary); library(tinytable)
source("./code/data_wrangling/data_wrangling_final_ds.R")

final <- final |>
  mutate(patents_together_verz_italy = if_else(is.na(patents_together_verz_italy), 0, patents_together_verz_italy),
         patents_together_verz_italy_pc = if_else(is.na(patents_together_verz_italy_pc), 0, patents_together_verz_italy_pc))

plot <- final |>
  group_by(year, allegiance_1861) |>
  mutate(pop_dens = interpolated_population / area_of_intersection*1e4) |>
  filter(year==1861) |>
  ggplot(aes(x=pop_dens, group=allegiance_1861, color=allegiance_1861)) +
  geom_density() +
  ggtitle("Distribution of Population (1861)") +
  xlab("Population Density") +
  ylab("Density") +
  theme_light() +
  guides(color=guide_legend(title="Region"))

plot |>
  ggsave(filename="./pics/graph_pop_density_plot.png", width=10, height=8)
