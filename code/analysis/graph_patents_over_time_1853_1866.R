#graph_patents_over_time_1853_1866
library(fixest); library(tidyverse); library(modelsummary); library(tinytable)
source("./code/data_wrangling/data_wrangling_final_ds.R")

# Set the global bandwidth
bw <- 150000

# Mutate
final <- final |>
  mutate(patents_together_verz_italy = if_else(is.na(patents_together_verz_italy), 0, patents_together_verz_italy),
         patents_together_verz_italy_pc = if_else(is.na(patents_together_verz_italy_pc), 0, patents_together_verz_italy_pc))

# Graph Data
gd <- final |>
  filter(is.element(year, 1855:1866)) |> 
  group_by(allegiance_1861, year) |> 
  summarize(total_patents_per_capita = sum(patents_together_verz_italy, na.rm=T)/ sum(interpolated_population, na.rm=T)*1e6)

plot <- gd |> 
  ggplot(aes(x=year, y=total_patents_per_capita, color=allegiance_1861, group=allegiance_1861)) + 
  geom_line() + 
  ggtitle("Patents per 100,000 inhabitants") +
  xlab("Year") +
  ylab("Patents per 100,000 Inhabitants") +
  theme_light() +
  guides(color=guide_legend(title="Region"))

plot |>
  ggsave(filename="./pics/graph_patents_over_time_1853_1866.png", width=10, height=8)
