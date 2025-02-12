# Descriptive Stats
library(fixest); library(tidyverse); library(modelsummary); library(tinytable); library(patchwork)
source("./code/data_wrangling/data_wrangling_final_ds.R")

## Map depicting the border change from 1865 - Before After
plot <- geofile |>
  #mutate(mantova=as.factor(if_else(DEN_PROV == "Mantova", 1, 0))) |>
  ggplot() + 
  geom_sf(aes(fill=allegiance_1861)) +
  scale_color_viridis_d() +
  guides(fill=guide_legend(title="1861 Region"), color="none") +
  ggtitle("Lombardy and Veneto Regions", subtitle = "1861 Borders") +
  theme(
    axis.text = element_blank(),  # Remove axis labels
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank()  # Optionally remove axis ticks as well
  )

ggsave("./pics/border_change.pdf", plot)