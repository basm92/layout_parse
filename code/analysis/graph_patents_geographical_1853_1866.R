library(tidyverse); library(sf)

# Filter out
blacklist <- c("Alagna", "Carbonara al Ticino", "Cava Manara", "Ferrera Erbognone", "Mezzana Rabattone",
               "Pieve Albignola", "San Martino Siccomario", "Sannazzaro de' Burgondi", "Scaldasole",
               "Sommo", "Travaco' Siccomario", "Villanuova Ardenghi", "Zinasco")
 
geofile <-   read_sf('./data/shapefiles_images/geofile.geojson') |>
  filter(!is.element(COMUNE, blacklist))

source("./code/data_wrangling/data_wrangling_final_ds.R")

# Mutate
final <- final |>
  mutate(patents_together_verz_italy = if_else(is.na(patents_together_verz_italy), 0, patents_together_verz_italy),
         patents_together_verz_italy_pc = if_else(is.na(patents_together_verz_italy_pc), 0, patents_together_verz_italy_pc))

data_to_be_plotted <- final |>
  group_by(PRO_COM, year) |>
  summarize(sum_pat = sum(patents_together_verz_italy)) |>
  summarize(sum_pat_before = sum(sum_pat[year < 1860]), sum_pat_after = sum(sum_pat[year>1859]))

## Map depicting the border change from 1865 - Before After
plot <- geofile |>
  mutate(mantova=as.factor(if_else(DEN_PROV == "Mantova", 1, 0))) |>
  ggplot() + 
  geom_sf(aes(fill=allegiance_1861, color=mantova)) +
  scale_color_viridis_d() +
  guides(fill=guide_legend(title="1861 Region"), color="none") +
  ggtitle("Lombardy and Veneto Regions", subtitle = "1861 Borders, Mantova Province in yellow") +
  theme(
    axis.text = element_blank(),  # Remove axis labels
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank()  # Optionally remove axis ticks as well
  )
