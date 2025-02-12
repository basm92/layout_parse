library(tidyverse); library(sf); library(patchwork)

# Filter out
blacklist <- c("Alagna", "Carbonara al Ticino", "Cava Manara", "Ferrera Erbognone", "Mezzana Rabattone",
               "Pieve Albignola", "San Martino Siccomario", "Sannazzaro de' Burgondi", "Scaldasole",
               "Sommo", "Travaco' Siccomario", "Villanuova Ardenghi", "Zinasco")
 
source("./code/data_wrangling/data_wrangling_final_ds.R")

geofile <- geofile |>
  filter(!is.element(COMUNE, blacklist))

# Mutate
data_to_be_plotted <- final |>
  group_by(PRO_COM, year) |>
  summarize(sum_pat = sum(patents_together_verz_italy)) |>
  summarize(sum_pat_before = sum(sum_pat[year < 1860]), sum_pat_after = sum(sum_pat[year>1859]))

plot_data <- geofile |>
  left_join(data_to_be_plotted)

## Map depicting the border change from 1865 - Before After
#plot <- 

p1 <- plot_data |>
  mutate(mantova=as.factor(if_else(DEN_PROV == "Mantova", 1, 0))) |>
  ggplot() + 
  geom_sf(aes(fill=log(1+sum_pat_before), color=allegiance_1861)) +
  scale_fill_viridis_c()+
  scale_color_viridis_d(option = 'plasma') +
  guides(fill=guide_colorbar(title="Patent Intensity"), color="none") +
  ggtitle("Patent Intensity in Lombardy and Veneto Regions", subtitle = "Before") +
  theme(
    axis.text = element_blank(),  # Remove axis labels
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank()  # Optionally remove axis ticks as well
  )

p2 <- plot_data |>
  mutate(mantova=as.factor(if_else(DEN_PROV == "Mantova", 1, 0))) |>
  ggplot() + 
  geom_sf(aes(fill=log(1+sum_pat_after), color=allegiance_1861)) +
  scale_fill_viridis_c()+
  scale_color_viridis_d(option = 'plasma') +
  guides(fill="none", color="none") +
  ggtitle(label="", subtitle="After") +
  theme(
    axis.text = element_blank(),  # Remove axis labels
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank()  # Optionally remove axis ticks as well
  )

final <- p1 / p2
ggsave("./pics/graph_patents_geographical_1855_1866.pdf", width=10, height=6)
