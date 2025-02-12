#graph_exhibition_count
library(fixest); library(tidyverse); library(modelsummary); library(tinytable)
source("./code/data_wrangling/data_wrangling_final_ds.R")

exh_1855 <- left_join(geofile |>  select(PRO_COM),
                      final |> select(PRO_COM, count, year, allegiance_1861) |> filter(year==1855))

p1 <- exh_1855 |>
  ggplot(aes(fill=log(1+count), color=allegiance_1861)) + 
  geom_sf() + 
  scale_fill_viridis_c() +
  ggtitle("Exhibition Count 1855 Exhibition") +
  theme(
    legend.position = "none",
    axis.text = element_blank(),  # Remove axis labels
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank()  # Optionally remove axis ticks as well
  )


exh_1867 <- left_join(geofile |>  select(PRO_COM),
                      final |> select(PRO_COM, count, year, allegiance_1861) |> filter(year==1867))

p2 <- exh_1867 |>
  ggplot(aes(fill=log(1+count), color=allegiance_1861)) +
  geom_sf() + 
  scale_fill_viridis_c()  + 
  ggtitle("Exhibition Count 1867 Exhibition") +
  theme(
    legend.position = "none",
    axis.text = element_blank(),  # Remove axis labels
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank()  # Optionally remove axis ticks as well
  )

plot2 <- p1+p2
ggsave("./pics/graph_exhibition_count.pdf")