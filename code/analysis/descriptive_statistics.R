# Descr Stats
library(fixest); library(tidyverse); library(modelsummary); library(tinytable)
source("./code/data_wrangling/data_wrangling_final_ds.R")

## Number of Patents in the Database {Italy, Austra}
table_no_patents <- final |> 
  st_drop_geometry() |>
  group_by(year) |>
  summarize(number_total = sum(patents_together),
            number_austria = sum(patents_austria),
            number_italy = sum(patents_italy)) |>
  filter(number_italy>0, year < 1889)


n <-  "Table displays Total Number of Patents issued to individuals in Lombard and Venetian cities, 
  Number of Austrian Patents (issued to individuals in Lombard and Venetian cities) and 
  Number of Piedmontese (until 1861, issued to individuals in Lombard and Venetian cities) and Italian (after 1861) Patents. 
  After 1867, the Italian Patent data is only sparsely available. 
  We display only available years, even though Austrian patents are availablein other years."

table_no_patents |>
  mutate(year = as.character(as.factor(year))) |>
  add_row(tibble(year="Total", 
                 number_total = sum(table_no_patents$number_total),
                 number_austria=sum(table_no_patents$number_austria),
                 number_italy = sum(table_no_patents$number_italy))) |>
  rename(Year = year, 
         `Total Patents` = number_total,
         `Austrian Patents` = number_austria,
         `Italian Patents` = number_italy) |>
  datasummary_df(
    title="Patents in Veneto and Lombardy",
    notes = n,
    fmt=0) |> 
  style_tt(i=0, bold=T) |>
  style_tt(j=2:4, align="r") |>
  style_tt(i=14, line="b", line_width=0.1) |>
  save_tt("./tables/table_no_patents.tex", overwrite = T)

## Number of Exhibitions by Year
table_no_exh <- final |>
  st_drop_geometry() |>
  group_by(year, allegiance_1861) |>
  summarize(count = sum(count)) |>
  filter(!is.na(count), year < 1911) |>
  pivot_wider(names_from = allegiance_1861, values_from=count) |>
  mutate(year = as.character(as.factor(year)))
  
n <-  "Table displays Total Number of Exhibits in Lombard and Venetian cities (total in the entire province) in subsequent exhibition years"

table_no_exh |>
  ungroup() |>
  add_row(tibble(year="Total", 
                 Lombardia = sum(table_no_exh$Lombardia),
                 Veneto=sum(table_no_exh$Veneto))) |>
  rename(Year = year) |>
  datasummary_df(
    title="Exhibitions in Veneto and Lombardy",
    notes = n,
    fmt=0,
    width=0.7) |> 
  style_tt(i=5, line="b", line_width=0.1) |>
  style_tt(j=2:3, align='r') |>
  save_tt("./tables/table_no_exhibitions.tex", overwrite=T)

## Number of Exhibitions 
## Geographical Distribution {1855, 1867}
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

## Map of Complexity of Various Locations {1855}

## Balance: Statistics of Industry Presence Austro-Hungary