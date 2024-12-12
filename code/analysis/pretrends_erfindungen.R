#pretrends_erfindungen
library(tidyverse); library(sf)
bw <- 100000
erfindungen_italy <- read_delim("./data/patent_data/erfindungen_italy_geocoded.csv")
geofile <- sf::read_sf("./data/shapefiles_images/geofile.geojson") |>
  select(-c(POP_1991, contains("Shape"), overlap, CC_P)) |>
  distinct()
# Create a grid for the patent years and exhibition years
## Change the grid to incorporate pre-trends
years <- 1800:1852
base <- expand_grid(PRO_COM=geofile$PRO_COM, year=years)
base <- base |>
  left_join(geofile |> 
              st_drop_geometry())

base <- base |>
  left_join(erfindungen_italy)

base <- base |>
  mutate(patents_austria_with_zeroes = if_else(is.na(patents_austria), 0, patents_austria),
         decade = case_when(
           between(year, 1800, 1810) ~ "1800-1810",
           between(year, 1811, 1820) ~ "1811-1820",
           between(year, 1821, 1830) ~ "1821-1830",
           between(year, 1831, 1840) ~ "1831-1840",
           between(year, 1841, 1852) ~ "1841-1852"
         ))


coords <- geofile |>
  st_transform('wgs84') |>
  st_centroid() |> 
  mutate(longitude = st_coordinates(geometry)[,1],
         latitude = st_coordinates(geometry)[,2]) |>
  dplyr::select(PRO_COM, longitude, latitude) |>
  st_drop_geometry()

# Set the data to the correct years
base <- base |>
  left_join(coords,by="PRO_COM") |>
  mutate(allegiance_1861 = factor(allegiance_1861, levels=c("Veneto", "Lombardia"))) |>
  filter(abs(running) < bw)

# Regression
## Intensive Margin
#feols(patents_austria ~ i(as.factor(decade), allegiance_1861, ref="1800-1810", ref2 = "Veneto") + allegiance_1861 + as.factor(decade), data = base, vcov=~PRO_COM) |> summary()
#fepois(patents_austria ~ i(as.factor(decade), allegiance_1861, ref="1800-1810", ref2 = "Veneto") + allegiance_1861 + as.factor(decade), data = base) |> summary()
## Extensive Margin
model1 <- feols(patents_austria_with_zeroes ~ i(as.factor(decade), allegiance_1861, ref="1800-1810", ref2 = "Veneto") + allegiance_1861 + as.factor(decade), data = base)
model2 <- fepois(patents_austria_with_zeroes ~ i(as.factor(decade), allegiance_1861, ref="1800-1810", ref2 = "Veneto") + allegiance_1861 + as.factor(decade), data = base)
model3 <- feols(patents_austria_with_zeroes ~ i(as.factor(decade), allegiance_1861, ref="1800-1810", ref2 = "Veneto") + allegiance_1861 + as.factor(decade) + area_of_intersection + abs_distance_to_border, data = base)
model4 <- fepois(patents_austria_with_zeroes ~ i(as.factor(decade), allegiance_1861, ref="1800-1810", ref2 = "Veneto") + allegiance_1861 + as.factor(decade) + area_of_intersection + abs_distance_to_border, data = base)


coef_map <- c("allegiance_1861Lombardia" = "Lombardia x 1800-1810",
              "as.factor(decade)::1811-1820:allegiance_1861::Lombardia" = "Lombardia x 1811-1820",
              "as.factor(decade)::1821-1830:allegiance_1861::Lombardia" = "Lombardia x 1821-1830",
              "as.factor(decade)::1831-1840:allegiance_1861::Lombardia" = "Lombardia x 1831-1840",
              "as.factor(decade)::1841-1852:allegiance_1861::Lombardia" = "Lombardia x 1841-1852"
              )

n <- "Table reports estimates of the difference in patent count in Lombardy relative to Veneto in different decades. 
The dependent variable is patent count. 
The estimates are conducted at the \\textit{Comune} level. 
The estimates in Columns 1 and 3 are OLS estimates, and the estimates in Columns 2 and 4 are Poisson estimates. 
Where applicable, the estimates control for area, distance to the border, longitude and latitude. 
Heteroskedasticity-robust standard errors are clustered at the Comune-level. $*: p<0.1, **: p<0.05, ***: p<0.01$."


modelsummary(list("OLS"=model1, "Poisson"=model2, "OLS"=model3, "Poisson"=model4),
             coef_map = coef_map, stars=c("*"=0.1, "**"=0.05, "***"=0.01), 
             title = "Pre-Trends in Patenting Activity\\label{tab:pretrends_erf}", 
             gof_map = tibble(raw=c("adj.r.squared", "nobs"), 
                              clean=c("Adj. $R^2$", "N"),
                              fmt=c(3, 0)),
             
             estimate = "{estimate}{stars}",
             notes = n, 
             output = "tinytable",
             width=c(0.3, 0.1, 0.1, 0.1, 0.1), 
             add_rows = as_tibble_row(c("Controls", rep("No", 2), rep("Yes", 2)), .name_repair = "unique"),
             vcov = ~PRO_COM) |>
  save_tt("./tables/pretrends_erfindungen.tex", overwrite = T)
