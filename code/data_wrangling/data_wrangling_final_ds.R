# data_wrangling_final_ds
export <- FALSE
## In this file, on the basis of:
### ./data/shapefiles_images/geofile.geojson (resulting from data_wrangling_geofile.R)
### ./data/patents_final_dataset.csv (resulting from data_wrangling_patents.R)
### ./data/exhibitions_final_dataset.csv (resulting from data_wrangling_exhibitions.R)
### We build the final dataset for analysis integrating all of these to a municipality, year dataset
### And we add several control variables to this dataset
library(sf); library(tidyverse); library(fixest); library(zoo)
# Import the basic dataset
geofile <- sf::read_sf("./data/shapefiles_images/geofile.geojson") |>
  select(-c(POP_1991, contains("Shape"), overlap, CC_P)) |>
  distinct()
# Create a grid for the patent years and exhibition years
## Change the grid to incorporate pre-trends
years <- 1804:1911
base <- expand_grid(PRO_COM=geofile$PRO_COM, year=years)
base <- base |>
  left_join(geofile |> 
              st_drop_geometry() |> 
              select(PRO_COM, PRO_COM_T))

patents <- read_delim("./data/patents_final_dataset.csv") |>
  select(PRO_COM, year, contains("patents"))
exhibitions <- read_csv2("./data/exhibitions_final_dataset.csv") |>
  select(PRO_COM_T, year, count, contains("complexity"))

# Add the dependent variables to the dataset
base <- base |>
  left_join(patents)
base <- base |>
  left_join(exhibitions)
base <- base |> 
  distinct()
# Now add the base dataset to the geofile to make the final dataset (without control variables)
final <- left_join(base, geofile, by = c("PRO_COM" = "PRO_COM"))

# Add zero's for observations we observe, but not for "gap years" in the exhibition
exhibition_years <- c(1855, 1867, 1878, 1889, 1900, 1911)
final <- final |> 
  mutate(count = if_else(is.element(year, exhibition_years) & is.na(count), 0, count))

coords <- geofile |>
  st_transform('wgs84') |>
  st_centroid() |> 
  mutate(longitude = st_coordinates(geometry)[,1],
         latitude = st_coordinates(geometry)[,2]) |>
  dplyr::select(PRO_COM, longitude, latitude) |>
  st_drop_geometry()

# Set the data to the correct years
final <- final |>
  left_join(coords,by="PRO_COM") |>
  mutate(allegiance_1861 = factor(allegiance_1861, levels=c("Veneto", "Lombardia"))) |>
  select(-PRO_COM_T.y) |>
  rename(PRO_COM_T = PRO_COM_T.x)

# Integrate the census data in the dataset
geocoded_census <- read_csv2('./data/control_variables/geocoded_census.csv')
years <- c(1861, 1871, 1881, 1901, 1911, 1921, 1931, 1936, 1951, 1961, 1971, 1981, 1991)
colnames(geocoded_census)[2:14] <- years
geocoded_census <- geocoded_census |>
  mutate(`1991`= parse_number(`1991`), `1936`=parse_number(`1936`)) |>
  pivot_longer(`1861`:`1991`, names_to='year', values_to='pop') |>
  select(c(COMUNE, PRO_COM, year, pop)) |>
  mutate(year=as.numeric(year)) |>
  group_by(PRO_COM, year) |>
  summarize(pop = sum(pop))

final <- left_join(final, geocoded_census, by = c("PRO_COM", "year"))

# Now, linearly interpolate the population based on a FE model with time
# The relevant census years are 1861, 1871, 1881, 1901, 1911
# Code the interpolated_pop for all years
final <- final |> 
  group_by(PRO_COM) |> 
  mutate(interpolated_population = case_when(
    year < 1855 ~ NA,
    # Until 1881 Separately Since Lombardy is in the census in 1861 and Veneto is not
    allegiance_1861 == "Lombardia" & year <= 1871 ~ pop[year == 1861] + ((pop[year==1871] - pop[year==1861])/10)*(year - 1861),
    allegiance_1861 == "Lombardia" & between(year, 1872, 1881) ~ pop[year == 1871] + ((pop[year==1881] - pop[year==1871])/10)*(year - 1871),
    allegiance_1861 == "Veneto" & year <= 1881 ~ pop[year == 1871] + ((pop[year==1881] - pop[year==1871])/10)*(year - 1871),
    # After 1881 Jointly
    between(year, 1882, 1901) ~ pop[year==1881] + ((pop[year==1901] - pop[year==1881])/20)*(year - 1881),
    between(year, 1902, 1911) ~ pop[year==1901] + ((pop[year==1911] - pop[year==1901])/10)*(year - 1901),
    TRUE ~ NA
  )) |>
  mutate(interpolated_population = case_when(
    between(year, 1855, 1861) & interpolated_population < 0 & interpolated_population[year==1861] > 0 ~ interpolated_population[year==1861],
    between(year, 185, 1861) & interpolated_population < 0 ~ NA,
    year < 1855 ~ NA,
    TRUE ~ interpolated_population
  )) |>
  ungroup()

# Interpolate the population before 1855 with a logarithmic relationship - per municipality separately
## Compute the log pop = \beta (t-1855) on a couple of data points after 1855 and predict values for t<1855
grouped_data <- final |>
  group_by(PRO_COM) |>
  group_split()
  
results <- map(grouped_data, function(data){
  out <- NULL
  ip <- data |>
    filter(between(year, 1855, 1860)) |>
    select(interpolated_population) |>
    pull()
  
  if(all(!is.na(ip))) {
    data <- data |>
      mutate(yearp = year - 1855)
    result <- feols(log(interpolated_population) ~ year, data=data)
    predictions <- predict(result, newdata=tibble(year=1804:1854)) |> exp()
    
    rest <- data |>
      filter(between(year, 1855, 1911)) |>
      select(interpolated_population) |>
      pull()
    
    comb <- c(predictions, rest)
    out <- tibble(PRO_COM=data$PRO_COM[1],year=1804:1911, pred=comb)
  }
  return(out)
})

final <- left_join(final, results |> bind_rows(),
          by=c("PRO_COM", "year")) |>
  select(-interpolated_population) |>
  rename(interpolated_population = pred)

# Compute the per-capita equivalents of DV's
final <- final |>
  mutate(across(contains("patents_"), ~ .x / interpolated_population, .names = "{.col}_pc"),
         count_pc = count / interpolated_population)

# Correct Definitions (Put this in data_wrangling_final_ds.R)
final <- final |> 
  # Add the Erfindungen (patents_austria) to the "patents_together_verz_italy" variable (keep the name for convenience)
  rowwise() |>
  mutate(patents_together_verz_italy = patents_together_verz_italy + patents_austria) |>
  ungroup() |>
  mutate(patents_together_verz_italy = if_else(is.na(patents_together_verz_italy), 0, patents_together_verz_italy),
         patents_together_verz_italy_pc =patents_together_verz_italy / interpolated_population)

# Group a couple of years together for the years it is possible
final <- final |>
  mutate(year_group = case_when(
    between(year, 1821, 1823) ~ "1822",
    between(year, 1832, 1834) ~ "1833",
    between(year, 1843, 1845) ~ "1844",
    between(year, 1855, 1857) ~ "1855",
    between(year, 1865, 1867) ~ "1867", 
    year == 1878 ~ "1878",
    year == 1889 ~ "1889", 
    year == 1902 ~ "1902",
    year == 1911 ~ "1911",
    TRUE ~ NA
  ))



# Export dataset to csv
if(export){
  final |> 
    select(-geometry) |>
    write_csv2("./data/final_dataset.csv")
}

