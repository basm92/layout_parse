library(tidyverse); library(janitor); library(readxl)
# Need exhibitions from 1867 to 1900 in this folder
files_to_import <- list.files('./data/raw_exhibition_data/italy_exhibitions_ocr/', full.names = T)
imported_files <- map(files_to_import, ~ read_delim(.x, delim = "\t"))
# Extract the correct variables and do some preliminary cleaning
together <- list_rbind(imported_files) |>
  clean_names() |>
  mutate(year = str_extract(pdf, "\\d{4}"),
         page = as.numeric(str_extract(pdf, "\\d+(?=\\D*$)"))) |>
  arrange(year, page) |>
  group_by(year, page) |>
  mutate(position_on_page = row_number()) |>
  ungroup() |>
  select(-pdf)

# Load the class boundaries
source('./data/dictionaries/class_boundaries.R')
class_boundary <- class_boundary |>
  mutate(year = as.character(year))
# Add the classes to the together dataset
together <- together |>
  left_join(class_boundary, by = c('year', 'page', 'position_on_page')) |>
  fill(class)


# And two separate exhibition data from 1855 and 1911
# These are individual-level datasets but lack description of exhibition/name
exh_1911 <- read_xlsx('./data/raw_exhibition_data/1911_italy.xlsx') |>
  clean_names() |>
  filter(country_1 == "Italia") |>
  select(location_1, class_number, nuvas_abridged_class) |>
  rename(location = location_1, class = class_number, rca_class = nuvas_abridged_class) |>
  mutate(year = "1911")

exh_1855 <- read_xlsx('./data/raw_exhibition_data/Exhibitions_Lombardo_Veneto_1855.xlsx') |>
  clean_names() |>
  select(-seqn) |>
  mutate(year = as.character(year))

together_1855_1911 <- bind_rows(exh_1911, exh_1855) |>
  rename(place = location)

# Add the 1855-1911 to the rest of the data
final <- together |>
  bind_rows(together_1855_1911)

# Add the complexity index
complexity <- read_delim('./data/raw_outcomes_data/complexity.csv') |>
  mutate(year = as.character(year))

individual_with_class <- final |>
  left_join(complexity, by = c('year', 'class')) |>
  mutate(class_description = if_else(is.na(rca_class.x), rca_class.y, rca_class.x)) |>
  select(-c(rca_class.x, rca_class.y))

# Save this as an intermediate-level exhibition dataset
write_csv2(individual_with_class, "./data/intermediate_exhibition_data/individual_level_exhibition_data.csv")
# Import it again
individual_with_class <- read_csv2('./data/intermediate_exhibition_data/individual_level_exhibition_data.csv')
# Geocode this data


# Aggregate the data to the (year, city, count) level and use geocode
aggregated_data <- individual_with_class |>
  group_by(year, place) |>
  summarize(count = n(), average_complexity = mean(pci, na.rm=T), top_complexity = max(pci))



