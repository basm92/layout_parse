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

# Add the class information in this file
# class_boundary identifies the observation (year, page, row_number, class) where a new class begins
class_boundary <- tribble(~ year, ~ page, ~position_on_page, ~class,
                          1867, 0, 1, 6,
                          1867, 2, 1, 7,
                          1867, 3, 15, 8,
                          1867, 4, 17, 9,
                          1867, 5, 24, 10,
                          1867, 6, 22, 11,
                          1867, 8, 1, 12,
                          1867, 11, 1, 13,
                          1867, 13, 1, 14,
                          1867, 15, 10, 15,
                          1867, 18, 30, 16,
                          
                          )

# And two separate exhibition data from 1855 and 1911
# These are individual-level datasets but lack description of exhibition/name
exh_1911 <- read_xlsx('./data/raw_exhibition_data/1911_italy.xlsx') |>
  clean_names() |>
  filter(country_1 == "Italia") |>
  select(location_1, class_number, nuvas_abridged_class) |>
  rename(location = location_1, class = class_number) |>
  mutate(year = "1911")

exh_1855 <- read_xlsx('./data/raw_exhibition_data/Exhibitions_Lombardo_Veneto_1855.xlsx') |>
  clean_names() |>
  select(-seqn) |>
  mutate(year = as.character(year))

together_1855_1911 <- bind_rows(exh_1911, exh_1855)
