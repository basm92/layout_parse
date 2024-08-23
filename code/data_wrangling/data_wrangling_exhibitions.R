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
## Make a list of possible municipalities to be matched to
municipalities <- read_sf("./shapefiles_images/italy_admin_borders/Limiti1991/Com1991/Com1991_WGS84.shp")
compartimenti <- read_sf('./shapefiles_images/italy_admin_borders/Limiti_1871/Compartimenti_1871/Compartimenti_1871.shp')
municipalities_with_compartimenti <- st_intersection(municipalities, compartimenti) |>
  mutate(area=st_area(geometry),
         overlap = as.numeric(area / Shape_Area)) |>
  group_by(PRO_COM_T) |>
  filter(overlap == max(overlap) | is.na(overlap)) |>
  select(PRO_COM_T, COMUNE, COD_COMP, DEN_COMP) |>
  st_drop_geometry() |>
  mutate(score = NA)

# Define a function to replace accented characters
replace_accents <- function(text) {
  # Define a named vector for replacement
  replacements <- c(
    "à" = "a'", "è" = "e'", "ì" = "i'", "ò" = "o'", "ù" = "u'",
    "À" = "A'", "È" = "E'", "Ì" = "I'", "Ò" = "O'", "Ù" = "U'",
    "á" = "a'", "é" = "e'", "í" = "i'", "ó" = "o'", "ú" = "u'",
    "Á" = "A'", "É" = "E'", "Í" = "I'", "Ó" = "O'", "Ú" = "U'",
    "â" = "a'", "ê" = "e'", "î" = "i'", "ô" = "o'", "û" = "u'",
    "Â" = "A'", "Ê" = "E'", "Î" = "I'", "Ô" = "O'", "Û" = "U'",
    "ä" = "a'", "ë" = "e'", "ï" = "i'", "ö" = "o'", "ü" = "u'",
    "Ä" = "A'", "Ë" = "E'", "Ï" = "I'", "Ö" = "O'", "Ü" = "U'"
  )
  
  # Replace accented characters with their corresponding replacements
  text <- str_replace_all(text, replacements)
  
  return(text)
}

## Add customized databases of translations
french <- read_delim('./data/dictionaries/french_italian_names_dictionary.txt', delim=' - ', ) |> 
  janitor::clean_names() |>
  mutate(french = str_trim(french)) |>
  distinct()

english <- read_delim('./data/dictionaries/english_italian_names_dictionary.txt', delim=' - ') |>
  janitor::clean_names() |>
  mutate(english = str_trim(english)) |>
  distinct()


geocode_row <- function(row){
  exact_match <- tibble(PRO_COM_T = NA, COMUNE = NA, score=NA)
  # 1. Exact Match Raw
  place_raw <- row$place
  exact_match <- municipalities_with_compartimenti |>
    filter(COMUNE == place_raw) |>
    select(PRO_COM_T, COMUNE, score)
  
  if(nrow(exact_match)>0){
    return(exact_match)
  }
  # 2. Exact Match Clean 
  ## 2.1 Remove Spaces
  place_clean <- row$place |>
    str_remove_all("à |\\.|\\((.+)\\)|-") |>
    str_trim()
  exact_match <- municipalities_with_compartimenti |>
      filter(COMUNE == place_clean) |>
      select(PRO_COM_T, COMUNE, score)
    
    if(nrow(exact_match)>0){
      return(exact_match)
    }

  # 3. Exact Match Under Translation
  ## 3.1 French
  place_clean <- row$place |>
    str_remove_all("à |\\.|\\((.+)\\)") |>
    str_trim()
  
  match <- french |>
    filter(french == place_clean) |>
    select(italian) |>
    rename(COMUNE = italian)
  
  if(nrow(match)>0){
    exact_match <- municipalities_with_compartimenti |>
      filter(COMUNE == match$COMUNE) |>
      select(PRO_COM_T, COMUNE, score)
    return(exact_match)
  }
  ## 3.2 English
  match <- english |>
    filter(english == place_clean) |>
    select(italian) |>
    rename(COMUNE = italian)
  if(nrow(match)>0){
    exact_match <- municipalities_with_compartimenti |>
      filter(COMUNE == match$COMUNE) |>
      select(PRO_COM_T, COMUNE, score)
    return(exact_match)
  }
  
  # 4. Exact Match without spaces and capitals in both lists
  place_lowercase <- place_clean |>
    str_remove_all(" ") |>
    tolower()
  
  exact_match <- municipalities_with_compartimenti |>
    mutate(COMUNE = tolower(COMUNE)) |>
    filter(COMUNE == place_lowercase) |>
    select(PRO_COM_T, COMUNE, score)
  if(nrow(exact_match)>0){
    return(exact_match)
  }
  
  # 5. Exact Match with only alphabetic characters
  place_alphabetic <- place_clean |>
    str_remove_all("[^A-Za-z]") |>
    tolower()
  
  exact_match <- municipalities_with_compartimenti |>
    mutate(COMUNE = tolower(str_remove_all(COMUNE, "[^A-Za-z]"))) |>
    filter(COMUNE == place_alphabetic) |>
    select(PRO_COM_T, COMUNE, score)
  
  if(nrow(exact_match)>0){
    return(exact_match)
  }
  
  # 6. Exact Match For Partial String in Place
  match <- place_raw |> 
    str_extract("(?:à |de |d' )(\\b\\w+\\b)") |>
    str_remove("de |à ")
  
  exact_match <- municipalities_with_compartimenti |>
    filter(COMUNE == match) |>
    select(PRO_COM_T, COMUNE, score)
  
  if(nrow(exact_match)>0){
    return(exact_match)
  }
  
  # 7. Replace all accents by <character>'
  place_noaccents <- place_clean |> 
    replace_accents()
  exact_match <- municipalities_with_compartimenti |>
    filter(COMUNE == place_noaccents) |>
    select(PRO_COM_T, COMUNE, score)
  
  if(nrow(exact_match)>0){
    return(exact_match)
  }
  
  # Name String
  ## 7.1 Name String - Look for Last Word Match
  last_word <- row$name |>
    str_extract("\\s\\S+$")
  
  clean_last_word <- last_word |>
    str_remove_all("à |\\.|\\((.+)\\)|\\(|\\)|-|\\s")
  
  ### 7.1.1 Exact Match
  exact_match <- municipalities_with_compartimenti |>
    filter(COMUNE == clean_last_word) |>
    select(PRO_COM_T, COMUNE, score)
  
  if(nrow(exact_match)>0){
    return(exact_match)
  }
  ### 7.1.2 Translation
  match <- french |>
    filter(french == clean_last_word) |>
    select(italian) |>
    rename(COMUNE = italian)
  
  if(nrow(match)>0){
    exact_match <- municipalities_with_compartimenti |>
      filter(COMUNE == match$COMUNE) |>
      select(PRO_COM_T, COMUNE, score)
    return(exact_match)
  }
  
  ## 7.2 Name Identifier on the basis of a or de
  ### 7.2.1 Simple Case
  name_raw <- row$name
  match <- name_raw |> 
    str_extract("(?<=(de|à)\\s)\\S+") |>
    str_remove_all("de |à ")
  
  exact_match <- municipalities_with_compartimenti |>
    filter(COMUNE == match) |>
    select(PRO_COM_T, COMUNE, score)
  
  if(nrow(exact_match)>0){
    return(exact_match)
  }
  ### 7.2.2 Translation
  match <- french |>
    filter(french == match) |>
    select(italian) |>
    rename(COMUNE = italian)
  
  if(nrow(match)>0){
    exact_match <- municipalities_with_compartimenti |>
      filter(COMUNE == match$COMUNE) |>
      select(PRO_COM_T, COMUNE, score)
    return(exact_match)
  }
  
  ### 7.3.3 Translation (for d') - Original and Translation
  match_updated <- name_raw |>
    str_extract("d'[^ ]+") |>
    str_remove("d'")
  
  exact_match <- municipalities_with_compartimenti |>
    filter(COMUNE == match_updated) |>
    select(PRO_COM_T, COMUNE, score)
  
  if(nrow(exact_match)>0){
    return(exact_match)
  }
  
  match <- french |>
    filter(french == match_updated) |>
    select(italian) |>
    rename(COMUNE = italian)
  
  if(nrow(match)>0){
    exact_match <- municipalities_with_compartimenti |>
      filter(COMUNE == match$COMUNE) |>
      select(PRO_COM_T, COMUNE, score)
    return(exact_match)
  }
  ### 7.3 Slight More Complicated with More Words
  ## Regex for second word with capital: (?:\b[A-Z][a-z]*\b.*?){1}\b[A-Z][a-z]*\b
  ## But many have Sous-commission de <city with 2 words>
  match <- name_raw |> 
    str_extract("(d'|de |à ).*") |>
    str_remove("de |à |d'")
  
  match <- french |>
    filter(french == match) |>
    select(italian) |>
    rename(COMUNE = italian)
  
  if(nrow(match)>0){
    exact_match <- municipalities_with_compartimenti |>
      filter(COMUNE == match$COMUNE) |>
      select(PRO_COM_T, COMUNE, score)
    return(exact_match)
  }
  
  ## 7.4 Before the Comma - Original And Translation
  # Original
  match <- place_raw |>
    str_extract("^[^,]+") |>
    str_remove(",")
  
  exact_match <- municipalities_with_compartimenti |>
    filter(COMUNE == match) |>
    select(PRO_COM_T, COMUNE, score)
  
  if(nrow(exact_match)>0){
    return(exact_match)
  }
  
  # Translation
  match <- french |>
    filter(french == match) |>
    select(italian) |>
    rename(COMUNE = italian)
  
  if(nrow(match)>0){
    exact_match <- municipalities_with_compartimenti |>
      filter(COMUNE == match$COMUNE) |>
      select(PRO_COM_T, COMUNE, score)
    return(exact_match)
  }
  
  ## 7.5 First Word - Original And Translation
  # Original
  match <- place_raw |>
    str_extract("\\b[A-Z][^\\s]*\\b")
  exact_match <- municipalities_with_compartimenti |>
    filter(COMUNE == match) |>
    select(PRO_COM_T, COMUNE, score)
  
  if(nrow(exact_match)>0){
    return(exact_match)
  }
  
  # Translation
  match <- french |>
    filter(french == match) |>
    select(italian) |>
    rename(COMUNE = italian)
  
  if(nrow(match)>0){
    exact_match <- municipalities_with_compartimenti |>
      filter(COMUNE == match$COMUNE) |>
      select(PRO_COM_T, COMUNE, score)
    return(exact_match)
  }
  
  
  # 12. Bracket Stuff
  # 12. Exact Match Proceeding from Raw - In Brackets
  ## 12.1 Very raw
  brackets <- place_raw |>
    str_extract("\\((.+)\\)") |>
    str_remove_all("\\(|\\)")
  
  exact_match <- municipalities_with_compartimenti |>
    filter(COMUNE == brackets) |>
    select(PRO_COM_T, COMUNE, score)
  
  if(nrow(exact_match)>0){
    return(exact_match)
  }
  
  ## 12.2 Alphabetical
  brackets_alphabet <- place_raw |>
    str_extract("\\([A-Za-z]+\\)") |>
    str_remove_all("\\(|\\)")
  
  exact_match <- municipalities_with_compartimenti |>
    filter(COMUNE == brackets_alphabet) |>
    select(PRO_COM_T, COMUNE, score)
  
  if(nrow(exact_match)>0){
    return(exact_match)
  }
  
  # 13. Exact Match Brackets - Translation
  ## 13.1 French
  brackets_clean <- brackets 
  
  match <- french |>
    filter(french == brackets_clean) |>
    select(italian) |>
    rename(COMUNE = italian)
  
  if(nrow(match)>0){
    exact_match <- municipalities_with_compartimenti |>
      filter(COMUNE == match$COMUNE) |>
      select(PRO_COM_T, COMUNE, score)
    return(exact_match)
  }
  
  ## Try again with cleaner string
  brackets_clean <- brackets |>
    str_remove_all("[^A-Za-z]")
  
  match <- french |>
    filter(french == brackets_clean) |>
    select(italian) |>
    rename(COMUNE = italian)
  
  if(nrow(match)>0){
    exact_match <- municipalities_with_compartimenti |>
      filter(COMUNE == match$COMUNE) |>
      select(PRO_COM_T, COMUNE, score)
    return(exact_match)
  }
  
  ## 13.2 English
  match <- english |>
    filter(english == brackets_clean) |>
    select(italian) |>
    rename(COMUNE = italian)
  if(nrow(match)>0){
    exact_match <- municipalities_with_compartimenti |>
      filter(COMUNE == match$COMUNE) |>
      select(PRO_COM_T, COMUNE, score)
    return(exact_match)
  }
  
  # 11. Finally, Fuzzy Match with high threshold
  ## First, Place without brackets
  place_without_brackets <- str_remove_all(place_raw, "\\((.+)\\)") |>
    str_squish()
  
  matrix <- stringdist(place_without_brackets,
                       municipalities_with_compartimenti$COMUNE,
                       method='jw')
  index <- which.min(matrix)
  score <- min(matrix)
  match_name <- municipalities_with_compartimenti[index,]$COMUNE
  match_PRO_COM_T <- municipalities_with_compartimenti[index,]$PRO_COM_T
  exact_match <- tibble(PRO_COM_T = match_PRO_COM_T, COMUNE = match_name, score = score)
  
  ## Then, Place focusing on the brackets
  place_brackets <- str_extract(place_raw, "\\((.+)\\)") |>
    str_remove_all("\\(|\\)")
  
  matrix <- stringdist(place_without_brackets,
                       municipalities_with_compartimenti$COMUNE,
                       method='jw')
  index <- which.min(matrix)
  score_2 <- min(matrix)
  
  if(score_2 < score & !is.na(place_brackets)) {
    match_name <- municipalities_with_compartimenti[index,]$COMUNE
    match_PRO_COM_T <- municipalities_with_compartimenti[index,]$PRO_COM_T
    exact_match <- tibble(PRO_COM_T = match_PRO_COM_T, COMUNE = match_name, score = score_2)
  }
  
  return(exact_match)
  
}

test <- individual_with_class |>
  rowwise() |>
  mutate(geo = list(geocode_row(pick(everything()))))

test <- test |>
  unnest_wider(geo)

# Aggregate the data to the (year, city, count) level and use geocode
aggregated_data <- individual_with_class |>
  group_by(year, place) |>
  summarize(count = n(), 
            average_complexity = mean(pci, na.rm=T),
            top_complexity = max(pci))



