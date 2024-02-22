library(tidyverse)
library(stringdist)
library(giscoR)
library(sf)
source('./code/data_wrangling/helper_matching_functions.R')

geocode_data <- function(directory, municipality_names){
  # Inputs: directory of dataset with variable respones from chatgpt (respones) and hard-coded location (location)
  # Municipality names should have column LAU_NAME
  dataset <- read_csv(directory)
  # Load the dataset of to be matched observations and filter the parasite observations
  raw_data <- dataset |> 
    filter(!is.na(location))
  
  ## Pre-processing: Remove all accents from raw_data and also from municipalities to increase the
  ## pool of exact matching (implement later)
  
  # First step: Match the raw-coded data to the municipalities
  # Step 1: Exact match based on hard code
  step1 <- raw_data |> 
    left_join(municipality_names, 
              by= c("location" = "LAU_NAME"))
  
  # Step 2: Exact match based on ChatGPT guess
  step2 <- step1 |> 
    mutate(candidate_exact = str_extract_all(respones, '\\b[A-Z][a-z]*\\b')) |>
    mutate(candidate_exact_filtered = map(candidate_exact, ~{
      elements <- .x
      elements_to_remove <- forbidden_words
      
      for (word in elements_to_remove) {
        elements <- elements[!str_detect(tolower(elements), paste0("\\b", tolower(word), "\\b"))]
      }
      
      elements
    }))
  
  
  # Step 2 Matching Algorithm:
  ## For each row entry, we'll loop through the possible entries, and match as soon as there is a match
  ## For Compound Names: If there is no match after having looped through the entries, 
  ## string paste the entries together and see if there is a match
  step2 <- step2 |> 
    rowwise() |> 
    mutate(candidate_match_step2 = list(find_candidate_match(cur_data())))
  
  ## Step 3: Go from the name matches to the LAU numbers and coalesce the LAU columns together
  
  # Find the LAU Number:
  step3 <- step2 |> 
    mutate(lau_id_step2 = list(find_lau_number(cur_data()))) |>
    unnest(lau_id_step2, keep_empty = TRUE) |>
    mutate(LAU_ID = coalesce(LAU_ID, lau_id_step2))
  
  # Step 4: Make a translation-based exact match from French and then English
  step3 <- step3 |>
    rowwise() |> 
    mutate(candidate_match_step3 = find_translation_match(cur_data()))
  
  # Convert to the LAU numbers finally
  step4 <- step3 |>
    rowwise() |> 
    mutate(temp_lau_id = find_lau_no_again(cur_data())) |>
    mutate(LAU_ID = coalesce(LAU_ID, temp_lau_id)) |>
    select(c(text, source, class, location , LAU_ID, candidate_match_step2))
  
  # Step 5: Now use first candidate_match_step2 to put them together and fuzzy string match
  # Threshold boundary:  0.15 seems like a good threshold - implement this tomorrow
  step5 <- step4 |> 
    rowwise() |> 
    mutate(candidate_match_step4 = list(fuzzy_match(cur_data()))) |>
    rowwise() |> 
    mutate(temp_lau = allocate_the_match(cur_data())) |>
    mutate(LAU_ID = coalesce(LAU_ID, temp_lau))
  
  ## Step 6: If the score is too low, then go back to location and fuzzy string match: On hard coded location
  # Threshold boundary: 
  step6 <- step5 |> 
    rowwise() |> 
    mutate(candidate_match_step6 = list(fuzzy_match_hardcode(cur_data())))
  
  step6 <- step6 |>
    rowwise() |> 
    mutate(temp_lau2 = allocate_the_match_step6(cur_data())) |>
    mutate(LAU_ID = coalesce(LAU_ID, temp_lau2)) |>
    select(c(text, source, class, location, LAU_ID))
  
  # Finally, write to csv
  name <- paste0(str_remove(directory, "\\.csv"), "_geomatched.csv", collapse='')
  step6 |> write_csv2(name)
  
}

# Load the dataset of candidate matches
municipality_names <- giscoR::gisco_get_lau(country="Italy", year="2016") |>
  st_drop_geometry() |> 
  select(LAU_NAME, LAU_ID) |> 
  as_tibble()

#geocode_data('./data/1867_italy_chatgpt.csv', municipality_names)

geocode_data('./data/1878_italy_chatgpt.csv', municipality_names)
