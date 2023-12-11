library(tidyverse)
library(stringdist)
library(giscoR)
library(sf)

# Load the dataset of candidate matches
municipality_names <- giscoR::gisco_get_lau(country="Italy", year="2016") |>
  st_drop_geometry() |> 
  select(LAU_NAME, LAU_ID) |> 
  as_tibble()

# Load the dataset of to be matched observations and filter the parasite observations
raw_data <- read_csv('./data/1867_italy_chatgpt.csv')
raw_data <- raw_data |>
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
  mutate(candidate_exact = str_extract_all(respones, '\\b[A-Z][a-z]*\\b'))

forbidden_words <- c("Italian",
                     "Italy", 
                     "French", 
                     "The", 
                     "There",
                     "Please",
                     "English",
                     "Sorry", 
                     "I", 
                     "It",
                     "In",
                     "An",
                     "Assuming",
                     "Unfortunately")

step2 <- step2 |> 
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
find_candidate_match <- function(row){
  
  match_vector <- row$candidate_exact_filtered

  if(rlang::is_empty(match_vector[[1]])){
    outcome <- "NA"
    return(outcome)
  } else {
  
  for(i in match_vector[[1]]){
    possible_match <- municipality_names |>
      filter(LAU_NAME == i) 
    
    if(nrow(possible_match) > 0){
      break
    }
  }
  
  if(nrow(possible_match) > 0){
    outcome <- possible_match |> 
      select(LAU_NAME) |>
      pull()
  } else {
    outcome <- str_c(match_vector[[1]])
  }
  
  return(outcome)
  }
  
}

step2 <- step2 |> 
  rowwise() |> 
  mutate(candidate_match_step2 = list(find_candidate_match(cur_data())))

## Step 3: Go from the name matches to the LAU numbers and coalesce the LAU columns together
find_lau_number <- function(row) {
  flat_list <- row$candidate_match_step2[[1]]
  if(length(flat_list) > 1 | length(flat_list) == 0){
    return(NA_character_)
  } else{
    lau_id <- municipality_names |> 
      filter(LAU_NAME == flat_list) |>
      select(LAU_ID) |> 
      pull()
    
    return(lau_id)
  }
}

# Find the LAU Number:
step3 <- step2 |> 
  mutate(lau_id_step2 = list(find_lau_number(cur_data())))

# Flatten the list
step3 <- step3 |>
  unnest(lau_id_step2, keep_empty = TRUE)

# Put the LAU numbers from step 1 and step 2 together
step3 <- step3 |> 
  mutate(LAU_ID = coalesce(LAU_ID, lau_id_step2))
  
# Step 4: Go to Python and explore a couple of libraries in there
step3 |>
  select(-c(candidate_exact, candidate_exact_filtered)) |>
  unnest_wider(candidate_match_step2, names_sep="_") |>
  write_csv2('./data/interim_matched_data.csv')
