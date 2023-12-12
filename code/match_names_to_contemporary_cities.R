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
                     "One",
                     "Can",
                     "Could",
                     "A",
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
  
# Step 4: Make a translation-based exact match from French and then English
french <- read_delim('./data/french_italian_names_dictionary.txt', delim=' - ', ) |> 
  janitor::clean_names() |>
  mutate(french = str_trim(french)) |>
  distinct()

english <- read_delim('./data/english_italian_names_dictionary.txt', delim=' - ') |>
  janitor::clean_names() |>
  mutate(english = str_trim(english)) |>
  distinct()

find_translation_match <- function(row){
  # For multiple elements
  candidates <- row$candidate_match_step2[[1]]
  match <- NA_character_

  for(i in candidates){
    prelim_match <- french |>
      filter(french == i) 
      
    if(nrow(prelim_match) > 0){
      match <- prelim_match |>
        select(italian) |> 
        pull()
      
    } else{
      prelim_match <- english |>
        filter(english == i)
      
      if(nrow(prelim_match) > 0){
        match <- prelim_match |>
          select(italian) |> 
          pull()
      }
    }
    if(nrow(prelim_match) > 0){
      break
    }
  }
  
  return(match)
}

step3 <- step3 |>
  rowwise() |> 
  mutate(candidate_match_step3 = find_translation_match(cur_data()))

# Convert to the LAU numbers finally
find_lau_no_again <- function(row) {
  if(!is.na(row$candidate_match_step3)){
    lau_id <- municipality_names |> 
      filter(LAU_NAME == row$candidate_match_step3) |>
      select(LAU_ID) |> 
      pull()
  } else{
    lau_id <- NA_character_
  }
    return(lau_id)
}

step4 <- step3 |>
  rowwise() |> 
  mutate(temp_lau_id = find_lau_no_again(cur_data()))

# Merge the LAU ID's and drop some variables
step4 <- step4 |>
  mutate(LAU_ID = coalesce(LAU_ID, temp_lau_id))

step4 <- step4 |> 
  select(c(text, source, class, location , LAU_ID, candidate_match_step2))

# Step 4: Now use first candidate_match_step2 to put them together and fuzzy string match
## If the score is too low, then go back to location and fuzzy string match
fuzzy_match <- function(row){
  
  candidate_match <- NA
  score <- NA
  
  if(is.na(row$LAU_ID)){
  names <- row$candidate_match_step2[[1]]
  together <- paste0(unique(names), collapse=' ')
  matrix <- stringdist(together, municipality_names$LAU_NAME)
  index <- which.min(matrix)
  score <- min(matrix)
  candidate_match <- municipality_names$LAU_NAME[index]
  }
  
  return(c(candidate_match, score))
}

step4[1:100, ] |> 
  rowwise() |> 
  mutate(candidate_match_step4 = list(fuzzy_match(cur_data())))
