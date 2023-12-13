# helper_matching_functions
# List of filtered words
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
                     "Based",
                     "A",
                     "It",
                     "In",
                     "An",
                     "Assuming",
                     "Unfortunately")

# Step 2:
find_candidate_match <- function(row){
  
  match_vector <- row$candidate_exact_filtered
  
  if(rlang::is_empty(match_vector[[1]])){
    outcome <- NA_character_
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

# Step 3:
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

# Step 4:
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

# Step 4: Integrate lau numbers
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

# Step 5: Fuzzy match
fuzzy_match <- function(row){
  
  candidate_match <- NA
  score <- NA
  
  if(is.na(row$LAU_ID)){
    names <- row$candidate_match_step2[[1]]
    together <- paste0(unique(names), collapse=' ')
    matrix <- stringdist(together, municipality_names$LAU_NAME, method='jw')
    index <- which.min(matrix)
    score <- min(matrix)
    candidate_match <- municipality_names$LAU_NAME[index]
  }
  
  return(c(candidate_match, score))
}


allocate_the_match <- function(row){
  
  score <- row$candidate_match_step4[[1]][2]
  out <- NA_character_
  if(!is.na(score)){
    out <- if_else(as.numeric(score) < 0.15, 
                   municipality_names |>
                     filter(LAU_NAME == row$candidate_match_step4[[1]][1]) |>
                     select(LAU_ID) |>
                     pull() |>
                     pluck(1),
                   NA_character_)
  }
  
  return(out)
}


# Step 6: Fuzzy match on hardcoded location
fuzzy_match_hardcode <- function(row){
  
  candidate_match <- NA
  score <- NA
  
  if(is.na(row$LAU_ID)){
    names <- row$location
    matrix <- stringdist(names, municipality_names$LAU_NAME, method='jw')
    index <- which.min(matrix)
    score <- min(matrix)
    candidate_match <- municipality_names$LAU_NAME[index]
  }
  
  return(c(candidate_match, score))
}


allocate_the_match_step6 <- function(row){
  
  score <- row$candidate_match_step6[[1]][2]
  out <- NA_character_
  if(!is.na(score)){
    out <- if_else(as.numeric(score) < 0.101, 
                   municipality_names |>
                     filter(LAU_NAME == row$candidate_match_step6[[1]][1]) |>
                     select(LAU_ID) |>
                     pull() |>
                     pluck(1),
                   NA_character_)
  }
  
  return(out)
}
