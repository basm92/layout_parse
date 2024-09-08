# figure_patents_per_year
## Absolute Number of Patents from Veneto/Lombardia in Austrian Patents, Share of V/L Patents In Total Patents
library(patchwork); library(tidyverse); library(sf)
data_for_urls <- readRDS("./data/patent_data/interim_patent_data/austrian_patent_data.RDS")
clean <- function(df){
  if(class(df$`Shelfmark:`)=="list"){
    out <- unnest(df) |>
      distinct()
  } else{
    out <- df
  }
  return(out)
}

# Clean the universe of patents dataset
data_for_urls <- data_for_urls |>
  map(clean) |>
  list_rbind()

data_for_urls <- data_for_urls |>
  janitor::clean_names() |>
  mutate(across(everything(), ~ str_remove(.x, ":\n")))

# Import and merge the universe of patents dataset with the subset of geocoded Italian Observations
final <- read_csv2("data/final_dataset.csv")

sum_ap_from_italy <- final |> 
  group_by(year) |> 
  summarize(no_patents = sum(patents_austria, na.rm=T))

all_ap <- data_for_urls |>
  mutate(year = as.numeric(str_extract(granted_on, "\\d{4}"))) |>
  group_by(year) |> 
  count()
  
data_for_graph <- left_join(sum_ap_from_italy, all_ap) |>
  mutate(share = no_patents/n) |>
  filter(year < 1900)


g1 <- data_for_graph |> ggplot(aes(year, no_patents)) + geom_line() + geom_point()
g2 <- data_for_graph |> ggplot(aes(year, share)) + geom_line() + geom_point()
# Count the number of V/L Observations and their relative share
plot <- g1 + g2


