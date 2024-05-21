# italy_create_1856_1900_patents
library(sf); library(tidyverse)
# Import the border
border <- st_read('./data/shapefiles_images/old_province_borders_lines/shapefile_italy_1860.shp') |>
  filter(id == 5)

# Load the municipalities
italy_municipalities <- giscoR::gisco_get_lau(country='Italy', year='2016')

# Load the provinces - to filter the relevant municipalities
italy_provinces <- giscoR::gisco_get_nuts(country='Italy', nuts_level='3', year='2016') |> 
  mutate(prov_code =  str_sub(NUTS_ID, 1, 4)) |> 
  filter(is.element(prov_code, c('ITC4', 'ITH3')))

# Filter the relevant municipalities
# Combine overlaps and within
filter_indication <- st_within(italy_municipalities, italy_provinces, sparse=TRUE) 
filter_indication2 <- st_overlaps(italy_municipalities, italy_provinces, sparse=TRUE) 
mask <- apply(filter_indication, 1, any)
mask2 <- apply(filter_indication2, 1, any)
final_mask <- map2_lgl(mask, mask2, ~ any(.x + .y))
relevant_part <- italy_municipalities[final_mask,]

# Plot to check
ggplot() + geom_sf(data=relevant_part) + geom_sf(data=border, color='blue')

# Spatial join
relevant_part <- st_join(relevant_part, italy_provinces, join=st_nearest_feature)
# Calculate centroids of polygons in relevant_part
relevant_centroids <- st_centroid(relevant_part)

# Calculate nearest point on the line for each centroid
nearest_points <- st_nearest_points(relevant_centroids, border)

# Calculate the angle between the line connecting centroids and their nearest points on the border
angles <- numeric(length(nearest_points))
for (i in seq_along(nearest_points)) {
  centroid_coords <- st_coordinates(relevant_centroids[i,])
  nearest_coords <- st_coordinates(nearest_points[i])
  
  # Calculate the X and Y differences (endpoint - origin)
  delta_y <- nearest_coords[2, 2] - nearest_coords[1, 2]
  delta_x <- nearest_coords[2, 1] - nearest_coords[1, 1]
  
  # Calculate the angle using atan2
  angle_radians <- atan2(delta_y, delta_x)
  angles[i] <- angle_radians * (180 / pi)
}

# Add angles to relevant_part
relevant_part$angle_to_line <- angles
relevant_part$group <- if_else(abs(relevant_part$angle_to_line) > 90, "Veneto", "Lombardia")

# Isolate only the relevant variables
relevant_part <- relevant_part |>
  select(-c(contains(c("CNTR_CODE", "NUTS_NAME", "2016", "YEAR", "GISCO_ID"))))

# Create variables for each quarter in the timeframe 1856-1900
dates_start <- seq(as.Date("1856-01-01"), as.Date("1900-10-01"), by = "quarter")
dates_end <- dates_start + months(3)

# Create a df with a start and end for each municipality
grid <- expand_grid(relevant_part$LAU_ID, tibble(ds=dates_start, de=dates_end)) |>
  rename(LAU_ID = `relevant_part$LAU_ID`)

# Merge this with the relevant_part data.frame to obtain the polygon panel
polygon_panel <- grid |>
  left_join(relevant_part, by = "LAU_ID")

# Import the Austrian patent data
patents <- read_sf("./data/austrian_patent_data_geocoded_matched.geojson")

# Make a rowwise() function to extract the patents in location i in quarter t
count_patents <- function(row, patents){
  date_start <- row$ds
  date_end <- row$de
  polygon <- row$`_ogr_geometry_` |> 
    st_buffer(dist=1500)
  found_patents <- patents |> 
    filter(between(granted_on, date_start, date_end)) |>
    st_filter(polygon)
  
  return(nrow(found_patents))
}


pp <- polygon_panel |>
  rowwise() |> 
  mutate(patents = count_patents(cur_data(), patents))

pp <- pp |> 
  mutate(year = year(ds))
# Write to csv
pp |> write_csv2('./data/italy_austrian_patents.csv')

# Integrate the Piedmontese data
pp_austria <- read_csv2('./data/italy_austrian_patents.csv')

# Import the Piedmontese Data
piedmont <- readxl::read_xlsx('./data/Patents_Piedmont_1856_1862.xlsx') |>
  group_by(Period, Location) |> 
  count() |>
  ungroup()

# Geomatch it
library(sf); library(osmdata)
geocode_place <- function(row){
  loc <- row$Location
  out <- tryCatch({
    box <- getbb(loc)
    centroid <- data.frame(x=(box[1,1]+box[1,2])/2, y = (box[2,1]+box[2,2])/2)
    out <- st_as_sf(centroid, coords = c('x', 'y'), crs='wgs84')
    }, 
    error=function(e){
      centroid <- data.frame(x=0, y=0)
      out <- st_as_sf(centroid, coords=c('x', 'y'), crs='wgs84')
    })

  return(out)
}

test <- piedmont |> 
  rowwise() |> 
  mutate(coordinate = geocode_place(cur_data()))

test2 <- test |> 
  unnest_wider(coordinate) |>
  st_as_sf(crs='wgs84') |>
  mutate(year = as.numeric(str_extract(Period, "\\d{4}")),
         quarter = as.numeric(str_extract(Period, "\\d$")))

grid <- expand_grid(relevant_part$LAU_ID, year=1856:1862,quarter=1:4) |>
  rename(LAU_ID = `relevant_part$LAU_ID`)

polygon_panel <- grid |> left_join(relevant_part, by = "LAU_ID")

# Make a rowwise() function to extract the patents in location i in quarter t
# patents argument pertains to test2 data.frame
count_patents_piedmont <- function(row, patents){
  yr <- row$year
  qrt <- row$quarter
  polygon <- row$`_ogr_geometry_` |> 
    st_buffer(dist=1500)
  found_patents <- patents |> 
    filter(year == yr, quarter == qrt) |>
    st_filter(polygon)
  
  out <- found_patents |>
    summarize(total = sum(n, na.rm=T)) |>
    pull(total)
  return(out)
}


pp <- polygon_panel |>
  rowwise() |> 
  mutate(patents = count_patents_piedmont(cur_data(), test2))

# Incorporate the Piedmontese patents and the Austrian patents together
pp_austria_to_be_merged <- pp_austria |>
  filter(is.element(year, 1856:1862)) |>
  group_by(LAU_ID, year) |>
  mutate(quarter = row_number()) |>
  rename(patents_austria = patents) |>
  ungroup()

pp_piedmont_to_be_merged <- pp |>
  rename(patents_piedmont = patents) |>
  ungroup() |>
  st_drop_geometry() |>
  select(c(LAU_ID, year, quarter, patents_piedmont))

# Merge them together
together <- left_join(pp_austria_to_be_merged,  pp_piedmont_to_be_merged,
          by=c("LAU_ID", "year", "quarter")) |>
  mutate(patents = patents_austria + patents_piedmont)

library(fixest)
reg1 <- feols(patents ~ group*post, data = together |> 
                mutate(post = if_else(year > 1859 & ds > ymd("1859-06-01"), 1, 0),
                       patents = patents*1000))

reg2 <- feols(patents ~ as.factor(year)*group + MOUNT_TYPE + COAST_TYPE + URBN_TYPE, data = together |> 
                mutate(patents = patents*1000), vcov='hc1')

# Export the together dataset
# Assuming your tibble is named 'together'

# Extract the coordinates from `_ogr_geometry_` column and convert to sf
together_sf <- left_join(together |>
            select(-`_ogr_geometry_`), 
          italy_municipalities |>
            select(LAU_ID), by= 'LAU_ID') |> 
  st_as_sf() |>
  select(-c(FID.y)) |>
  rename(FID = FID.x)

# Write this to geojson
st_write(together_sf, "./data/italy_austrian_piedmontese_patents.geojson")
