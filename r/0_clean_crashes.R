library(rgdal)
library(stringr)
library(tidyverse)

clean_road_name <- function(x) {
  
  x %>% 
    str_replace("^0[:space:]+", "") %>% 
    str_to_upper() %>% 
    str_trim()
  
}

raw <- read_csv("./data/crash3.csv")

crashes <- raw

names(crashes) <- str_to_lower(names(crashes))

# Add markers for data quality issues and missing geographic information
geo_cols <- crashes %>% 
  select(dotd_longitude,
         dotd_latitude,
         r_pri_road_name,
         r_inter_road,
         crash_year,
         crash_date,
         crash_time,
         num_tot_kil,
         num_tot_inj,
         severity_cd) %>% 
  mutate(bad_xy = is.na(dotd_longitude) | is.na(dotd_latitude) | near(dotd_longitude, 0) | near(dotd_latitude, 0),
         bad_pri_road = is.na(r_pri_road_name),
         bad_inter_road = str_detect(r_inter_road, ":") | is.na(r_inter_road),
         r_pri_road_name = clean_road_name(r_pri_road_name),
         r_inter_road = clean_road_name(r_inter_road)) %>% 
  filter(!is.na(crash_date))

# Create lookup table using cross streets with geographic information
imp_lookup <- geo_cols %>% 
  filter(!(bad_xy),
         !(bad_pri_road),
         !(bad_inter_road)) %>% 
  select(r_pri_road_name, r_inter_road, dotd_longitude, dotd_latitude) %>% 
  group_by(r_pri_road_name, r_inter_road) %>% 
  summarize(imp_lo = mean(dotd_longitude),
            imp_la = mean(dotd_latitude))

# Filter for entries not requiring attention
good_xys <- geo_cols %>% 
  filter(!(bad_xy))

# Filter for entries with missing coordinates but good cross streets
bad_xys <- geo_cols %>% 
  filter(bad_xy,
         !(bad_pri_road),
         !(bad_inter_road))

# Remove entries missing both coordinates and good cross streets
no_geo <- geo_cols %>% 
  filter(bad_xy, (bad_pri_road | bad_inter_road))

# Use internal lookup table to impute geographic coordinates
imp_xys <- bad_xys %>% 
  left_join(imp_lookup, by = c("r_pri_road_name", "r_inter_road")) %>%
  select(-c(1:2)) %>% 
  select(dotd_longitude = imp_lo, dotd_latitude = imp_la, everything())

# Filter for entries with missing geographic coordinates
# and cross streets not contained in lookup table
need_geocode <- imp_xys %>% 
  filter(is.na(dotd_longitude) | is.na(dotd_latitude) | near(dotd_longitude, 0) | near(dotd_latitude, 0)) %>% 
  mutate(cross_streets = str_c(r_pri_road_name, r_inter_road, sep = " & "),
         cross_streets = str_c(cross_streets, ", New Orleans, LA"))

imp_xys <- imp_xys %>% 
  anti_join(need_geocode)

# Save entries that need geocoding to working directory
# write_csv(need_geocode, "./data/need_geocode.csv")

# Load table created using ESRI World Geocoder
geocode_lookup <- read_csv("./data/geocode_lookup.csv") %>% 
  select(cross_stre, X, Y) %>% 
  unique()

# Use imported geocoder table to impute geographic coordinates
geocoded <- need_geocode %>% 
  left_join(geocode_lookup, by = c("cross_streets" = "cross_stre")) %>% 
  select(-c(1:2, 14)) %>% 
  select(dotd_longitude = X, dotd_latitude = Y, everything())

# Append entries with valid geographic information
fixed_xy <- rbind(good_xys, imp_xys, geocoded) %>% 
  arrange(crash_date, crash_time)

# Load polygon layer for parish and filter for Orleans
parish <- readOGR('geo', layer = 'tl_2016_us_county')
parish <- parish[parish$GEOID == 22071,]

# Clip entries falling outside Orleans Parish
coordinates(fixed_xy) <- ~ dotd_longitude + dotd_latitude
proj4string(fixed_xy) <- CRS('+proj=longlat +ellps=WGS84')
clean_crashes <- fixed_xy %>% 
  spTransform(CRS(proj4string(parish))) %>% 
  .[parish,] %>% 
  as_tibble() %>% 
  select(-c(9:11))

write_csv(clean_crashes, "./data/crashes.csv")
