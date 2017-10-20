# Dependencies ------------------------------------------------------------

library(feather)
library(lubridate)
library(stringr)
library(tidyverse)
library(zoo)

# Data loading and cleaning -----------------------------------------------

raw_cameras <- read_csv("data/cameras_roads.csv")
raw_crashes <- read_csv("data/crashes_roads.csv")
roads_point <- read_csv("data/roads_point.csv")

cameras <- raw_cameras
names(cameras) <- str_to_lower(names(cameras)) 

cameras <- cameras %>% 
  select(road_id = join_fid, fullname, 5:9) %>% 
  mutate(cam_dt = as.yearmon(cam_dt)) %>% 
  arrange(cam_dt)

cit_dates <- unique(cameras[["cam_dt"]])

# Assign unique identifier for beginning citation date at each location
# in order to separate data into before and after periods.
# More than one camera may have started issuing citations on the same date

for (i in seq_along(cit_dates)) {
  cameras[cameras$cam_dt == cit_dates[i], "cit_date"] <- i
}

crashes <- raw_crashes
names(crashes) <- str_to_lower(names(crashes))

crashes <- crashes %>% 
  mutate(crash_date = as.yearmon(mdy_hms(crash_date)))

key <- tibble(severity_cd = c("A", "B", "C", "D", "E"),
              weight = c(36, 36, 4, 1, 0.04))

crashes <- crashes %>% 
  mutate(injured = num_tot_kil + num_tot_inj) %>% 
  left_join(key, by = "severity_cd") %>% 
  select(road_id = join_fid,
         crash_id = target_fid,
         r_pri_road_name:r_inter_road,
         crash_date,
         fullname,
         injured,
         weight)

crash_names <- crashes %>% 
  mutate(r_pri_road_name = if_else(is.na(r_pri_road_name), "", r_pri_road_name),
         r_inter_road = if_else(is.na(r_inter_road), "", r_inter_road),
         loc_name = str_c(r_pri_road_name, r_inter_road, sep = " & ")) %>% 
  group_by(road_id) %>% 
  summarize(loc_name = names(which.max(table(loc_name))))
    
  

# Take subset of crashes for debugging
# crashes <- crashes[sample(nrow(crashes), 10000),]

# Data transformation and aggregation -------------------------------------

before <- NULL
after <- NULL

for (i in seq_along(cit_dates)) {
  
  print(cit_dates[i])
  
  df_before <- crashes %>%
    filter(crash_date < cit_dates[i],
           crash_date >= cit_dates[i] - 3) %>% 
    group_by(road_id) %>%
    summarize(crashes_before = n(),
              injured_before = sum(injured),
              score_before = sum(weight)) %>% 
    mutate(cit_date = i)
  
  df_after <- crashes %>%
    filter(crash_date > cit_dates[i],
           crash_date <= cit_dates[i] + 3) %>% 
    group_by(road_id) %>%
    summarize(crashes_after = n(),
              injured_after = sum(injured),
              score_after = sum(weight)) %>% 
    mutate(cit_date = i)
  
  if (i == 1) {
    before <- df_before
    after <- df_after
  } else {
    before <- before %>% bind_rows(df_before)
    after <- after %>% bind_rows(df_after)
  }
  
}

road_ids <- rep(unique(crashes$road_id), each = length(cit_dates))

sites <- tibble(road_id = road_ids, cit_date = rep(1:length(cit_dates), length(unique(crashes$road_id))))

sites_long <- sites %>% 
  left_join(before, by = c("road_id", "cit_date")) %>% 
  left_join(after, by = c("road_id", "cit_date")) %>% 
  replace_na(list(crashes_before = 0,
                  injured_before = 0,
                  score_before = 0,
                  crashes_after = 0,
                  injured_after = 0,
                  score_after = 0))

cam_sites <- cameras %>% 
  left_join(sites_long, by = c("road_id", "cit_date")) %>% 
  group_by(cam_id) %>% 
  arrange(desc(crashes_before), desc(injured_before), desc(score_before)) %>%
  slice(1) %>% 
  arrange(cam_id) %>% 
  ungroup() %>% 
  select(cam_id, cam_desc, road_id, road_name = fullname, cam_dt:cam_lo, cit_date)

road_names <- crashes %>% 
  select(road_id, road_name = fullname) %>% 
  unique()

roads_point <- roads_point %>% 
  select(road_id = ORIG_FID,
         x = POINT_X,
         y = POINT_Y)

sites_long <- sites_long %>% 
  left_join(cam_sites, by = "road_id") %>% 
  mutate(has_cam = !is.na(cam_dt)) %>% 
  left_join(road_names, by = "road_id") %>% 
  select(road_id,
         road_name = road_name.y,
         has_cam, 
         cit_date = cit_date.x,
         crashes_before:score_after) %>% 
  left_join(roads_point, by = "road_id") %>% 
  left_join(crash_names, by = "road_id") %>% 
  select(road_id, road_name, loc_name, everything())

# Output ------------------------------------------------------------------

write_feather(sites_long, "data/sites_long.feather")
write_feather(cam_sites, "data/cameras.feather")
