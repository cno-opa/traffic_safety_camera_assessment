library(readxl)
library(stringr)
library(tidyverse)

raw <- read_excel("./data/camera_locations.xlsx")

cameras <- raw 
names(cameras) <- str_to_lower(names(cameras))

cameras <- cameras %>% 
  filter(!is.na(cit_live_date),
         cit_live_date < '2013-01-01') %>% 
  mutate(loc_desc = str_replace(loc_desc, "[A-Z\\/]{2,3} ", ""),
         loc_desc = str_replace(loc_desc, " AVE ", " "),
         loc_desc = str_replace(loc_desc, "MELPOMENE ST", "MARTIN LUTHER KING JR BLVD")) %>% 
  select(2, 5:7) %>% 
  unique() %>% 
  arrange(longitude, latitude, loc_desc) %>% 
  rename(cam_desc = loc_desc, cam_lo = longitude, cam_la = latitude, cam_dt = cit_live_date)

cameras$cam_id = 1:nrow(cameras)

write_csv(cameras, "./data/cameras.csv")
