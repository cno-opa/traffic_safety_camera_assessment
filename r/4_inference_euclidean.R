# Dependencies ------------------------------------------------------------

library(broom)
library(caret)
library(feather)
library(forcats)
library(FNN)
library(ggmap)
library(ggrepel)
library(pdist)
library(rgdal)
library(stringr)
library(tidyverse)

sites_long <- read_feather("./data/sites_long.feather")
cameras <- read_feather("data/cameras.feather")

date_lookup <- cameras %>% 
  select(cit_date, cam_dt) %>% 
  arrange(cam_dt) %>% 
  unique()

treatment <- sites_long %>% 
  semi_join(cameras, by = c("road_id", "cit_date")) %>%
  filter(crashes_before > 0)

no_cams <- sites_long %>% 
  filter(!has_cam)

set.seed(1)

k <- 10

comp <- NULL

for (i in seq_along(treatment$road_id)) {

  test_site <-  treatment$road_id[i]
  
  query_set <- treatment %>% filter(road_id == test_site)
  
  date_id <- query_set %>% .$cit_date
  
  query_val <- query_set %>% 
    select(crashes_before:score_before) %>%  
    as.numeric()
  
  search_space <- no_cams %>% filter(cit_date == date_id)
  
  # remove previously used comparison sites from search space
  if (i != 1) {
    
    search_space <- search_space %>% 
      filter(!(road_id %in% comp$road_id))
    
  }
  
  query_xy <- query_set %>% select(x, y) %>% as.matrix()
  search_xy <- search_space %>% select(x, y) %>% as.matrix()
  dists <- pdist(query_xy, search_xy)@dist
  search_space <- search_space[dists > 0.006 & dists < 0.075,]
  
  nn <- NULL
  
  for (j in 1:k) {
    
    search_mat <- search_space %>% 
      select(crashes_before:score_before) %>%  
      as.matrix()
  
    # Euclidean distance
    query_mat <- matrix(query_val, ncol = 3, nrow = 1)
    dimnames(query_mat)[[2]] <- dimnames(search_mat)[[2]]
    pp <- preProcess(search_mat, method = c("center", "scale"))
    search_mat <- predict(pp, search_mat)
    query_mat <- predict(pp, query_mat)
    indices_euclidean <- get.knnx(search_mat, query_mat, k = 1)[["nn.index"]]
    nn_euclidean <- search_space[indices_euclidean,] %>% 
      mutate(treatment_id = test_site) %>% 
      select(treatment_id, everything())
    
    if (j == 1) {
      nn <- nn_euclidean
    } else {
      nn <- nn %>% bind_rows(nn_euclidean)
    }
    
    query_xy <- nn_euclidean %>% select(x, y) %>% as.matrix()
    search_xy <- search_space %>% select(x, y) %>% as.matrix()
    dists <- pdist(query_xy, search_xy)@dist
    search_space <- search_space[dists > 0.006,]
  
  }
  
  if (i == 1) {
    comp <- nn
  } else {
    comp <- comp %>% bind_rows(nn)
  }
  
}

# Inference ---------------------------------------------------------------

comp_agg <- comp %>% 
  group_by(treatment_id) %>% 
  summarize(control_before = sum(crashes_before),
            control_after = sum(crashes_after),
            control_change = (control_after - control_before) / control_before * 100)

comp_mean <- comp %>% 
  mutate(control_change = (crashes_after - crashes_before) / crashes_before * 100) %>% 
  group_by(treatment_id) %>% 
  summarize(control_change = mean(control_change))

test <- treatment %>% 
  left_join(comp_mean, by = c("road_id" = "treatment_id")) %>% 
  mutate(treatment_change = (crashes_after - crashes_before) / crashes_before * 100) %>% 
  left_join(cameras, by = "road_id") %>% 
  select(road_id, cam_desc, treatment_change, control_change, x, y)

t.test(test$treatment_change, test$control_change, alternative = "less", paired = TRUE) %>% print()
t.test(test$treatment_change, test$control_change, alternative = "less", paired = FALSE) %>% print()
t.test(test$treatment_change, test$control_change, alternative = "two.sided", paired = TRUE) %>% print()
t.test(test$treatment_change, test$control_change, alternative = "two.sided", paired = FALSE) %>% print()

# Simple linear regression --------------------------------------------------------------

regression_data <- comp %>% 
  select(-treatment_id) %>% 
  bind_rows(treatment) %>% 
  left_join(date_lookup, by = "cit_date") %>% 
  mutate(change_crash = (crashes_after - crashes_before)/crashes_before,
         change_injured = (injured_after - injured_before)/injured_before,
         change_score = (score_after - score_before)/score_before)

mod <- lm(data = regression_data,
           formula = change_crash ~ has_cam + crashes_before + injured_before + score_before + cam_dt)
mod_sum <- summary(mod)
print(mod_sum)
print(mod_sum$fstatistic)
print(mod_sum$r.squared)
print(confint(mod))

write_csv(tidy(mod), "data/regression_gaussian.csv")

# Difference of differences -----------------------------------------------

regression_data_long <- regression_data %>% 
  gather(key = time_period, val = crashes, crashes_before, crashes_after) %>% 
  mutate(time_period = fct_relevel(time_period, "crashes_before", "crashes_after"))

mod_diff <- glm(data = regression_data_long,
                formula = crashes ~ (has_cam * time_period) + has_cam + time_period + poly(injured_before, 2) + score_before + cam_dt,
                family = quasipoisson())
print(summary(mod_diff))
print(confint(mod_diff))
print(exp(coef(mod_diff)["has_camTRUE:time_periodcrashes_after"]))

write_csv(tidy(mod_diff), "data/regression_poisson.csv")

# Plots -------------------------------------------------------------------

print(test %>%
        gather(group, value, treatment_change:control_change) %>% 
        ggplot(aes(fct_reorder(group, value, median), value, fill = group)) +
        geom_boxplot() +
        xlab("") +
        ylab("Percent change in crashes") +
        ggtitle("Percent change in crashes, before versus after month of initial citations") +
        labs(caption = "Similar locations identified by number of crashes and injuries before initial citation date") +
        scale_x_discrete(labels = c("Traffic cameras", "Comparison sites")) +
        scale_fill_discrete(labels = c("Traffic cameras", "Comparison sites")) +
        theme(text = element_text(size = 16), legend.position = "none"))

print(test %>%
        mutate(diff = treatment_change - control_change) %>% 
        arrange(diff) %>% 
        mutate(cam_desc = str_c(1:nrow(test), cam_desc, sep = " ")) %>%
        mutate(cam_desc = fct_reorder(factor(cam_desc), -diff)) %>%
        ggplot(aes(cam_desc, diff)) +
        geom_bar(stat = "identity") +
        geom_hline(aes(yintercept = mean(diff)), color = "green", linetype = "longdash", size = 2) +
        coord_flip() +
        xlab("") +
        ylab("") +
        ggtitle("Difference in change in crashes, camera locations versus comparison sites",
                subtitle = paste0("Negative value indicates that camera location performed better; dashed line indicates mean difference")) +
        labs(caption = "Similar locations identified by number of crashes and injuries before initial citation date") +
        theme(text = element_text(size = 16),
              legend.title = element_blank()))

# Mapping -----------------------------------------------------------------

treatment_point <- test[sample(nrow(test), 1),]
control_points <- comp %>% filter(treatment_id == treatment_point$road_id)

coordinates(control_points) <- ~ x + y
proj4string(control_points) <- CRS("+proj=longlat +ellps=WGS84")

print(ggmap(get_map(control_points@bbox, zoom = 13, color = "bw")) + 
        geom_point(data = treatment_point,
                   mapping = aes(x = x, y = y),
                   color = "red",
                   size = 5,
                   alpha = 0.5) + 
        geom_point(data = as_tibble(control_points),
                   mapping = aes(x = x, y = y),
                   color = "blue",
                   size = 2,
                   alpha = 0.5) + 
        geom_label_repel(data = as_tibble(control_points),
                  mapping = aes(x = x, y = y, label = loc_name)) +
        geom_label_repel(data = as_tibble(treatment_point),
                  mapping = aes(x = x, y = y, label = cam_desc)) +
        ggtitle(paste0("Comparison sites for ", treatment_point$cam_desc),
                subtitle = "Camera site in red; comparison sites in blue"))

control_table <- control_points@data %>% 
  select(loc_name, crashes_before:score_after)

print(treatment %>% filter(road_id == treatment_point$road_id) %>% select(loc_name, crashes_before:score_after))
print(control_table)

write_csv(control_table, "data/control_table.csv")
