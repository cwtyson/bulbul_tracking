## Summarize detections and make maps

## Housekeeping
library(tidyverse)
library(janitor)
library(lubridate)
library(sf)

## Load personal information 
source("personal_info.R")

## Read dets
dcbu_dets <- readRDS("./data/processed_detections/bulbul_detections.RDS")

## Read in tag log
tag_log <- readxl::read_excel(tag_file)  %>%
  clean_names() %>%
  transmute(species,
            tag = gsub("NA",NA, tag),
            bird_band,
            tag_start_time = dmy_hm(paste(date, time),
                                    tz = "Africa/Mbabane"),
            year = format(tag_start_time, "%Y"),
            location)  %>%
  filter(tag != "no tag") %>% 
  filter(species == "DCBU")  %>% 
  filter(year == "2023")

## Individual summaries by day
dets_sum <- dcbu_dets %>%
  mutate(dt_r = lubridate::floor_date(date_time, unit = "1 day")) %>%
  group_by(bird_band, grid_point, dt_r) %>%
  summarise(dets = n(),
            rssi = max(rssi))

## Summary table of detections per individual per day in wide format
sum_tab <- tag_log %>% 
  left_join(dcbu_dets %>% 
              mutate(dt_r = lubridate::floor_date(date_time, unit = "1 day")) %>%
              group_by(bird_band, dt_r) %>%
              summarise(dets = n()) %>% 
              pivot_wider(names_from = dt_r,
                          values_from = dets))


## Values for plotting
min_rssi <- min(dets_sum$rssi)
max_rssi <- max(dets_sum$rssi)
min_dets <- min(dets_sum$dets)
max_dets <- max(dets_sum$dets)
min_date <- min(dets_sum$dt_r)
max_date <- max(dets_sum$dt_r)

## Tagged birds
tagged <- unique(tag_log$bird_band)

## For each, save a timeline 
for(bb in tagged){
  
  # bb = tagged[1]
  
  ## Summarize to plot
  dets_2plot <- dcbu_dets %>%
    filter(bird_band == bb) %>%
    mutate(dt_r = round_date(date_time, "1 day")) %>%
    group_by(bird_band, grid_point, dt_r) %>%
    summarise(max_rssi = max(rssi),
              .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(grid_point = factor(grid_point,
                                      ordered = TRUE,
                                      levels = gtools::mixedsort(unique(grid_point))))  %>% 
    left_join(tag_log %>% 
                select(bird_band,
                       year,
                       location) %>% 
                distinct(bird_band,.keep_all = T))
  
  ## Plot
  dets_ind_plot <- ggplot(dets_2plot) +
    geom_point(aes(x = dt_r, y = grid_point, color = max_rssi)) +
    ggplot2::scale_colour_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"), name = "Max RSSI",
                                    limits = c(min_rssi,max_rssi)) +
    scale_x_datetime(limits = c(min_date,max_date), breaks = "2 weeks", date_labels = "%Y-%m-%d") +
    ggplot2::theme_minimal() +
    labs(x = NULL,y=NULL) +
    ggplot2::theme(axis.text.x = element_text(angle = 45,hjust = 0.8)) +
    labs(title = paste(bb, " - ", unique(format(dets_2plot$dt_r, "%Y")),  " - ", unique(dets_2plot$location)))
  
  
  if(nrow(dets_2plot) == 0){
    
    
    dets_ind_plot <- ggplot(dets_2plot) +
      labs(title = bb)
    
    
  }
  
  ## Save
  suppressMessages(ggplot2::ggsave(paste0("./plots/timelines/",bb,".jpg"),
                                   plot = dets_ind_plot,
                                   scale = 1.5))
  
  
  cat("Saved tag", bb, "\n")
  
}


## Join node to pole point
node_pts <- sf::read_sf(grid_point_file) %>%
  transmute(grid_point  = name) %>%
  mutate(x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
         y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2])

## Summarize per tag, per node, per day
dets_sum <- dcbu_dets %>%
  mutate(dt_r = lubridate::floor_date(date_time, unit = "1 day")) %>%
  group_by(bird_band, grid_point, dt_r) %>%
  summarise(dets = n(),
            rssi = mean(rssi))

min_rssi <- min(dets_sum$rssi)
max_rssi <- max(dets_sum$rssi)
min_dets <- min(dets_sum$dets)
max_dets <- max(dets_sum$dets)


## For band, save a timeline
for(bb in tagged){
  
  ## bb = tagged[1]
  
  dets_map <- dets_sum %>%
    filter(bird_band == bb) %>% 
    mutate(dt_r_f = format(dt_r, "%d-%m")) %>%
    left_join(node_pts) %>%
    left_join(tag_log %>%
                select(bird_band,
                       year,
                       location) %>%
                distinct(bird_band,.keep_all = T))
  
  if(nrow(dets_map) > 1){
    
    (dets_tag_map <- ggplot(dets_map) +
       geom_point(aes(x,y),
                  node_pts,
                  size = 0.8,
                  alpha = 0.3) +
       geom_point(aes(x,y,color = rssi,size=dets)) +
       facet_wrap(.~dt_r) +
       coord_equal() +
       theme_void() +
       labs(title = paste(bb, " - ", unique(dets_map$location))) +
       ggplot2::scale_colour_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                                       name = "Mean RSSI",
                                       limits = c(min_rssi,max_rssi)) +
       ggplot2::scale_size_continuous(limits = c(min_dets,max_dets)))
    
    
  }
  
  if(nrow(dets_map) == 0){
    
    
    dets_tag_map <- ggplot(dets_map) +
      labs(title = bb)
    
  }
  
  
  ## Save
  suppressMessages(ggplot2::ggsave(paste0("./plots/maps/",bb,".jpg"),
                                   plot = dets_tag_map,
                                   scale = 1.5))
  
}

