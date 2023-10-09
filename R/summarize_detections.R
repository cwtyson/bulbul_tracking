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
  filter(!is.na(tag)) %>% 
  filter(species == "DCBU")  %>% 
  filter(year == "2023")

tagged <- unique(tag_log$bird_band)

## Individual summaries by day
dets_sum <- dcbu_dets %>%
  mutate(dt_r = lubridate::round_date(date_time, unit = "1 day")) %>%
  group_by(bird_band, grid_point, dt_r) %>%
  summarise(dets = n(),
            rssi = max(rssi))

min_rssi <- min(dets_sum$rssi)
max_rssi <- max(dets_sum$rssi)
min_dets <- min(dets_sum$dets)
max_dets <- max(dets_sum$dets)
min_date <- min(spmo_dets$date_time)
max_date <- max(spmo_dets$date_time)

## For each, save a timeline
for(bb in tagged){
  
  # bb = "4A94812"
  
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
