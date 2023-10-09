## Check tags ##########

## Housekeeping
library(tidyverse)
library(janitor)
library(lubridate)
library(sf)

## Load personal information 
source("personal_info.R")

## Prepare data ############

## Connect to data base back end
conn <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = dbname,
                       user = user,
                       password = password)

## Tags
tag_data <- readxl::read_excel(tag_file)  %>%
  clean_names() %>%
  transmute(species,
            tag = gsub("NA",NA, tag),
            bird_band,
            start_date_time = dmy_hm(paste(date, time),
                                     tz = "Africa/Mbabane"),
            year = format(start_date_time, "%Y"))  %>%
  filter(!is.na(tag)) 

## Keep Elke's bulbuls
dcbu_tags <- tag_data %>% 
  filter(species == "DCBU") %>% 
  pull(tag)

## Read in detections from postgres database
dets <- dplyr::tbl(conn, "raw") %>%
  
  ## Keep only station ids matching the specified filter
  dplyr::filter(station_id %in% c("3DDBDADF9153", 
                                  "39C9F709EC64", 
                                  "31517E791AAE", 
                                  "31556FCE4EEA")) %>%
  
  ## Keep only relevant tags
  dplyr::filter(tag_id %in%  dcbu_tags) %>%

  ## Distinct
  dplyr::distinct(tag_id,
                  node_id,
                  time,
                  .keep_all = T) %>%
  
  dplyr::collect() %>%
  
  ## Select and rename
  dplyr::transmute(node = toupper(node_id),
                   date_time = lubridate::with_tz(time, tz = "Africa/Mbabane"),
                   tag = tag_id,
                   rssi = tag_rssi) %>%
  arrange(date_time)

## Filter erroneous detections
dets_f <- dets %>%
  
  ## Future dates
  filter(date_time < with_tz(Sys.time(), tz = "Africa/Mbabane")) %>%
  
  ## Impossible RSSI values
  filter(rssi < 0)

## Node codes
node_codes <- readxl::read_excel(node_code_file) %>%
  clean_names() %>%
  transmute(node = toupper(node),
            node_number = as.character(node_number))

## Node deployment log
node_log <- readxl::read_excel(node_log_file) %>% 
  dplyr::transmute(grid_point,
                   node_number = as.character(round(as.numeric(node_number),0)),
                   deployment_time = lubridate::dmy_hm(paste(start_date, start_time), 
                                                       tz = "Africa/Mbabane"),
                   removal_time = lubridate::dmy_hm(paste(end_date, end_time),
                                                    tz = "Africa/Mbabane")) %>% 
  ## Join node node
  dplyr::left_join(node_codes,
                   by  = "node_number") %>% 
  
  dplyr::select(node,
                grid_point,
                node_number,
                date_time = deployment_time,
                removal_time)


## Associate node with correct grid point. Convert to data.table and do a roiling join.
nodes <- data.table::data.table(node_log, key = c("node", "date_time"))
dets_f <- data.table::data.table(dets_f, key = c("node", "date_time"))

## Rolling join node log to node records
dets_f <- nodes[dets_f, roll = Inf]

## Remove locations without a grid point and where date time is after removal time
dets_f1 <- dets_f %>%
  dplyr::filter(!is.na(grid_point),
                (date_time < removal_time | is.na(removal_time))) %>%
  
  ## Remove impossible RSSI values
  dplyr::filter(rssi < 0) %>%
  dplyr::arrange(tag,
                 date_time) %>%
  dplyr::select(grid_point,
                tag,
                date_time,
                rssi) %>%
  data.frame()

## Associate tag with correct band Convert to data.table and do a roiling join.
tag_log <- tag_log %>% 
  rename(date_time = tag_start_time)
tag_log <- data.table::data.table(tag_log, key = c("tag", "date_time"))
dets_f1 <- data.table::data.table(dets_f1, key = c("tag", "date_time"))

## Rolling join node log to node records
dets_f1 <- tag_log[dets_f1, roll = Inf]

## Remove tags without a band and where detection time is after removal time
dets_f2 <- dets_f1 %>%
  dplyr::filter(!is.na(bird_band),
                (date_time < tag_removal_time | is.na(tag_removal_time))) %>%
  
  dplyr::arrange(tag,
                 date_time) %>%
  dplyr::select(grid_point,
                bird_band,
                tag,
                date_time,
                rssi) %>%
  data.frame()

## Save detections
saveRDS(dets_f2, "./data/processed_detections/bulbul_detections.RDS")


