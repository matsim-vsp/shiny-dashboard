require(shiny)
require(plotly)
require(tidyverse)
require(sf)
require(bslib)
require(lubridate)
require(leaflet)
require(shinydashboard)

# specify file paths

main_file <- "data/berlin-v5.5.3-10pct.output_trips.csv"
comparison_data1 <- "data/berlin-v5.5.3-10pct.output_trips.csv"
comparison_data2 <- "data/berlin-v5.5.3-10pct.output_trips.csv"

spatial_data_shp <- "C:/Users/J/Documents/VSP_Berlin/Shapes/bezirke_laender.shp"


# load and transform data

trips <- read.csv(main_file, sep = ";", nrows= 1000)
trips_comp1 <- read.csv(comparison_data1, sep = ";", nrows= 15000) %>%
  slice_sample(n =1000)
trips_comp2 <- read.csv(comparison_data2, sep = ";", nrows= 15000) %>%
  slice_sample(n =1000)

trips$dep_time <- strptime(trips$dep_time, tz= "", format = "%H:%M:%S")
trips_comp1$dep_time <- strptime(trips_comp1$dep_time, tz= "", format = "%H:%M:%S")
trips_comp2$dep_time <- strptime(trips_comp2$dep_time, tz= "", format = "%H:%M:%S")

spatial_data_shp <- read_sf(spatial_data_shp) %>%
  st_transform(crs = 4326)

# columns with coordinates are used to georeference the csv file (here: start of trip), csv file is joined to spatial file
# not all functions work on simple features (georeferenced data) so that column is dropped again


sf_trips_start <- st_as_sf(trips, coords = c("start_x", "start_y"), crs = 31468)%>%
  st_transform(crs = 4326)
sf_trips_start_comp1 <- st_as_sf(trips_comp1, coords = c("start_x", "start_y"), crs = 31468)%>%
  st_transform(crs = 4326)
sf_trips_start_comp2 <- st_as_sf(trips_comp2, coords = c("start_x", "start_y"), crs = 31468)%>%
  st_transform(crs = 4326)

trips_spatial_data <- st_join(sf_trips_start, spatial_data_shp)
trips_spatial_data_comp1 <- st_join(sf_trips_start_comp1, spatial_data_shp)
trips_spatial_data_comp2 <- st_join(sf_trips_start_comp2, spatial_data_shp)

trips_key <- st_drop_geometry(trips_spatial_data)
trips_key_comp1 <- st_drop_geometry(trips_spatial_data_comp1)
trips_key_comp2 <- st_drop_geometry(trips_spatial_data_comp2)


#functions are defined

modal_split_trips_longest_mode <- function(x){
  x %>%
    count(longest_distance_mode) %>%
    mutate(percent= 100*n/sum(n))
}

modal_split_trips_longest_mode_by_spatial_data <- function(x){
  x %>%
    group_by(Name) %>%
    count(longest_distance_mode) %>%
    group_by(Name) %>%
    mutate(percent = 100*n/sum(n))
}

modal_split_trips_main_mode <- function(x){
  x %>%
    count(main_mode) %>%
    mutate(percent = 100*n/sum(n))
}

modal_split_trips_main_mode_by_spatial_data <- function(x){
  x %>%
    group_by(Name) %>%
    count(main_mode) %>%
    group_by(Name) %>%
    mutate(percent = 100*n/sum(n))
}

modal_split_distance_longest_mode <- function(x){
  x %>%
    group_by(longest_distance_mode) %>%
    summarise(distance = sum(traveled_distance)) %>%
    mutate(percent = round(100*distance/sum(distance),2))
}

modal_split_distance_longest_mode_by_spatial_data <- function(x){
  x %>%
    group_by(Name, longest_distance_mode) %>%
    summarise(distance = sum(traveled_distance)) %>%
    mutate(percent = round(100*distance/sum(distance),2))
}

modal_split_distance_main_mode <- function(x){
  x %>%
    group_by(main_mode) %>%
    summarise(distance = sum(traveled_distance)) %>%
    mutate(percent = round(100*distance/sum(distance),2))
}

modal_split_distance_main_mode_by_spatial_data <- function(x){
  x %>%
    group_by(Name,main_mode) %>%
    summarise(distance = sum(traveled_distance)) %>%
    mutate(percent = round(100*distance/sum(distance),2))
}

modal_split_main_mode_sorted_by_distance <- function(x){
  x %>%
    mutate(sorted_distance = if_else(traveled_distance<1000, "less than 1km",
                                     if_else(traveled_distance>= 1000 & traveled_distance<2000, "1 - 2km",
                                             if_else(traveled_distance>=2000 & traveled_distance<5000, "2 - 5km",
                                                     if_else(traveled_distance>=5000 & traveled_distance<10000, "5 - 10km",
                                                             if_else(traveled_distance>=10000, "more than 10km", "ERROR")))))) %>%
    group_by(sorted_distance) %>%
    count(main_mode)
}

trips_aggregated_15_mins <- function(x){
  x%>%
    mutate(time_slot = floor_date(dep_time, unit = period(num = 15, units = "minutes"))) %>%
    mutate(time_slot = substr(as.character(time_slot), 12, 19)) %>%
    separate(end_activity_type, into = c("end_activity", "x"), sep = "_")
}



#some calculations are done outside of the server function to minimize waiting time

#modal split by number of trips
ms_trips_longest_mode <- modal_split_trips_longest_mode(trips_key) %>%
  saveRDS("data/ms_trips_longest_mode")
ms_trips_longest_mode_comp1 <- modal_split_trips_longest_mode(trips_key_comp1)%>%
  saveRDS("data/ms_trips_longest_mode_comp1")
ms_trips_longest_mode_comp2 <- modal_split_trips_longest_mode(trips_key_comp2)%>%
  saveRDS("data/ms_trips_longest_mode_comp2")

ms_trips_longest_mode_spatial <- modal_split_trips_longest_mode_by_spatial_data(trips_key) %>%
  saveRDS("data/ms_trips_longest_mode_spatial")
ms_trips_longest_mode_spatial_comp1 <- modal_split_trips_longest_mode_by_spatial_data(trips_key_comp1)%>%
  saveRDS("data/ms_trips_longest_mode_spatial_comp1")
ms_trips_longest_mode_spatial_comp2 <- modal_split_trips_longest_mode_by_spatial_data(trips_key_comp2)%>%
  saveRDS("data/ms_trips_longest_mode_spatial_comp2")

ms_trips_main_mode <- modal_split_trips_main_mode(trips_key)%>%
  saveRDS("data/ms_trips_main_mode")
ms_trips_main_mode_comp1 <- modal_split_trips_main_mode(trips_key_comp1)%>%
  saveRDS("data/ms_trips_main_mode_comp1")
ms_trips_main_mode_comp2 <- modal_split_trips_main_mode(trips_key_comp2)%>%
  saveRDS("data/ms_trips_main_mode_comp2")

ms_trips_main_mode_spatial <- modal_split_trips_main_mode_by_spatial_data(trips_key)%>%
  saveRDS("data/ms_trips_main_mode_spatial")
ms_trips_main_mode_spatial_comp1 <- modal_split_trips_main_mode_by_spatial_data(trips_key_comp1)%>%
  saveRDS("data/ms_trips_main_mode_spatial_comp1")
ms_trips_main_mode_spatial_comp2 <- modal_split_trips_main_mode_by_spatial_data(trips_key_comp2)%>%
  saveRDS("data/ms_trips_main_mode_spatial_comp2")

#modal split by distance
ms_distance_longest_mode <- modal_split_distance_longest_mode(trips_key) %>%
  saveRDS("data/ms_distance_longest_mode")
ms_distance_longest_mode_comp1 <- modal_split_distance_longest_mode(trips_key_comp1)%>%
  saveRDS("data/ms_distance_longest_mode_comp1")
ms_distance_longest_mode_comp2 <- modal_split_distance_longest_mode(trips_key_comp2)%>%
  saveRDS("data/ms_distance_longest_mode_comp2")

ms_distance_longest_mode_spatial <- modal_split_distance_longest_mode_by_spatial_data(trips_key) %>%
  saveRDS("data/ms_distance_longest_mode_spatial")
ms_distance_longest_mode_spatial_comp1 <- modal_split_distance_longest_mode_by_spatial_data(trips_key_comp1)%>%
  saveRDS("data/ms_distance_longest_mode_spatial_comp1")
ms_distance_longest_mode_spatial_comp2 <- modal_split_distance_longest_mode_by_spatial_data(trips_key_comp2)%>%
  saveRDS("data/ms_distance_longest_mode_spatial_comp2")

ms_distance_main_mode <- modal_split_distance_main_mode(trips_key)%>%
  saveRDS("data/ms_distance_main_mode")
ms_distance_main_mode_comp1 <- modal_split_distance_main_mode(trips_key_comp1)%>%
  saveRDS("data/ms_distance_main_mode_comp1")
ms_distance_main_mode_comp2 <- modal_split_distance_main_mode(trips_key_comp2)%>%
  saveRDS("data/ms_distance_main_mode_comp2")

ms_distance_main_mode_spatial <- modal_split_distance_main_mode_by_spatial_data(trips_key)%>%
  saveRDS("data/ms_distance_main_mode_spatial")
ms_distance_main_mode_spatial_comp1 <- modal_split_distance_main_mode_by_spatial_data(trips_key_comp1)%>%
  saveRDS("data/ms_distance_main_mode_spatial_comp1")
ms_distance_main_mode_spatial_comp2 <- modal_split_distance_main_mode_by_spatial_data(trips_key_comp2)%>%
  saveRDS("data/ms_distance_main_mode_spatial_comp2")


#modal split sorted by distance (main mode)
ms_main_mode_sorted_by_distance <- modal_split_main_mode_sorted_by_distance(trips_key) %>%
  saveRDS("data/ms_main_mode_sorted_by_distance")
ms_main_mode_sorted_by_distance_comp1 <- modal_split_main_mode_sorted_by_distance(trips_key_comp1) %>%
  saveRDS("data/ms_main_mode_sorted_by_distance_comp1")
ms_main_mode_sorted_by_distance_comp2 <- modal_split_main_mode_sorted_by_distance(trips_key_comp2) %>%
  saveRDS("data/ms_main_mode_sorted_by_distance_comp2")

ms_main_mode_sorted_by_distance_table <- as.data.frame(readRDS("data/ms_main_mode_sorted_by_distance")) %>%
  spread(sorted_distance, n)
ms_main_mode_by_distance_table <- ms_main_mode_sorted_by_distance_table[, c(1,5,2,3,4,6)] %>%
  saveRDS("data/ms_main_mode_sorted_by_distance")

ms_main_mode_sorted_by_distance_table_comp1 <- as.data.frame(readRDS("data/ms_main_mode_sorted_by_distance_comp1")) %>%
  spread(sorted_distance, n)
ms_main_mode_sorted_by_distance_table <- ms_main_mode_sorted_by_distance_table_comp1[, c(1,5,2,3,4,6)] %>%
  saveRDS("data/ms_main_mode_sorted_by_distance_comp1")

ms_main_mode_sorted_by_distance_table_comp2 <- as.data.frame(readRDS("data/ms_main_mode_sorted_by_distance_comp2")) %>%
  spread(sorted_distance, n)
ms_main_mode_sorted_by_distance_table <- ms_main_mode_sorted_by_distance_table_comp2[, c(1,5,2,3,4,6)] %>%
  saveRDS("data/ms_main_mode_sorted_by_distance")

#daily load curve: 15 minute chunks
trips_15_mins <- trips_aggregated_15_mins(trips_key) %>%
  saveRDS("data/trips_15_mins")
trips_15_mins_comp1 <- trips_aggregated_15_mins(trips_key_comp1) %>%
  saveRDS("data/trips_15_mins_comp1")
trips_15_mins_comp2 <- trips_aggregated_15_mins(trips_key_comp2) %>%
  saveRDS("data/trips_15_mins_comp2")


trips %>% saveRDS("data/trips")
trips_key %>% saveRDS("data/trips_key")

trips_comp1 %>% saveRDS("data/trips_comp1")
trips_key_comp1 %>% saveRDS("data/trips_key_comp1")

trips_comp2 %>% saveRDS("data/trips_comp2")
trips_key_comp2 %>% saveRDS("data/trips_key_comp2")