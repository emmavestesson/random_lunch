library("osmdata")
library(sf)
library(units)
library(leaflet)
library(shiny)
library(lwgeom)
library(tidyverse)
library(here)
library(janitor)


q0 <- opq(bbox='greater london uk', timeout = 10000) 


amenity<- q0[["osm_points"]][["amenity"]]
table(amenity)

restaurants <- add_osm_feature(opq = q0, key = 'amenity', value = "restaurant") %>% 
  osmdata_sf()

cafe <- add_osm_feature(opq = q0, key = 'amenity', value = "cafe") %>% 
  osmdata_sf() 

fast_food <- add_osm_feature(opq = q0, key = 'amenity', value = "fast_food")  %>% 
  osmdata_sf() 

pub <- add_osm_feature(opq = q0, key = 'amenity', value = "pub")  %>% 
  osmdata_sf() 

# Combine different food place
food_places <- c(restaurants, cafe, fast_food, pub) 
food_places <- food_places$osm_points 

food_places_cg <- food_places %>% 
  filter(!is.na(name)) %>% 
  mutate(as.character(cuisine)) 

food_places_cg <- remove_empty(food_places_cg)
# food_places_cg <- st_transform(food_places_cg, crs = 4326)


work_coor <-data.frame(longitude=-0.10640000000000782, latitude=51.5133)
work_coor <- st_as_sf(work_coor, coords = c("longitude", "latitude"), crs = 4326)
work_coor <- st_transform(work_coor, crs=st_crs(food_places_cg, asText = TRUE))
distance <- st_distance(work_coor, food_places_cg)
head(distance) 

food_places_cg$distance_from_work <- t(round(distance))
food_places_cg$label <- paste0("<b>", food_places_cg$name, "</b> <br>", 
                               "Distance: ", food_places_cg$distance_from_work, "m")

food_places_cg1 <- st_coordinates(food_places_cg) %>% 
  as.data.frame()
food_places_cg1$name <- food_places_cg$name 
food_places_cg1$distance_from_work <- t(round(distance))
food_places_cg1$label <- paste0("<b>", food_places_cg1$name, "</b> <br>", 
                               "Distance: ", food_places_cg1$distance_from_work, "m")

saveRDS(food_places_cg1,file=here::here("greater_ldn.rds"))
