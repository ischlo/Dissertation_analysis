#### libs ####

library(sf)
library(tidyverse)
library(ggplot2)
library(here)
library(tmap)
library(tmaptools)
library(igraph)
library(doParallel)
library(stplanr)
# performance monitoring
library(profvis)
library(bench)
library(pryp)
library(concaveman)
library(osmdata)

#### DATA ####
westminsterbb
haringeybb <- getbb("Haringey, London, UK", format_out = "sf_polygon") %>% 
  st_transform(27700)

haringey_road <- getOSMdata(haringeybb 
                            ,k = "highway"
                            ,val = c("primary"
                                     ,"secondary"
                                     ,"tertiary"
                                     ,"residential"
                                     ,"pedestrian")) %>%
  osm_poly2line() %>% 
  .$osm_lines %>% 
  select(osm_id, name,highway,geometry)%>%
  st_set_crs(4326) %>% 
  st_transform(27700) %>%
  filter(st_intersects(haringeybb, sparse = FALSE))

westminster_roads <- getOSMdata(westminsterbb %>% st_transform(4326) 
                            ,k = "highway"
                            ,val = c("primary"
                                     ,"secondary"
                                     ,"tertiary"
                                     ,"residential"
                                     ,"pedestrian")) %>%
  osm_poly2line() %>% 
  .$osm_lines %>% 
  select(osm_id, name,highway,geometry)%>%
  st_set_crs(4326) %>% 
  st_transform(27700) %>%
  filter(st_intersects(westminsterbb, sparse = FALSE))

haringeybb %>% qtm()

#### SELECTED NEIGHBOURHOODS #### 

haringey_neighbourhood <- neighbourhoods_iso_hull %>%
  filter(st_intersects(geometry,haringeybb %>% st_transform(27700), sparse = FALSE))

westminster_neighbourhood <- neighbourhoods_iso_hull %>%
  filter(st_intersects(geometry,westminsterbb , sparse = FALSE))

tmap_mode("plot")
neighbourhoods_iso_map <- 
  tm_shape(westminster_neighbourhood) + tm_polygons(col = "cluss_iso"
                                       ,style = "cat"
                                       ,palette = "Set1"
                                       ,alpha = 1
                                       #,size = 0.05
  ) + 
  tm_shape(westminster_roads) + tm_lines(col = "black") + 
  tm_layout(legend.show = FALSE
            ,main.title = "Neighbourhoods of Westminster"
            ,title = "isochrones t = 7 min") + tm_scale_bar()
neighbourhoods_iso_map
