#### LIBS ####

library(rlist)
library(tidyverse)
library(sf)
library(tmap)
library(rlist)
library(av)
library(gifski)
library(foreach)
library(doParallel)

#### CLEANING env ####

# maps <- NULL
# 
# neighbourhood <- NULL

#### Loading data ####

# maps <- list.load("london_animation_maps/maps.rdata")
# 
# maps

neighbourhood <- london_neighbourhood %>% 
  dplyr::filter(visits >= 70)
 
hcluster <- london_hclust

thames <- getOSMdata(londonbb %>% st_transform(4326)
                     ,k = "water", val = "river")  %>% 
  osmdata::osm_poly2line() %>%
  .$osm_lines %>% 
  dplyr::select(osm_id,name,geometry) %>% 
  st_set_crs(4326) %>%
  st_transform(27700)

thames %>% sf::st_write("thames.geojson")

# optional: 
roads <- thames

# when from scratch, provide neighbourhood data set and hierar tree.

#
#### CREATING ANIMATION ####
tmap_mode("view")

area_name <- "London"
n <- 500

registerDoParallel(3)
maps <- foreach(i = seq(from = 1,to = n+1, by = 20)) %dopar% {
  neighbourhood_grouped <- neighbourhood %>%
    mutate(groups = cutree(hcluster, k = i)) %>%
    group_by(groups) %>%
    #filter(n()>10) %>%
    summarise(geometry = st_union(geometry,by_feature = FALSE))
  tm_shape(neighbourhood_grouped) + tm_polygons(col = "MAP_COLORS"
                                                ,palette = "Pastel1"
                                                ,border.col = "dimgray"
                                                ,alpha = 1) +
    tm_shape(thames) + tm_lines(col = "darkblue") +
    tm_scale_bar(text.size = 1.1
                 ,position = c("left","bottom")) +
    tm_layout(main.title = paste("Neighbourhood formation, k = ",as.character(i), sep = ""))

}
stopImplicitCluster()

#list.save(westminster_neighbourhood_map, "westminster_neighbourhood_map.rdata")

tmap_animation(maps
               ,filename = paste(area_name,"neighbourhood_animation300_500.mp4",sep = "_")
               ,height = 540
               ,width = 860
               ,delay = 40
               ,loop = FALSE)

