install.packages('stplanr', repos="http://cran.r-project.org")
.libPaths()
#### Libraries ####

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

#### memory usage. ####

gc() #This will free up unused RAM
gcinfo(TRUE)

mem_used()

#### FUNCTIONS ####
source("functions.R")

#### Data ####
 
# # data set of amenities with the entropy and size variables.
# london_entropy <- sf::st_read(dsn = here::here("london_entropy.geojson"))


#### Marylebone example of clustering ####

marylebone <- neighbourhoods %>% filter(neighbourhood == "Marylebone") %>%
  st_set_crs(4326) %>%
  st_transform(27700) %>%
  st_buffer(dist = 1000)

tmap_mode("view")
marylebone %>% qtm()

london_amenities_isochrones$id <- as.character(london_amenities$osm_id)
entropy_iso$osm_id <- as.character(london_entropy$osm_id)

marylebone_neighbourhood <- entropy_iso %>% 
  filter(st_intersects(geometry,marylebone, sparse = FALSE))

marylebone_roads <- london_roads %>% 
  st_transform(27700) %>% 
  filter(st_intersects(geometry,marylebone,sparse = FALSE))

marylebone_isochrones <- london_amenities_isochrones %>% 
  filter(id %in% marylebone_neighbourhood$osm_id) %>%
  st_set_crs(4326) %>% 
  st_transform(27700)

marylebone_neighbourhood %>% qtm(dots.col = "entropy"
                                 )

marylebone_neighbourhood %>% filter(osm_id == "802152485")

int <- sf::st_intersects(marylebone_isochrones, marylebone_neighbourhood)
# once we have made the intersection, we need to check that they all intersect at least with their own amenity
# this is not the case every time because some amenities are in locations with no roads around
# while the isochrones uses the underlyinig road network to build the areas. 
marylebone_neighbourhood <- marylebone_neighbourhood %>% 
  dplyr::mutate(ind = 1:nrow(marylebone_neighbourhood))
i <- 1294
nb <- marylebone_neighbourhood[int[[i]],]
indice <- i
glob_ind <- c(i)
#osmid <- data$osm_id[i]
m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
while(m != indice) {
  nb <- marylebone_neighbourhood[int[[m]],]
  indice <- m
  glob_ind <- c(glob_ind,m)
  m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
}
glob_ind

tmap_mode("plot")

marylebone_neighbourhood$steps <- "omitted"

marylebone_neighbourhood$steps[glob_ind] <- paste(c("start", "intermediate max","intermediate max", "local max")
                                                  ,", S="
                                                  ,round(marylebone_neighbourhood$entropy[glob_ind],2)
                                                  ,sep = "")

marylebone_clustering_map <- tm_shape(marylebone_roads
                                      ,bbox = marylebone) + tm_lines(col = "black"
                                      ,alpha = 1) +
  tm_shape(marylebone_neighbourhood) + tm_dots(col = "entropy"
                                               ,palette = "viridis"
                                               ,style = "fisher"
                                               ,size = 0.05
                                               ,breaks = c(.5,1,1.5,2,2.5,3,3.2,3.5)
                                               ) + 
  tm_shape(marylebone_isochrones[glob_ind,]) + tm_polygons(col = "grey"
                                                           ,alpha = 0.2) +
  tm_shape(marylebone_neighbourhood[glob_ind,]) + tm_dots(col = "red") + 
  tm_text(text = "steps"
          ,ymod = 1) + tm_scale_bar(breaks = c(0,0.5,1))

marylebone_clustering_map

tmap_save(marylebone_clustering_map
          ,"marylebone_clustering_map.pdf"
          ,height = h
          ,width = w)


#### neighbourhoods with buffers (NOT USED IN FINAL WORK)####

neighbourhood_buff <- read_csv("neighbourhood_buff2/clus_buff.csv")

neighbourhood_buff

colnames(neighbourhood_buff) <- c("buff500","buff1000","buff1500")

london_entropy_clus <- london_entropy %>% bind_cols(neighbourhood_buff)

tmap_mode("plot")

neighbourhoods_1000 <- tm_shape(london_roads) + tm_lines(col = "black") + 
  tm_shape(london_entropy_clus) + tm_dots(col = "buff1000"
                                     ,palette = "Set1"
                                     ,alpha = 0.5
                                     ,size = 0.01
                                     ) + 
  tm_layout(legend.show = FALSE
            ,main.title = "Neighbourhoods"
            ,title = "Buffers, r = 1000m") + tm_scale_bar()

neighbourhoods_500

tmap_save(neighbourhoods_1000,"neighbourhoods_buff_500.pdf") 


#### neighbourhoods with isochrones ####

neighbourhood_iso <- readr::read_csv("neighbourhood_iso/max_iso.csv") %>% 
  rename("cluss_iso" = ".")

london_amenities <- london_amenities %>% select(osm_id,name,amenity)
london_amenities <- london_amenities %>% bind_cols(neighbourhood_iso)


tmap_mode("plot")
neighbourhoods_iso_map <- tm_shape(london_roads, bbox = westminsterbb) + tm_lines(col = "black") + 
  tm_shape(london_amenities) + tm_dots(col = "cluss_iso"
                                      ,style = "cat"
                                      ,palette = "Set1"
                                      ,alpha = 0.5
                                      ,size = 0.01
  ) + 
  tm_layout(legend.show = FALSE
            ,main.title = "Neighbourhoods"
            ,title = "isochrones t = 7 min") + tm_scale_bar()
neighbourhoods_iso_map

tmap_save(neighbourhoods_iso_map
          ,"neighbourhoods_iso_map.pdf"
          ,height = h
          ,width = w) 

london_amenities %>% filter(cluss_iso == 347)  %>% qtm()

tmap_mode("plot")

## Example of clustering procedure. 

london_amenities

# combined amenities by neighbourhood with count

amenities_combined <- london_amenities %>% 
  group_by(cluss_iso) %>% 
  summarize(combined = st_combine(geometry), number = n()) %>% 
  st_centroid()

# neighbourhoods from concave hulls enveloping the amenities of a single neighbourhood

neighbourhoods_iso_hull <- london_amenities %>% 
  group_by(cluss_iso) %>% 
  group_modify(.,concaveman,concavity = 1) %>% 
  rename(geometry = polygons) %>% 
  st_sf() %>% 
  st_make_valid()

south_londonbb <- sf::st_read("GPS signals part/south_londonbb.geojson") %>%
  st_set_crs(27700)

neighbourhoods_iso_hull %>%
  filter(st_intersects(geometry,south_londonbb, sparse = FALSE)) %>% 
  qtm(fill = "MAP_COLORS", bbox = south_londonbb %>% st_buffer(dist = -1500))


neighbourhoods_iso_hull %>% sf::st_write("neighbourhoods_iso_hull.geojson")

# westminster area 
westminster_iso_hull <- neighbourhoods_iso_hull %>%
  filter(st_intersects(geometry,westminsterbb, sparse = FALSE))

sf::st_write(westminster_iso_hull,"westminster_iso_hull.geojson") 

#### Map making ####

# neighbourhood concave hulls
london_neighbourhoods_artsy <- tm_shape(london_roads) + tm_lines(col="black"
                                  ,alpha = 0.6
                           ) +
  tm_shape(neighbourhoods_iso_hull) + tm_fill(col= "MAP_COLORS"
                                              ,palette = "Set1"
                                              ,alpha = .8
                                              ,style = "cat"
                                              ) +
  tm_layout(legend.show = FALSE
            ,title = "Neighbourhoods") +
  tm_scale_bar()
london_neighbourhoods_artsy

tmap_save(london_neighbourhoods_artsy
          ,"neighbourhoods_iso_pretty.pdf"
          ,height = h
          ,width = w)

##  numer of amenities in neighbourhoods as dots size
amenities_combined_map <- tm_shape(thames) + tm_lines(col = "darkblue") +
  tm_shape(parks) + tm_polygons(col = "green") +
  tm_shape(amenities_combined) + tm_dots(size = "number"
                                         ,legend.size.is.portrait = TRUE
                                         
                                         
  ) +
  tm_layout(main.title = "Neighbouhoods"
            #,legend.height = 1.5
  )
amenities_combined_map

tmap_save(amenities_combined_map
          ,"neighbourhoods_amenities_combined.pdf"
          ,height = h
          ,width = w)

