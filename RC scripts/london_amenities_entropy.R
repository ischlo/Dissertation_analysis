#### library ####

library(sf) #TO READ SHAPEFILES 
library(tidyverse) #TO MANIPULATE CSV FILES 
library(iterators)
library(tmap)
library(here)
library(dplyr)
library(parallel)
library(osmdata) # to load data from OSM
library(foreach) # for parallel computing
library(doParallel)


#### DEFINING FUNCTIONS ####

# facilitated osm query 
getOSMdata <- function(bb,k, val = "all") {
  
  if (val != "all") { 
    opq(bbox = bb,timeout = 600) %>%
      add_osm_feature(key = k
                      ,value = val
                      ,key_exact = TRUE
                      ,value_exact = TRUE
                      ,match_case = TRUE
      ) %>% osmdata_sf() 
  } else { 
    opq(bbox = bb,timeout = 600) %>% 
      add_osm_feature(key = k
                      #,value = val
                      ,key_exact = TRUE
                      #,value_exact = TRUE
                      #,match_case = TRUE
      ) %>% osmdata_sf() 
  } 
}

osm_group_points <- function(d,k,kofinterest) {
  df <- bind_rows(filter(d$osm_points,.data[[k]] %in% kofinterest)
                  ,filter(d$osm_polygons,.data[[k]] %in% kofinterest) %>% sf::st_centroid()) %>% 
    .[c("osm_id","name",k,"geometry")] %>% rename("amenity" = k) %>%
    sf::st_as_sf()
  df
}


neighbors_entropy_par <- function(d, r = 500, cor_num = 1){
  pts_buf <- sf::st_buffer(d, r)
  int <- sf::st_intersects(pts_buf, d)
  registerDoParallel(cor_num)
  entropy <- foreach(i = 1:nrow(d), .combine=c) %dopar% {
    counted <- d[int[[i]],] %>% dplyr::group_by(amenity) %>% dplyr::count()
    p <- counted$n/sum(counted$n)
    e <- c(-sum(p*log(p)),sum(counted$n))
  }
  stopImplicitCluster()
  data.frame(matrix(entropy, ncol = 2, byrow = TRUE)) %>% rename("entropy" = "X1", "size" = "X2")
}

neighbors_entropy_iso <- function(d, iso, cor_num = 1){
  # make sure that the isochrones data is perfectly alligned by row with the data.
  int <- sf::st_intersects(iso, d)
  # once we have made the intersection, we need to check that they all intersect at least with their own amenity
  # this is not the case every time because some amenities are in locations with no roads around
  # while the isochrones uses the underlyinig road network to build the areas. 
  checks <- map(int,length) %>% unlist() %>% tibble()
  # when the intersection is zero, we impose that there is just the amenity for which the isochrone is computed
  bad_values <- which(checks$. == 0)
  # put the index of the amenity itself in the intersectio
  int[bad_values] <- bad_values
  
  registerDoParallel(cor_num)
  entropy <- foreach(i = 1:nrow(d), .combine=c) %dopar% {
    counted <- d[int[[i]],] %>% dplyr::group_by(amenity) %>% dplyr::count()
    p <- counted$n/sum(counted$n)
    e <- c(-sum(p*log(p)),sum(counted$n))
  }
  entropy
  stopImplicitCluster()
  data.frame(matrix(entropy, ncol = 2, byrow = TRUE)) %>% rename("entropy" = "X1", "size" = "X2")
}


#### Defining the amenities of interest ####
# the set of all the amenities that are considered. 


amenityofinterest <- c("cafe","college","dentist","fire_station","hospital","nightclub","pharmacy","police","pub","recycling","studio"
                       ,"childcare","community_centre","embassy","food_court","marketplace","nursing_home","townhall","veterinary","bank"
                       ,"cinema","clinic","conference_centre","courthouse","crematorium","doctors","events_venue","fast_food","gym"
                       ,"ice_cream","internet_cafe","kindergarten","language_school","library","monastery","music_school","place_of_worship"
                       ,"planetarium","post_office","public_building","public_bookcase","restaurant","school","taxi","theatre","university"
)

shopofinterest <- c("alcohol","appliance","art","bakery","beauty","beverages","bicycle","bookmaker","books","boutique"
                    ,"butcher","camera","cheese","chemist","chocolate","clothes","coffee","computer","convenience"
                    ,"deli","department_store","doityourself","florist","general","greengrocer","hairdresser"
                    ,"health_food","hifi","jewelry","model","mobile_phone","musical_instrument","optician"
                    ,"organic","paint","pasta","pastry","pawnbroker","perfumery","photo","seafood","shoes"
                    ,"spices","sports","supermarket","tea","tobacco","toys")

tourismofinterest <- c("artwork","gallery","hotel","museum")

#### entropy ####

london_amenities <- sf::st_read(dsn = here::here("london_amenities.geojson")) %>%
  st_set_crs(4326) %>% 
  st_transform(27700)
london_roads <- sf::st_read(dsn = here::here("london_roads.geojson")) %>%
  st_set_crs(4326) %>% 
  st_transform(27700)
amenities_isochrones <- sf::st_read(dsn = here::here("london_amenities_isochrones.geojson")) %>%
  st_set_crs(4326) %>% 
  st_transform(27700)

numCores <- 12

# time <- system.time({
#   entropy <- neighbors_entropy_par(london_amenities,r=500, cor_num = numCores)
# })
# time

time <- system.time({
  entropy_iso <- neighbors_entropy_iso(london_amenities,amenities_isochrones, cor_num = numCores)
})
time


london_amenities <- bind_cols(london_amenities, entropy_iso)

#### Plotting the entropy ####

london_entropy_map <- tm_shape(london_roads) + tm_lines(col = "black"
                                                        ,alpha = 0.6) + 
  tm_shape(london_amenities) + tm_dots(col = "entropy_iso"
                                      ,palette = "viridis"
                                      ,size = 0.02
                                      ,style = "fisher"
                                      ,title = "Entropy Score"
                                      ,id = "amenity"
                                      ,legend.hist = TRUE
                                      ,n=8
                                      ,legend.hist.title = "Distribution of values") + 
  tm_layout(main.title = "Amenities of London") + 
  tm_legend(legend.outside=T) + 
  tm_scale_bar()

london_size_map <- tm_shape(london_roads) + tm_lines(col = "black"
                                                        ,alpha = 0.6) + 
  tm_shape(london_amenities) + tm_dots(col = "size"
                                       ,palette = "viridis"
                                       ,size = 0.02
                                       ,style = "fisher"
                                       ,title = "Size of cluster"
                                       ,id = "amenity"
                                       ,legend.hist = TRUE
                                       ,n=8
                                       ,legend.hist.title = "Distribution of values") + 
  tm_layout(main.title = "Amenities of London") + 
  tm_legend(legend.outside=T) + 
  tm_scale_bar()




#westminster_entropy_map
sf::st_write(london_amenities,"amenities_entropy_iso.geojson",delete_dsn = TRUE,layer = "entropy")

tmap_save(london_entropy_map
          ,filename = "london_entropy.pdf"
          ,asp = 0
)

tmap_save(london_size_map
          ,filename = "london_size.pdf"
          ,asp = 0
)

tmap_save(london_entropy_map
          ,filename = "london_entropy.html"
          ,asp = 0
)
