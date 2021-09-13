#### LIBS ####
library(mapboxapi)
library(sf)
library(tidyverse)
library(profvis)
library(tmap)
library(tmaptools)


#### Isochrones turnpike lane ####

SevenSistersCoord <- filter(haringeyStations, name == "Seven Sisters") %>% st_coordinates() %>% c()

#MB_token <- "pk.eyJ1IjoiaXNjaGxvIiwiYSI6ImNrYWduNGFpdDAwYWkydG1vMWR1NmMyaDMifQ.MTroT-sAYvk6EJIQz98jkQ"

isochronesTurnpikeLane <- mb_isochrone(c(-0.10282,51.59010)
                                       ,time = c(5, 10, 20)
                                       ,access_token = MB_token
                                       ,profile = "walking")
isochromeSevenSisters <- mb_isochrone(SevenSistersCoord
                                      ,time = c(5, 10, 20)
                                      ,access_token = MB_token
                                      ,profile = "walking")

isochroneTurnpike <- tm_shape(haringayStreets) + tm_lines(col = "grey", lwd = 2) + 
  tm_shape(haringeyAmenities[which(haringeyAmenities$amenity %in% amenityofinterest),]) + 
  tm_dots(col = "amenity", size = .2, legend.show = F) +
  tm_shape(isochronesTurnpikeLane) + tm_fill(col = "time"
                                             ,alpha = 0.5
                                             ,title = "Walkable area (min)"
  ) + tm_layout(legend.outside = T) +
  tm_shape(isochromeSevenSisters) + tm_fill(col = "time"
                                            ,alpha = 0.5
                                            ,legend.show = F
  ) +
  tm_shape(haringeyStations) + tm_dots(col = "black"
                                       ,size = 0.1) + tm_scale_bar()

isochroneTurnpike


# how to create empty data frame of sF objects : https://github.com/r-spatial/sf/issues/354 
nrows <- nrow(london_stations)
isochrones_test <- st_sf(t = 1:(3*nrows), geometry = st_sfc(lapply(1:nrows, function(x) st_geometrycollection()),crs = 4326))
isochrones_test[4:6,]
for (i in c(1:nrows)) {
  coord <- london_stations[i,] %>% st_coordinates() %>% c() 
  isochrones_test[(3*i-2):(3*i),] <- mb_isochrone(coord
                                           ,time = c(5, 10, 15)
                                           ,access_token = MB_token
                                           ,profile = "walking")
}


filter(isochrones_test,is.empty(geometry))

view(isochrones_test)

tm_shape(isochrones_test) + tm_polygons(col = "t"
                                        ,palette = "-viridis"
                                        ,alpha = 0.5) + 
  tm_shape(london_stations) + tm_dots(col = "red")

pal <- viridisLite::viridis(3)

nrows15 <- nrow(filter(isochrones_test, t == 15))

coverage15min <- filter(isochrones_test, t == 15) %>% sf::st_union()

coverage10min <- filter(isochrones_test, t == 10) %>% sf::st_union()

coverage5min <- filter(isochrones_test, t == 5) %>% sf::st_union()

coverage_london <- tm_shape(london_sf) + tm_polygons(alpha = 0.3) +  
  tm_shape(coverage15min) + tm_polygons(col = pal[1]
                                        ,alpha = 1) +
  tm_shape(coverage10min) + tm_polygons(col = pal[2]
                                        ,alpha = 1) +
  tm_shape(coverage5min) + tm_polygons(col = pal[3]
                                       ,alpha = 1) + 
  tm_layout(main.title = "Walking time to transport station") +
  tm_basemap("OpenStreetMap")

tmap_mode("plot")

coverage_london

qtm(london_sf)




#### isochrones london ####

profvis::profvis({
  london_amenities_isochrones <- mb_isochrone(london_entropy
                                              ,time = c(7)
                                              ,access_token = MB_token
                                              ,profile = "walking"
                                              ,id_column = "osm_id"
                                              )
})

tmap_mode("view")
london_amenities_isochrones[1:400,] %>% qtm(fill = "rosybrown")

#### Verifying the validity of isochrones from an Sf point of view. We want polygons. 

isochones_validity <- st_is_valid(valid_isochrones,NA_on_exception = TRUE,reason = TRUE)

valid_isochrones <- st_make_valid(london_amenities_isochrones)

isochones_validity[which(isochones_validity != "Valid Geometry")]

london_amenities_isochrones <- valid_isochrones

london_amenities_isochrones$geometry <- london_amenities_isochrones %>% st_geometry() %>% st_cast("POLYGON")

# still wrong geometries. 
london_amenities_isochrones <- london_amenities_isochrones %>% tibble() %>% st_sf()

sf::st_write(london_amenities_isochrones, "london_amenities_isochrones.geojson",delete_dsn=TRUE, delete_layer = TRUE)

# 
# mb_isochrone(
#   london_entropy,
#   profile = "walking",
#   time = c(5),
#   access_token = ,
#   denoise = 1,
#   geometry = "polygon",
#   output = "sf",
#   rate_limit = 300,
#   keep_color_cols = FALSE,
#   id_column = NULL
# )

# function to compute entropy using isochrones. 
# not used because the isochrones were computed separately. 
neighbors_entropy_iso <- function(d, t = 5, cor_num = 1){
  #function that computes entropy based on isochrones of amenities. 
  # pass data here that is in CRS 27700
  MB_token <- "pk.eyJ1IjoiaXNjaGxvIiwiYSI6ImNrYWduNGFpdDAwYWkydG1vMWR1NmMyaDMifQ.MTroT-sAYvk6EJIQz98jkQ"
  isochrone <- mb_isochrone(d
                            ,time = c(t)
                            ,access_token = MB_token
                            ,profile = "walking"
                            ,id_column = "osm_id"
  )
  #pts_buf <- sf::st_buffer(d, r)
  int <- sf::st_intersects(test_isochrone, d)
  registerDoParallel(cor_num)
  entropy <- foreach(i = 1:nrow(d), .combine=c) %dopar% {
    counted <- d[int[[i]],] %>% dplyr::group_by(amenity) %>% dplyr::count()
    p <- counted$n/sum(counted$n)
    e <- c(-sum(p*log(p)),sum(counted$n))
  }
  stopImplicitCluster()
  data.frame(matrix(entropy, ncol = 2, byrow = TRUE)) %>% rename("entropy" = "X1", "size" = "X2")
}


