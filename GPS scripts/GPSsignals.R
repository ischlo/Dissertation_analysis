#### Libs ####
library(sf)
library(tidyverse)
library(doParallel)
library(foreach)
library(tmap)
library(osmdata)
library(rgeos)

#### memory management ####

.libPaths()
gc()

#### SQL connection ####

# TO Manipulate the big data set of signals, a local PostgreSQL data base was set up
# to run tests. Suggested to add an index on the id variable as well to accelerate # significantly the queries execution. 

# Connection to Postgres
con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),    
                      host = "localhost",   
                      port = 5432,   
                      dbname = "DATA BASE NAME HERE",   
                      )

## finish a pipe with collect to get the data from the database
users <- tbl(con, "users_set") %>% 
  filter(visitor == "No") %>% collect () 

#### SIGNALS DATA SET MANIPULATIONS ####

columns_signals <- tbl(con, "signals_set") %>% colnames()

#coordinates <- tbl(con, "signals_set") %>% group_by(id,date)

signals_query <- tbl(con, "signals_set") %>% filter(id %in% residents_haringey)

signals_query %>% show_query()

signals_west_query <- tbl(con, "signals_set") %>% filter(id %in% users_west_set)

signals_west_query %>% show_query()
# collecting the signals emitted by residents of haringey in all of london. 
# with the right index on the data base it takes about x100 faster
time <- system.time({
  signals <- signals_query %>% collect()
})

time <- system.time({
  signals_west <- signals_west_query %>% collect()
})


# tranforming into a sf object to easily switch coordinates 
signals <- signals %>% 
  drop_na(lat,lon) %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326,remove = FALSE)

signals_west <- signals_west %>% 
  drop_na(lat,lon) %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326,remove = FALSE) %>% 
  st_transform(27700)

### Density of signals per user per hour. 

signals_per_hour_usr <-  signals %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(id,hour,date) %>% 
  dplyr::mutate(n = dplyr::n()) %>% 
  dplyr::filter(n > 15)

hist(signals_per_hour_usr$distance)

hist(signals$distance, breaks = 100, xlab = "Distance", main = "Distance distribution")

tmap_mode("plot")
qtm(filter(signals_west, distance < 100))

## Dealing with hexes. 
london_hex$ind <- 1:nrow(london_hex)

signals_hex_test <- signals[1:2000,] %>% 
  st_transform(4326) %>% 
  st_join(london_hex, left = TRUE)

signals_hex_test_grouped <- signals_hex_test %>%
  st_drop_geometry() %>% 
  group_by(id,ind) %>% 
  mutate(visit = dplyr::n(), av_dist = mean(distance))


###

#### Read in data #### 

# test reading signalsdata from the original csv. 

sig_test <- readr::read_csv("signals_clean.csv",n_max = 1000, col_types = cols(
                                                                                        id = "c",
                                                                                        #east = col_double(),
                                                                                        #nor = col_double(),
                                                                                        date = col_date(format = ""),
                                                                                        hour = "i"
))

# hexagone grid level 10 of london
london_hex <- sf::st_read("london_grid_uber_h3_hex_10lvl.geojson")

# same grid but only for haringey with already merged entropy data in python. 
haringey_hex <- sf::st_read("haringey_hex.geojson")

#  signals emitted by residents of haringey within haringey
signals_hex <- sf::st_read("signals_hex.geojson",int64_as_string = TRUE,stringsAsFactors = FALSE)

####Getting bbs ####

londonbb <- getbb("London, United Kingdom", format_out = "sf_polygon") %>% 
  st_set_crs(4326) %>% st_transform(27700)

haringeybb <- getbb("Haringey, London, UK", format_out = "sf_polygon") %>% 
  st_set_crs(4326) %>% st_transform(27700) %>% st_buffer(dist = 3000)

westminsterbb <- getbb("Westminster city, London, UK", format_out = "sf_polygon") %>% 
  st_set_crs(4326) %>% st_transform(27700) %>% st_buffer(dist = 3000)

tower_hamlets <- getbb("Tower Hamlets, London, UK", format_out = "sf_polygon") %>% 
  st_set_crs(4326) %>% st_transform(27700) %>% st_buffer(dist = 3000)


sf::st_write(westminsterbb,"westminsterbb.geojson")

#### filtering & cleaning data ####

users <- users %>% 
  drop_na(rlat,rlon) %>%
  st_as_sf(coords = c("rlon","rlat"), crs = 4326, remove = FALSE) %>% 
  st_transform(27700) %>% 
  filter(st_intersects(geometry, londonbb[1,], sparse = FALSE))

users <- users %>% st_transform(4326)

#### VORONOI OF RESIDENTIAL LOCATIONS ####

users_voronoi <- users_count %>% 
  sf::st_geometry() %>% 
  sf::st_union() %>% 
  sf::st_voronoi(envelope = londonbb[1,] %>% 
                   st_geometry() %>% 
                   st_sfc()) %>%
  st_collection_extract(type = "POLYGON") %>% # a list of polygons
  st_sf() %>% # from list to sf object
  st_intersection(londonbb[1,]) %>% # cut to shape of NC state
  st_join(users_count) # put names back



tmap_mode("view")
users_voronoi[1020,] %>% st_area()


users_voronoi %>% st_write("voronoi_hex_london.geojson")

users_count <- users %>% 
  st_drop_geometry() %>% 
  dplyr::group_by(rlat,rlon) %>% 
  dplyr::count() %>% 
  st_as_sf(coords = c("rlon","rlat"), remove = FALSE, crs = 4326) %>% 
  st_transform(27700)

users_count
# 
# samp <- sample_n(users_haringey, 10) 
# 
# samp$ind <- 1:10
# 
# signals_samp <- filter(signals,id %in% samp$userid)
# 
# samp$userid <- NULL
# 
# signals_samp <- samp %>% 
#   st_drop_geometry() %>%
#   select(userid,ind) %>% 
#   right_join(signals_samp, by = c("userid"= "id")) %>% st_sf()
# signals_samp <- st_sf(signals_samp)
# 
# signals_samp$userid <- NULL
# 
# readr::write_csv(samp %>% st_drop_geometry(),"samp.csv")
# readr::write_csv(signals_samp %>% st_drop_geometry(),"signals_samp.csv")
# 
# sf::st_write(samp, "samp.geojson")
# sf::st_write(signals_samp, "signals_samp.geojson")

# subset of only the users living in haringey

users_haringey <- users %>% st_transform(27700) %>%
  filter(st_intersects(geometry, haringeybb, sparse = FALSE))

users_haringey_set <- users_haringey$userid

summary(users_haringey_signals %in% users_haringey_set)

length(users_haringey_signals)
length(users_haringey_set)

residents_haringey <- users_haringey_set[users_haringey_set %in% users_haringey_signals]

signals_hex <- signals_hex[signals_hex$id %in% residents_haringey,]

# map of users residential location 

user_density_map <- tm_shape(londonbb) + tm_polygons(col = "grey"
                                                     ,alpha = 0.5
                                                     ) +
  tm_shape(users_count) + tm_dots(col = "n"
                                  ,palette = "plasma"
                                  ,title = "User density"
                                  ,size = 0.04
                                  ,n = 10
                                ) + tm_scale_bar() +
  tm_layout(legend.outside = TRUE
            ,main.title = "User's residence distribution")
user_density_map

tmap_save(user_density_map,"user_density_2.pdf"
          # ,height = h
          # ,width = w
          #,asp = 0
          )

users_westminster <- users %>% filter(st_intersects(.,westminsterbb, sparse = FALSE))

users_west_set <- users_westminster$userid


# getting the number of signals by date, hour and user to filter out the low emitter of signals
signals_grouped <- signals_hex %>% group_by(id,date,hour) %>% mutate(count = dplyr::n())

# setting the threshold to 20 for signals per hour per user
signals_filtered <- filter(signals_grouped, count > 20) %>% ungroup()

signals_filtered

qtm(signals_filtered[1:1000,],fill = "id" ,symbols.col = "id")

# grouping by number of signals emitted by a user in each hexagone. 
user_hex <- signals_filtered %>% 
  st_drop_geometry() %>% 
  dplyr::group_by(index,id) %>% 
  dplyr::count() %>% 
  dplyr::ungroup()

# setting an index for users that is easier to use.
user_hex$int <- as.integer(as.factor(user_hex$id))

hex_grouped <- user_hex %>% 
  dplyr::group_by(index) %>% summarise(usr = list(id))

sigma <- matrix(0,nrow = length(hex_grouped$index), ncol = length(hex_grouped$index))

length(intersect(hex_grouped$usr[1][[1]] , hex_grouped$usr[10][[1]]))/length(hex_grouped$usr[1][[1]])

user_overlap_matrix <- matrix(0, nrow = length(users_haringey), ncol = length(users_haringey))
#user_overlap_matrix <- NULL

# similarity of hexagones 

# idea : for 2 hexagones, group all their unique users, and see the size of the intersection. 
# hex1$usr in hex2$usr / union_size(hex$usr1)

numCores <- 4
registerDoParallel(numCores)
sigma <- foreach(b=hex_grouped$usr, .combine='rbind') %:%
  foreach(a=hex_grouped$usr, .combine='c') %dopar% {
    length(intersect(a,b))/length(b)
  }
stopImplicitCluster()

gc()


# map of entropy

tmap_mode("view")

tm_shape(haringey_hex) + tm_polygons(col = "entropy"
                                     ,palette = "plasma"
                                     ,alpha = 0.5)
