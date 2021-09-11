#### Libs ####

library(sf)
library(tidyverse)
library(doParallel)
library(foreach)
library(tmap)
library(fastcluster)
library(stats)
library(rlist)

#### LOAD DATA ####

signals <- readr::read_csv(file = here::here("signals_clean.csv")
                           ,col_types = cols(
                             id = "c",
                             date = col_date(format = ""),
                             hour = "i",
                             X = col_double(),
                             Y = col_double(),
                             distance = col_double())
                           )

signals


signals_short <- readr::read_csv(file = here::here("signals_clean_short.csv")
                           ,col_types = cols(
                             id = "c",
                             date = col_date(format = ""),
                             hour = "i",
                             X = col_double(),
                             Y = col_double(),
                             distance = col_double())
)

signals_short


users <- sf::st_read(dsn = here::here("users.geojson")) %>%
  dplyr::select(userid,rlat,rlon) %>%
  st_set_crs(4326) %>%
  # transforming the users residential coordinates into 27700
  st_transform(27700) %>%
  # setting new columns with east nor coords from 27700
  mutate(r_x = st_coordinates(.)[,1]
         ,r_y = st_coordinates(.)[,2])

users$userid <- as.character(users$userid)

users
# 
# 
# london_hex <- sf::st_read(dsn = here::here("voronoi_hex_london.geojson")) %>%
#   st_set_crs(27700) #%>%
#   #st_transform(27700)
# 
# # adding an index variable for better readability
# london_hex$ind <- 1:nrow(london_hex)
# london_hex <- london_hex %>% select(ind,n,geometry)
# london_hex

# # tranforming into a sf object to easily switch coordinates
#signals <- signals %>%
#   # possibility to filter out the number of points.
#   # dplyr::group_by(id,hour,date) %>%
#   # dplyr::mutate(n = dplyr::n()) %>%
#   dplyr::filter(hour > 5 & hour < 23) %>%
#   # dplyr::ungroup() %>%
#   st_as_sf(coords = c("lon","lat"), crs = 4326,remove = TRUE) %>%
#   st_transform(27700) %>% 
#   mutate(X = round(st_coordinates(.)[,1])
#          ,Y = round(st_coordinates(.)[,2])) %>%
#   st_drop_geometry() %>% 
#   group_by(id,date,hour) %>%
#   distinct(X,Y,.keep_all = TRUE) %>% 
# st_as_sf(coords = c("X","Y"), crs = 27700,remove = FALSE) %>%
# ungroup()
# 
# #### Distance from home calculation ####
# 
# #computing the distance of each point to home.
# 
# signals$distance <- signals %>%
#   # dropping the geometry column
#   st_drop_geometry() %>%
#   # subsetting the only columns needed.
#   select(id,X,Y) %>%
#   # joining the signals coordinates and users coordinates by id.
#   #We get for each signal the residence of the user who emitted it.
#   left_join(users %>% st_drop_geometry(),by = c("id" = "userid")) %>%
#   # simple cartesian distance thanks to the projection.
#   # Very quick. No need to use the distance function.
#   mutate(distance = sqrt((X-r_x)^2+(Y-r_y)^2)) %>%
#   # pull function to retrieve only the column
#   dplyr::pull(distance)
# 
# print("distance computed")
# 
# signals %>%
#   st_drop_geometry() %>%
#   readr::write_csv("signals_clean.csv")
# 
# signals %>%
#   st_drop_geometry() %>%
#   dplyr::filter(distance < 3000) %>%
#   readr::write_csv("signals_clean_short.csv")

#### Saving the results. ####
# h <- 8.3
# w <- 11.3
# 
# pdf("histogram_distance.pdf",)
# hist(signals$distance
#      #,log
#      ,breaks = 100
#      ,xlab = "Distance (m)"
#      ,main = "Distance distribution"
#      ,freq = FALSE)
# dev.off()

##### Summary statistics ####

fraction <- nrow(signals_short)/nrow(signals)
fraction

#### Investigating the visitors of the hexagones ####

# london_hex <- signals %>%
#   st_as_sf(coords = c("X","Y"), crs = 27700,remove = FALSE) %>%
#   #dplyr::select(id,date,hour,X,Y, geometry) %>%
#   # primary tests with only one borough : Westminster.
#   # But including all the signals emitted by externals in the area.
#   # dplyr::filter(st_intersects(geometry, westminsterbb,sparse = FALSE)) %>%
#   sf::st_join(london_hex, left = TRUE) %>%
#   sf::st_drop_geometry() %>%
#   dplyr::group_by(ind) %>%
#   # dplyr::group_by(id,date,hour,ind) %>%
#   # dplyr::filter(dplyr::n() >= 1) %>%
#   # dplyr::ungroup(id,date,hour) %>%
#   dplyr::summarise(visits = dplyr::n_distinct(id), visitors = list(unique(id))) %>%
#   dplyr::right_join(london_hex, by = c("ind")) %>%
#   tidyr::replace_na(list(visits = 0)) %>%
#   st_sf()
# 
# print("signals distributed")
# 
# list.save(london_hex,"london_hex_short.rdata")

#### Second part, hierarchihcal clustering ####

#only when starting the code from here.

# london_hex <- list.load("voronoi_hex_short.rdata") %>%
#   st_set_crs(27700)

# westminsterbb <- sf::st_read(dsn = here::here("westminsterbb.geojson")) %>%
#   st_set_crs(27700) %>%
#   # st_centroid() %>%
#   sf::st_buffer(dist = 1000)
# 
# haringeybb <- sf::st_read(dsn = here::here("haringeybb.geojson")) %>%
#   st_set_crs(27700) %>%
#   # st_centroid() %>%
#   sf::st_buffer(dist = 1000)

#area_name <- "London"
# bb <- sf::st_read(dsn = here::here("east_londonbb.geojson")) %>%
#   st_set_crs(27700) %>%
  # st_centroid() %>%
  #sf::st_buffer(dist = 1000)


#### For selected areas of study, filter out the hexagones. ####

# london_hex <- london_hex %>%
#   filter(st_intersects(geometry, bb, sparse = FALSE))

#### building the distance matrix of hexagones ####

# london_hex_grouped_max <- london_hex %>% filter(visits >= 70)
# 
# numCores <- 12
# 
# system.time({
#   registerDoParallel(numCores)
#   sigma <- foreach(b=london_hex_grouped_max$visitors, .combine='rbind') %dopar% {
#     foreach(a=london_hex_grouped_max$visitors, .combine='c') %dopar% {
#       round(length(intersect(a,b))/length(b),4)
#     }
#   }
#   stopImplicitCluster()
# })
# print("sigma matrix computed")
# 
# #### hierarchical clustering ####
# system.time({
#   Hierar_cl <- fastcluster::hclust(stats::dist(sigma), method = "complete")
# })
# print("hierarchy built")
# 
# ## saving the hierarchy locally.
# 
# list.save(Hierar_cl, paste(area_name,'hclust.rdata',sep = "_"))
# 
# #### Subdivision into groups ####
# # define n in the range including the number of
# # neighbourhoods approximately that we are looking for.
# n <- 2000
# 
# #### computing the configuration in parralel.
# registerDoParallel(numCores)
# neighbourhoods_perc <- foreach(i = 2:n, .combine="rbind") %dopar% {
#   # transform to tibble to easily manipuilate
#   tibble(group = as.character(cutree(Hierar_cl, k = i))) %>%
#     # group and count the number of hex elements in each indivudual group
#     dplyr::count(group) %>%
#     # divide by the total number of hexes to normalize
#     dplyr::mutate(frac = n / sum(n)) %>%
#     # get the fraction of size of the biggest cluster and the entropy of the configuration.
#     dplyr::summarise(max = max(frac), entropy = -sum(frac*log(frac)))
# }
# stopImplicitCluster()
# 
# # number of estimated neighbourhoods about 500
# 
# pdf(paste(area_name,"_neighbourhoods_perc.pdf",sep = ""))
# par(mfrow=c(2,1), mar = c(4,4,0,4), oma = c(1,1,1,1), pty = "m")
# plot(2:n
#      ,neighbourhoods_perc$max
#      ,xlab = ""
#      ,ylab = "Size of biggest component"
#      ,main = paste(area_name,"neighbourhoods",sep = " "))
# #abline(v = n_neighb)
# plot(2:n,neighbourhoods_perc$entropy, xlab = "# clusters", ylab = "Entropy")
# #abline(v = n_neighb)
# dev.off()
# 
# readr::write_csv(neighbourhoods_perc
#                  ,paste(area_name,"_neighbourhoods_perc.pdf",sep = "")
#                  )

###


