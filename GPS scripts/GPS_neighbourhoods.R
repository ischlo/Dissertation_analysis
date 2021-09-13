#### Libs ####

library(sf)
library(tidyverse)
library(doParallel)
library(foreach)
library(tmap)
library(corrplot)
library(fastcluster)
library(pracma)
library(rjson)
require(rlist)
library(igraph)
library(osmdata)
library(factoextra)
library(cluster)


#### Read in the data ####

westminsterbb <- get_neighbourhood("Paddington") %>% 
  st_geometry() %>%
  st_sf() %>%
  st_set_crs(4326) %>%
  st_transform(27700) %>%
  st_buffer(dist = 5000)

westminsterbb %>% qtm(fill.alpha = 0.3)

# hexagone grid level 10 of london
london_hex <- sf::st_read("london_grid_uber_h3_hex_10lvl.geojson") %>% 
  st_transform(27700)
# adding an index variable for better readability
london_hex$ind <- 1:nrow(london_hex)

entropy <-sf::st_read("../amenities_entropy_iso.geojson") %>% 
  st_set_crs(27700)
entropy

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
  filter(sf::st_intersects(geometry, westminsterbb, sparse = FALSE))

tmap_mode("view")

east_londonbb <- get_neighbourhood("Upton park") %>% 
  st_geometry() %>%
  st_sf() %>%
  st_set_crs(4326) %>%
  st_transform(27700) %>%
  st_buffer(dist = 4500)

east_londonbb %>% qtm(fill.alpha = .3)

east_londonbb %>% sf::st_write("east_londonbb.geojson")

#
#### memory management ####
memory.limit()
R.version
.libPaths()
gc()

max.categories()

# heavy stuff is erased once saved to clear up RAM

entropy_hex_map <- NULL

london_visits_map <- NULL

signals_loc <- NULL

signals_coordinates <- NULL

signals_hex <- NULL

london_hex_test <-  NULL

signals_west_hex <- NULL

signals_west_filtered

london_entropy_short <- NULL

signals <- NULL

signals_west_filtered

visitors_entropy_plot <- NULL

london_hex_RC <- NULL

signals_distance <- NULL

signals_filtered <- NULL

signals_west <- NULL

london_hex_short <- NULL
signals_west_filtered <- NULL

signals_1 <- NULL

#### Functions ####

get_neighbourhood <- function(x, location = "London, UK") {
  x %>% 
    paste(location, sep = ", ") %>%
    osmdata::getbb() %>% 
    data.frame() %>% 
    dplyr::mutate(pos = (min + max)/2) %>%
    dplyr::select(pos) %>%
    t() %>% 
    c() %>% 
    st_point()
}
#
#### analyzing the results ####

distnace_summary <- signals_hex %>% 
  st_drop_geometry() %>% 
  select(distance) %>% 
  round(-2) %>% '/'(1000) %>% 
  dplyr::count(distance) %>%
  mutate(dens = n/sum(n)) %>%
  filter(distance > 0) 

signals_summary_fit <- lm(log(n) ~ distance,data = distnace_summary)

signals_summary_fit$coefficients

plot(distnace_summary$distance
     ,distnace_summary$n
     ,log="xy"
     #,lend=2
     )
points(distnace_summary$distance
       ,exp(signals_summary_fit$fitted.values)
       ,col = "darkred"
       )




# 
# 
# south_londonbb %>% st_buffer(dist = 3000) %>% qtm(fill.alpha = 0.3)
# 
# south_london_hex_short <- london_hex_short %>% 
#   filter(st_intersects(geometry, south_londonbb %>% 
#                          st_buffer(dist = 3000),sparse = FALSE))
# 
# south_london_hex_short <- NULL


#### Results for westminster ####

Hierar_cl_RC <- list.load("Westminster_hierarchy_rc/Hierar_cl.rdata")

westminster_neighbourhood <- london_hex_short %>% filter(st_intersects(geometry
                                          ,westminsterbb %>% st_buffer(dist = 1000)
                                          ,sparse = FALSE)
                            & visits >=1)



# percolation on the number of different clusters. from 2 to 50. 
n <- 150
# Cut tree into n groups
registerDoParallel(numCores)
sub_grps <- foreach(i = 2:n, .combine="rbind") %dopar% {
  tibble(group = as.character(cutree(Hierar_cl_RC, k = i))) %>% 
    dplyr::count(group) %>% 
    mutate(frac = n / sum(n)) %>% 
    summarise(max = max(frac), entropy = -sum(frac*log(frac)))
}
stopImplicitCluster()

n_RC <- 93

par(mfrow=c(2,1), mar = c(4,4,1,4), oma = c(1,1,1,1), pty = "m")
plot(1:(n-1),sub_grps$max, xlab = "",ylab = "Size of biggest component", main = "Westminster")
abline(v = n_RC)
plot(1:(n-1),sub_grps$entropy, xlab = "# clusters", ylab = "Entropy")
abline(v = n_RC)

par(mfrow=c(1,1))

westminster_neighbourhood_grouped <- westminster_neighbourhood %>% 
  mutate(groups = cutree(Hierar_cl_RC, k = n_RC)) %>% 
  group_by(groups) %>% 
  filter(n()>10) %>%
  summarise(geometry = st_union(geometry,by_feature = FALSE)) %>% 
  st_intersection(westminsterbb)

westminster_neighbourhood_map <- 
tm_shape(westminster_neighbourhood_grouped) + tm_polygons(col = "MAP_COLORS"
                                                          #,style = "cat"
                                                          ,palette = "Set1"
                                                          ,alpha = .7)+
  tm_shape(westminster_roads) + tm_lines(col = "black") +
  tm_scale_bar()

#tmap_mode("view")
westminster_neighbourhood_map



westminster_neighbourhood_grouped <- westminster_neighbourhood %>% 
  mutate(groups = cutree(Hierar_cl_RC, k = n_RC)) %>% 
  group_by(groups) %>% 
  filter(n()>10) %>%
  summarise(geometry = st_union(geometry,by_feature = FALSE))

westminster_neighbourhood_map <- tm_shape()
  tm_shape(westminster_neighbourhood_grouped) + tm_polygons(col = "MAP_COLORS"
                                                            #,style = "cat"
                                                            ,palette = "Set1"
                                                            ,alpha = .7)+
  tm_scale_bar()

#tmap_mode("view")
westminster_neighbourhood_map

tmap_mode("plot")

signals_short_main %>% filter(distance < 100 & st_intersects(geometry
                                                             ,westminsterbb
                                                             ,sparse = FALSE)) %>% 
  qtm() 

tmap_mode("view")

#
#### Buffers around neighbourhoods ####

marylebone <- get_neighbourhood("Marylebone") %>% 
  st_geometry() %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) %>%
  st_buffer(dist = 4500)
  
marylebone %>% qtm(fill.alpha = 0.3)

westminsterbb %>%
  st_centroid() %>%
  sf::st_buffer(dist = 4500) %>%
  qtm(fill.alpha = 0.3)

#### Grouping the entropy value by hexagon with st_join ####

tmap_mode("plot")

london_hex_short$entropy <- NULL

entropy_hex <- st_join(entropy,london_hex_short) %>% 
  drop_na(ind) %>% 
  st_drop_geometry() %>% 
  group_by(ind) %>% 
  summarise(entropy = mean(entropy))

entropy_hex$entropy %>% hist()

london_hex_short <- london_hex_short %>% 
  dplyr::left_join(entropy_hex,by = c("ind" = "ind")) %>% 
  tidyr::replace_na(list(entropy = -1))


entropy_hex_map <- tm_shape(london_hex_short) + tm_polygons(col = "entropy"
                                                      ,alpha = 1
                                                      ,palette = "viridis"
                                                      ,border.alpha = 0
                                                      ) + 
  tm_layout(title = "Entropy score") +
  tm_scale_bar()

entropy_hex_map

tmap_save(entropy_hex_map
          ,"entropy_hex_map.pdf"
          ,height = h
          ,width = w
          )
#
#### Grouping the signals by hexagon. ####

# haringey case. 

profvis::profvis({
  london_hex_haringey <- signals_hex %>%
    st_drop_geometry() %>%
    filter(distance < 3000) %>%
    dplyr::group_by(id,date,hour,ind) %>% 
    # the smaller the number here, the more detailed and big will be the data 
    # keeping it relatively small is good for the results
    dplyr::filter(n() >= 1) %>%
    dplyr::ungroup(id,date,hour) %>%
    summarise(visits = n_distinct(id), visitors = list(unique(id))) %>%
    dplyr::right_join(london_hex, by = c("ind")) %>%
    replace_na(list(visits = 0)) %>% 
    st_sf() %>% 
    filter((visits > 0) & (st_intersects(geometry,haringeybb,sparse = FALSE)))
    
})

# keep as small as possible to the limit of computer power.
london_hex_haringey_max <- london_hex_haringey %>% filter(visits > 30)

london_hex_haringey_max %>% nrow()

profvis::profvis({
  numCores <- 3
  registerDoParallel(numCores)
  sigma_haringey <- foreach(b=london_hex_haringey_max$visitors, .combine='rbind') %:%
    foreach(a=london_hex_haringey_max$visitors, .combine='c') %dopar% {
      round(length(intersect(a,b))/length(b),4)
    }
  stopImplicitCluster()
})

#### Parallelizing only the external loop
### CRAZY FASTER AND LESS MEMORY CONSUMING
# speed: approx x100
# memory: approx x200

profvis::profvis({
  numCores <- 3
  registerDoParallel(numCores)
  sigma_haringey_outerpar <- foreach(b=london_hex_haringey_max$visitors, .combine='rbind') %dopar% {
    foreach(a=london_hex_haringey_max$visitors, .combine='c') %do% {
      round(length(intersect(a,b))/length(b),4)
    }
  }
  stopImplicitCluster()
})

identical(sigma_haringey,sigma_haringey_outerpar)

# optionnaly round values for memory gain
sigma_haringey <- round(sigma_haringey,4)

#### Hierarchichal clustering ####
profvis::profvis({
  Hierar_cl_haringey <- fastcluster::hclust(dist(sigma_haringey), method = "complete")
})
# 

# percolation on the number of different clusters. from 2 to 50. 
n <- 50
# Cut tree into n groups
sub_grps <- foreach(i = 2:n, .combine="rbind") %do% {
  tibble(group = as.character(cutree(Hierar_cl_haringey, k = i))) %>% 
    dplyr::count(group) %>% 
    mutate(frac = n / sum(n)) %>% 
    summarise(max = max(frac), entropy = -sum(frac*log(frac)))
}

n_neighb <- 5
par(mfrow=c(2,1), mar = c(4,4,0,4), oma = c(1,1,1,1), pty = "m")
plot(1:(n-1),sub_grps$max, xlab = "",ylab = "Size of biggest component")
abline(v = n_neighb)
plot(1:(n-1),sub_grps$entropy, xlab = "# clusters", ylab = "Entropy")
abline(v = n_neighb)

par(mfrow=c(1,1))

# Plotting dendrogram
plot(Hierar_cl_haringey, cex = 0.6, hang = -1)
rect.hclust(Hierar_cl_haringey, k = n_neighb, border = 2:4)

haringey_hex_grouped <- london_hex_haringey_max %>% 
  mutate(groups = cutree(Hierar_cl_haringey, k = n_neighb)) 


#tmap_mode("view")

haringey_neighbourhoods <- tm_shape(haringey_hex_grouped
                                       ,bbox = haringeybb) + tm_polygons(col = "groups"
                                                                            ,style = "cat"
                                                                            ,palette = "Set1"
                                                                            ,alpha = .5)+
  tm_scale_bar()

haringey_neighbourhoods

# 
haringey_hex_grouped %>%
  filter(groups ==1) %>%
  pull(visitors) %>%
  unlist() %>%
  unique() %>%
  length()
#

# test reading sf tibble with rlist: WORKS !
# list.save(london_hex,"london_hex.rdata")
#london_hex <- list.load("london_hex.rdata")

## WESTMINSTER

profvis::profvis({
  london_hex_west <- signals_west_filtered %>%
    st_drop_geometry() %>%
    filter(distance < 3000) %>%
    # dplyr::group_by(id,date,hour,ind) %>% 
    # dplyr::filter(n() >= 1) %>%
    dplyr::ungroup(ind) %>%
    summarise(visits = n_distinct(id), visitors = list(unique(id))) %>%
    dplyr::right_join(london_hex, by = c("ind")) %>%
    replace_na(list(visits = 0)) %>% 
    st_sf() %>% 
    filter((visits > 0) & (st_intersects(geometry,westminsterbb,sparse = FALSE)))
})

signals_west_filtered %>% filter(distance < 100) %>% qtm()

london_hex_west

london_visits_map <- tm_shape(london_hex) + tm_polygons(col = "visits"
                                                        ,alpha = 1
                                                        ,palette = "viridis"
                                                        ,style = "fisher"
                                                        ,border.alpha = 0
                                                        ,n = 10
                                                        ) +
  tm_layout(title = "Visits score local") +
  tm_scale_bar()

london_visits_map

westminsterbb <- sf::st_read("westminsterbb.geojson")

london_hex_grouped_max <- london_hex_west %>% 
  filter(visits > 25)

london_hex_west %>% 
  filter(visits > 0) %>% 
  qtm(fill = "visits")

london_hex_west %>% filter(visits > 1) %>% pull(visits) %>% hist()

#### Overlap of visited hexs ####

# some reading on clusters in R
# https://bio723-class.github.io/Bio723-book/clustering-in-r.html 

# sigma is the similarity matrix of each hexagone in terms of the users that visited them.

# the sigma computed through the dice similarity
sigma_DSC 

profvis::profvis({
  numCores <- 3
  registerDoParallel(numCores)
  sigma <- foreach(b=london_hex_grouped_max$visitors, .combine='rbind') %:%
    foreach(a=london_hex_grouped_max$visitors, .combine='c') %dopar% {
      length(intersect(a,b))/length(b)
    }
  stopImplicitCluster()
})


# to get the distance matrix, 

# optionnaly round values for memory gain
sigma <- round(sigma,4)

#### Hierarchichal clustering ####
profvis::profvis({
  Hierar_cl <- fastcluster::hclust(dist(sigma), method = "complete")
})
# 

# percolation on the number of different clusters. from 2 to 50. 
n <- 50
# Cut tree into n groups
sub_grps <- foreach(i = 2:n, .combine="rbind") %do% {
  tibble(group = as.character(cutree(Hierar_cl, k = i))) %>% 
    dplyr::count(group) %>% 
    mutate(frac = n / sum(n)) %>% 
    summarise(max = max(frac), entropy = -sum(frac*log(frac)))
}

n_neighb <- 9
par(mfrow=c(2,1), mar = c(4,4,0,4), oma = c(1,1,1,1), pty = "m")
plot(1:(n-1),sub_grps$max, xlab = "",ylab = "Size of biggest component")
abline(v = n_neighb)
plot(1:(n-1),sub_grps$entropy, xlab = "# clusters", ylab = "Entropy")
abline(v = n_neighb)

par(mfrow=c(1,1))

# Plotting dendrogram
plot(Hierar_cl, cex = 0.6, hang = -1)
rect.hclust(Hierar_cl, k = n_neighb, border = 2:4)

london_hex_grouped <- london_hex_grouped_max %>% 
  mutate(groups = cutree(Hierar_cl, k = n_neighb)) 

#
#tmap_mode("view")

westminster_neighbourhoods <- tm_shape(london_hex_grouped
                                       ,bbox = westminsterbb) + tm_polygons(col = "groups"
                                                                            ,style = "cat"
                                                                            ,palette = "Set1"
                                                                            ,alpha = 1)+
  tm_scale_bar()

westminster_neighbourhoods

#
london_hex_grouped %>%
  filter(groups !=1) %>%
  pull(visitors) %>%
  unlist() %>%
  unique() %>%
  length()
#


# plot of the correlation matrix. 
# 
correlation_plot_westminster <- corrplot(sigma, method = "color"
         ,diag = FALSE
         ,col.lim = c(0,1)
         ,order = "hclust"
         ,hclust.method = "complete")

correlation_plot_westminster
#### comparison with the entropy neighbourhoods ####

tmap_mode("view")

tm_shape(london_hex_short %>% filter(st_intersects(geometry,westminsterbb,sparse = FALSE))
         ) + tm_polygons(col = "grey"
                         ,alpha = 0.5)

westminster_iso_hull <-  sf::st_read("../westminster_iso_hull.geojson") %>%
  st_set_crs(27700)

london_visits_neighb <- london_hex_grouped_max %>%
  group_by(groups) %>% 
  summarise(area = st_union(geometry),by_feature = TRUE)

tm_shape(london_visits_neighb) + tm_polygons(col = "groups"
                                             ,palette = "Set1"
                                             ,style = "cat") +
  tm_scale_bar()

#haringeybb %>% sf::st_write("haringeybb.geojson")


tmap_mode("plot")

tm_shape(london_hex_short) + tm_polygons(col = "visits"
                                         ,style = "log10"
                                         ,palette = "viridis"
                                         ,title = "Visitors"
                                         ,border.alpha = 0) +
  tm_layout(main.title = "Visits"
            ,legend.text.size = 0.9
            ,main.title.size = 1.3
            ,legend.title.size = 1.1
            ,legend.position = c("left","bottom"))
  

tm_shape(london_hex_short) + tm_polygons(col = "entropy"
                                         ,style = "pretty"
                                         ,palette = "viridis"
                                         ,title = "Entropy"
                                         ,interval.closure = "left"
                                         ,legend.width = 0.5
                                         ,legend.height = 1.2
                                         ,border.alpha = 0
                                         ) + 
  tm_layout(main.title = "Entropy"
            ,legend.text.size = 0.9
            ,main.title.size = 1.3
            ,legend.title.size = 1.1
            ,legend.position = c("left","bottom")
            )



entropy$cluss_iso <- neighbourhood_iso$cluss_iso

neighb <-  entropy %>% st_join(london_neighbourhood_grouped, left = TRUE) %>% 
  drop_na(groups)

# tm_shape(london_neighbourhood_grouped) + tm_polygons(col = "grey") +
#   tm_shape(neighb) + tm_dots(col = "groups"
#                              ,style = "pretty")
# 
# neighb %>% filter(is.na(groups) & st_intersects(geometry,londonbb,sparse = FALSE)) %>% nrow()

tmap_mode("view")


tm_shape(neighb) + tm_dots(col = "cluss_iso"
                           )

neighb$somme <- neighb %>% 
  st_drop_geometry() %>%
  select(groups,cluss_iso) %>%
  group_by(groups,cluss_iso) %>%
  mutate(n = n()) %>% 
  ungroup(cluss_iso) %>% 
  mutate(n_gr = n(), var1 = n/n_gr) %>% 
  group_by(cluss_iso) %>%
  ungroup(groups) %>% 
  mutate(n_indiv = n(),var2 = n/n_indiv) %>% 
  mutate(somme = (var1 + var2)/2) %>% 
  pull(somme)


neighb$dsc <- neighb %>% 
  st_drop_geometry() %>%
  select(groups,cluss_iso) %>%
  group_by(groups,cluss_iso) %>%
  mutate(n = n()) %>% 
  ungroup(cluss_iso) %>% 
  mutate(n_gr = n()) %>% 
  group_by(cluss_iso) %>%
  ungroup(groups) %>% 
  mutate(n_indiv = n()) %>% 
  mutate(dsc = 2*n/(n_indiv + n_gr)) %>% 
  pull(dsc)


tm_shape(london_neighbourhood_grouped) + tm_polygons(col = "grey"
                                                     ,alpha = 0.5) +
  tm_shape(neighbourhoods_iso_hull) + tm_polygons(col = "grey"
                                                  ,alpha = 0.5) +
  tm_shape(neighb %>% filter(dsc > .6)) + tm_dots(col = "groups"
                                                   ,palette= c("rosybrown","linen","navyblue","lighblue","green")
                                                   )


neighb %>% 
  filter(somme > .6) %>% 
  pull(groups) %>% 
  unique() %>% 
  length() %>% '/'(380)

neighb$cluss_iso %>% unique() %>% length()

neighb %>% st_drop_geometry() %>%
  group_by(groups) %>%
  summarise(max = max(somme)) %>%
  pull(max) %>% 
  hist()
  median()

neighb %>% arrange(desc(somme))

neighb %>% filter(groups == 103) %>% qtm()
