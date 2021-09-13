#### LIBS ####

library(sf) #TO READ SHAPEFILES 
library(sp)
library(rgeos)
library(tidyverse) #TO MANIPULATE CSV FILES 
library(iterators)
library(tmap)
library(osmdata) # to load data from OSM
library(foreach) # for parallel computing
library(doParallel)
library(spatstat)
library(igraph)
library(mapboxapi)
library(osrm)
library(xtable)
library(pryr)

source("functions.R")

#### LOADING THE DATA ####

# Creating a bbox 

westminsterbb <- getbb("City of Westminster, London")


# loading data from OSM

westminster_amenities <- getOSMdata(westminsterbb, "amenity") %>% osm_group_points(.,k = "amenity", amenityofinterest)

k <- "shop"

westminster_shops <- getOSMdata(westminsterbb, "shop") %>% osm_group_points(.,k = "shop", shopofinterest) 

westminster_shops <- westminster_shops %>% rename("amenity" = k)

westminster_amenities <- rbind(westminster_amenities,westminster_shops)

haringey_roads <- getOSMdata(Haringeybb, k = "highway", val = main_roads) %>% osm_poly2line() %>% .$osm_lines

haringey_roads <- haringey_roads %>% .[c("osm_id","name","highway","geometry")] %>% st_set_crs(4326)

#### test loading only the tags of interest ####

# speed test loading everything for that key and them filtering those of interest
# ITS QUICKER !!
system.time(
tourism_all <- getOSMdata(westminsterbb, k="tourism", val = tourismofinterest) %>% osm_group_points(.,k="tourism",kofinterest = tourismofinterest)
)

westminster_amenities <- rbind(westminster_amenities,tourism_all)

# save a file called westminster_amenities with the containing no null values of points and polygons of the OSM tags from above.
#The polygons are simplified by taking the centre point. 
sf::st_write(westminster_amenities, "westminster_amenities.geojson", append = FALSE, delete_dsn = TRUE)

westminster_entropy <- sf::st_read("westminster_amenities_out.geojson", crs = 27700)

duplicated(westminster_amenities$osm_id)

haringey_amenities <- haringey_amenities[!duplicated(haringey_amenities$osm_id),]

####Clustering the amenities into neighbourhoods ####

## HARINGEY 
## Neighbourhood with buffers performance test
profvis::profvis(
  {
    max <- haringey_amenities %>% st_transform(27700) %>% neighbourhoods_buff(r = 500,cores= 3)
  }
)

haringey_amenities$clus_buff <- max

mem_used()
gc()

unique(max)

max <- NULL

## WESTMINSTER

#west_test <- westminster_entropy[1:200,]
westminster_entropy$osm_id <- as.integer(westminster_entropy$osm_id)
westminster_entropy$freq <- NULL
westminster_entropy$clus <- NULL

max_buff <- list()
time <- list()

for (k in 1:3) {
  time[[k]] <- system.time({
    max_buff[k] <- westminster_entropy %>% st_transform(27700) %>% neighbourhood_buff(r = 100*i,cores = 4)
    })
}

max_buff

westminster_entropy$clus_buff <- as.character(max_buff)

tm_shape(westminster_roads_main) + tm_lines(col = "black"
                                            ,alpha = 0.7) +
  tm_shape(westminster_entropy) + tm_dots(col = "clusnum"
                                        ,size = 0.1
                                        ,palette = "Set1"
                                        ,title = "neighbourhoods from buffers") + 
  tm_layout(main.title="Neighbourhoods of Westminster"
            ,legend.outside = TRUE)

test_clus <- westminster_entropy$clus_buff[1]

# getting the main roads of westminster to plot the map
westminster_roads_main <- getOSMdata(westminsterbb,k = "highway",val = main_roads) %>% 
  osm_poly2line() %>% .$osm_lines

westminster_roads_main <- westminster_roads_main[c("osm_id","name","highway","geometry")]
st_crs(westminster_roads_main) <- 4326

#### Plotting summary statistics of the amenities for the report ####

tmap_mode("plot")

haringey_amenity_map <- tm_shape(haringey_roads) + tm_lines(col = "black"
                                                            ,size = 1) + 
  tm_shape(haringey_amenities) + tm_dots(col = "amenity"
                                       ,legend.show = TRUE
                                       ,size = 0.1) +
  tm_layout(legend.outside = TRUE) +
  tmap_options(max.categories = 73)

haringey_amenity_map

london_summary <- london_amenities %>% 
  st_drop_geometry() %>%
  dplyr::group_by(amenity) %>%
  dplyr::count() %>% 
  dplyr::arrange(desc(n))

london_summary_tex <- bind_cols(london_summary[1:32,]
                                ,london_summary[33:64,]
                                ,london_summary[65:96,]
                                ,.name_repair= "unique") 

colnames(london_summary_tex) <- c("amenities_1", "n_1","amenities_2", "n_2","amenities_3", "n_3")

table_latex <- xtable::xtable(london_summary_tex
                              ,caption = "London summary"
                              #,align = c("|","c","c","c","c","c","c")
                              )
print(table_latex,
      latex.environments =  c("scriptsize", "center", "widestuff"),
      tabular.environment = "longtable",
      floating = FALSE
)

tail(london_summary) 

#### MAPS ####

tmap_mode("plot")


max_cluster_haringey_map <- tm_shape(haringey_amenities) + tm_dots(col = "clus"
                                                                   ,palette = "Set1"
                                                                   ,size = 0.01
                                                                   ,title = "clusters"
                                                                   ,id = "entropy"
                                                                   ,n=8
) + 
  # tm_shape(london_stations) + tm_dots(col = "black"
  #                                     ,size = 0.02) +
  tm_layout(main.title = "clusters of Haringey") + 
  tm_legend(legend.outside=T) + 
  tm_scale_bar() 
max_cluster_haringey_map

max_cluster_westminster_map <- tm_shape(westminster_roads_main) + tm_lines(col = "black"
                                                                           ,alpha = 0.6) +
  tm_shape(westminster_entropy) + tm_dots(col = "clus"
                                         ,palette = "Set1"
                                         ,size = 0.01
                                         ,title = "clusters"
                                         ,id = "entropy"
                                         ,n=8
                                         ) + 
  # tm_shape(london_stations) + tm_dots(col = "black"
  #                                     ,size = 0.02) +
  tm_layout(main.title = "clusters of Westminster") + 
  tmap_options(max.categories = 41) +
  tm_legend(legend.outside=T) + 
  tm_scale_bar() 

max_cluster_westminster_map

#### Creating tilted layers on top of each other  ####
# follow tutorial from: https://www.urbandemographics.org/post/figures-map-layers-r/ 
# calculating isochrones: https://ipeagit.github.io/r5r/articles/calculating_isochrones.html 

#### PERFORMANCE ####

t <- c(1:10)
time <- c(1.855,4.433,  8.336, 11.252, 15.421, 21.749, 26.212, 38.690, 48.290, 55.819)
plot(t,time)


perf_fit <- glm(log(time) ~ log(t))

perf_fit$coefficients

exp(1.5*log(10))

london_amenites <- NULL

#### Loading data for london ####
## THIS IS VERY IMPORTANT
# we want data on amenities, shops and tourism based on the osm map features to compute their entropy. 

london_amenites <- getOSMdata(Londonbb, "amenity") %>% osm_group_points(.,k = "amenity", amenityofinterest)

head(london_amenites)

sf::st_write(london_amenites,"london_amenities.geojson")

london_shops <- getOSMdata(Londonbb, "shop") %>% osm_group_points(.,k = "shop", shopofinterest)

london_tourism <- getOSMdata(Londonbb, k="tourism") %>% osm_group_points(.,k="tourism",kofinterest = tourismofinterest)

london_amenities <- rbind(london_amenites,london_shops,london_tourism)

### THIS IS THE FINAL DATA SET OF AMENITIES FOR LONDON. 
length(unique(london_amenities$osm_id))
length(london_amenities$osm_id)

# dropping duplicates that can occur on the way. 
london_amenities <- london_amenities[!duplicated(london_amenities$osm_id),]

sf::st_write(london_amenities, "london_amenities.geojson",append=FALSE,delete_dsn = TRUE,overwrite = TRUE)

main_roads <- c("primary","secondary","tertiary")

london_roads <- getOSMdata(Londonbb, k = "highway", val = main_roads) %>% osm_poly2line()
london_roads <- london_roads$osm_lines %>% .[c("osm_id","name","highway","geometry")]

sf::st_write(london_roads, "london_roads.geojson",append=FALSE,delete_dsn = TRUE,overwrite = TRUE)

#### entropy ####
# THIS CODE DOES THE ENTROPY MEASURE FOR WESTMINSTER AND WAS PART OF TESTS.


st_crs(westminster_amenities) <- 4326
westminster_amenities <- sf::st_transform(westminster_amenities,27700)

west_test <- westminster_amenities[1:200,]

# numcores should be set to 12 for the final computation. 

numCores <- 4

time <- system.time({
  entropy <- neighbors_entropy_par(westminster_amenities[1:200,],r=500, cor_num = numCores)
})
time

entropy

bind_cols(west_test,entropy)

plot(entropy[,1],entropy[,2])

#### Plotting the entropy ####

westminster_entropy_map <- tm_shape(west_test) + tm_dots(col = "entropy"
                                         ,palette = "viridis"
                                         ,size = 0.01
                                         ,style = "cont"
                                         ,title = "Entropy"
  ) + tm_layout(main.title = "Amenities of Westminster"
  ) +
  tm_legend(legend.outside=T) + tm_scale_bar()

#westminster_entropy_map

tmap_save(westminster_entropy_map
          ,filename = "westminster_entropy.pdf"
          ,width = 12
          ,height = 8
          ,asp = 0
          )
