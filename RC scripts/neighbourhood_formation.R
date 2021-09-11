# run once the loading of packages.
#install.packages('spatstat', repos="http://cran.r-project.org")
#### Libraries ####

library(sf)
library(tidyverse)
library(here)
library(tmap)
library(igraph)
library(doParallel)
library(foreach)
library(stplanr)
library(spatstat)

#### import functions ####

source(here::here("functions.R"))

### Data ####
# specify in 27700 crs. 
londonbb_27700 <- matrix(c(503000.2,150000.8,563000.5,220933.9)
                         ,nrow = 2
                         ,byrow = TRUE
                         , dimnames = list(c("min","max"),c("x","y")))

# data set of amenities with the entropy and size variables.
entropy_data <- sf::st_read(dsn = here::here("london_amenities_entropy.geojson"))

amenities_isochrones <- sf::st_read(dsn = here::here("london_amenities_isochrones.geojson")) %>%
  st_set_crs(4326) %>% 
  st_transform(27700)

st_crs(entropy_data) <- 27700

ncores <- 12

# 
# to be able to compare , run for different values. r = 500, 1000,1500
# n <- 3
# #### Defining the neighbourhoods ####
# THIS IS NOT IMPLEMENTED IN THE END BECAUSE THE ISOCHRONES METHOD WAS THOUGHT TO BE THE MOST PRECISE
# # using the buffer method
# max_buff <- list()
# time_buff <- list()
# 
# for (k in 1:n) {
#   time_buff[[k]] <- system.time({
#     max_buff[[k]] <- entropy_data %>%
#       st_transform(27700) %>%
#       neighbourhoods_buff(r = 500*k,cores = ncores)
#   })
# }
# 
# clus_buff <- do.call(cbind,max_buff) %>% as.data.frame()
# 
# readr::write_csv(clus_buff, "clus_buff.csv")

# using the network method.
# 
# max_graph <- list()
# time_graph <- list()
# 
# for (k in 1:n) {
#   max_graph[[k]] <- entropy_data %>% sf::st_transform(27700) %>% neighbourhoods_graph(ord = (k+2),cores = ncores)
# }
# 
# 
# clus_graph <- do.call(cbind,max_graph)
# 
# readr::write_csv(clus_graph, "clus_graph.csv")
# 
# time_graph


# using the buffer method

time_iso <- system.time({
  max_iso <- entropy_data %>%
    st_transform(27700) %>%
    neighbourhoods_iso(.,amenities_isochrones,cores = ncores)
})

readr::write_csv(max_iso %>% as.data.frame(), "max_iso.csv")

