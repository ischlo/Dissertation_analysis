#### Loading and manipulating OSM data ####

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


#### Entropy calculation ####

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


#### Neighbourhood formation ####


# using buffers around amenities. 
# FUNCTION NOT USED BEACAUSE THE ISOCHRONE WERE USED IN THE FINAL VERSION
neighbourhoods_buff <- function(data, r = 500, cores = 1) {
  # pass data here in 27700 CRS
  registerDoParallel(cores)
  d_buff <- sf::st_buffer(data,r)
  int <- sf::st_intersects(d_buff, data)
  data$ind <- 1:nrow(data)
  max <- foreach(i = 1:nrow(data), .combine = c) %dopar% {
    nb <- data[int[[i]],]
    indice <- i
    #osmid <- data$osm_id[i]
    m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
    while(m != indice) {
      #nb <- data[int[[which(data$osm_id == m)]],]
      nb <- data[int[[m]],]
      indice <- m
      m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
    }
    m
  }
  stopImplicitCluster()
  max
}


neighbourhoods_iso <- function(data,iso, cores = 1) {
  # pass data here in 27700 CRS
  # make sure that the isochrones data is perfectly alligned by row with the data.
  int <- sf::st_intersects(iso, data)
  # once we have made the intersection, we need to check that they all intersect at least with their own amenity
  # this is not the case every time because some amenities are in locations with no roads around
  # while the isochrones uses the underlyinig road network to build the areas. 
  checks <- map(int,length) %>% unlist() %>% tibble()
  # when the intersection is zero, we impose that there is just the amenity for which the isochrone is computed
  bad_values <- which(checks$. == 0)
  # put the index of the amenity itself in the intersection
  int[bad_values] <- bad_values
  data$ind <- 1:nrow(data)
  registerDoParallel(cores)
  max <- foreach(i = 1:nrow(data), .combine = c) %dopar% {
    nb <- data[int[[i]],]
    indice <- i
    #osmid <- data$osm_id[i]
    m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
    while(m != indice) {
      #nb <- data[int[[which(data$osm_id == m)]],]
      nb <- data[int[[m]],]
      indice <- m
      m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
    }
    m
  }
  stopImplicitCluster()
  max
}


