#### library ####
library(sf) #TO READ SHAPEFILES 
library(tidyverse) #TO MANIPULATE CSV FILES 
library(iterators)
library(tmap)
library(tmaptools)
library(here)
library(dplyr)
library(parallel)
library(osmdata) # to load data from OSM
library(foreach) # for parallel computing
library(doParallel)
library(ggplot2)
library(mapboxapi)
library(units)
R.Version()

gc()

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

total_amenities <- length(amenityofinterest) + length(shopofinterest) + length(tourismofinterest)

#### Loading data ####

# importing the thames

london_roads <- london_roads %>% tibble() %>% st_sf() %>% st_set_crs(4326)

thames <- getOSMdata(Londonbb,k = "water", val = "river")

thames <- thames %>% osmdata::osm_poly2line() %>%.$osm_lines

thames <- thames[c("osm_id","name","geometry")] %>% 
  st_set_crs(4326) %>%
  st_transform(27700)

parks <- getOSMdata(Londonbb,k = "leisure", val = "park") %>% .$osm_polygons

parks <- parks[c("osm_id","name","leisure","area","geometry")] %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) %>%
  mutate(area = st_area(geometry))

parks <- parks%>%
  filter(area > set_units(500000,m^2))
  
  
median(parks$area)
mean(parks$area)

parks %>% qtm(fill = "green")

westminsterbb <- getbb("Westminster city, London, UK", format_out = "sf_polygon") %>%
  st_set_crs(4326)%>%
  st_transform(27700)

#thames %>% qtm(col="blue", lines.lwd = 2, lines.col = "blue")

london_amenities <- sf::st_read(dsn = here::here("london_amenities.geojson"))
london_roads <- sf::st_read(dsn = here::here("london_roads.geojson"))

london_entropy

st_crs(london_amenities) <- 4326
london_amenities <- st_transform(london_amenities,27700)

load_thames()




#### Entropy using isochrones ####

# test in haringey
# right projection of the bbox
haringey_poly  <- haringey_poly %>% st_transform(27700)


# character osm id for ease of use.
london_amenities_isochrones$id <- as.character(london_amenities_isochrones$id)

# getting the amenities inside haringey so that they match the ones in the isochrones perfectly. 
haringey_entropy <- london_entropy %>% 
  filter(st_intersects(.,haringey_poly, sparse = FALSE)) %>% 
  tibble() %>% 
  st_sf()

# character osm id for ease of use.
haringey_entropy$osm_id <- as.character(haringey_entropy$osm_id)

# getting the isochrones of the same amenities and setting the right crs.
haringey_amenities_isochrones <- london_amenities_isochrones %>% filter(id %in% haringey_entropy$osm_id) %>% st_transform(27700)
# additionally can check whjether the id vectors are the same. 

entropy_iso <- haringey_entropy %>% 
  neighbors_entropy_iso(cor_num = 3)

haringey_entropy$entropy_iso <- entropy_iso$entropy
# differentce to see how they differ
haringey_entropy <- haringey_entropy %>% mutate(entropy_diff = entropy-entropy_iso)

#map of the difference. 
qtm(haringey_entropy, dots.col = "entropy_diff", palette = "plasma", midpoint = NA)

hist(haringey_entropy$entropy_iso, breaks = 100)

## checking the isochrones. 

tmap_mode("view")
london_amenities_isochrones[which(checks$. == 0),] %>% qtm(fill.alpha = 0.3)

london_entropy[which(checks$. == 0),] %>% qtm(fill.alpha = 0.3)

london_entropy
# the isochrones are computed from the underlying spatial network of roads. HOwever, it is not everywhere well defined and some
# isochrones have no amenities in them at all. The following lines correct that and are added in the entropy with isochrones function.
int <- london_amenities_isochrones %>% st_transform(27700) %>% sf::st_intersects(.,london_entropy)

checks <- map(int,length) %>% unlist() %>% tibble()

bad_values <- which(checks$. == 0)

int[bad_values] <- bad_values

# now it works !!
london_amenities[bad_values,] %>% neighbors_entropy_iso(.,london_amenities_isochrones[bad_values,] %>% st_transform(27700))

#### Analyzing the results from isochrones. ####
entropy_iso <- sf::st_read("amenities_entropy_iso.geojson")
entropy_iso <- entropy_iso %>% tibble() %>% st_sf() %>% st_set_crs(27700)

entropy_iso_max <- entropy_iso %>% filter(entropy > 3.1) %>% st_transform(4326) %>% st_set_crs(4326)

neighbourhoods_iso_map <- tm_shape(london_roads, bbox = bb) + tm_lines(col = "grey"
                                                                   ,alpha = 0.5
                                                                   ) +
  tm_shape(entropy_iso_max) + tm_dots(col = "orange"
                                      ,size= 0.01
                                      ,alpha = 0.3) +
  tm_shape(thames) + tm_lines(col = "blue"
                              ,legend.col.show = TRUE) +
  tm_shape(neighbourhoods) +
  tm_text(text = "neighbourhood"
          ,size= 0.8
          ,alpha = 1
          #,ymod = -0.2
          ,auto.placement = TRUE
          ,col = "black") + 
  tm_layout(frame = TRUE) +
  tm_scale_bar(text.size = .7)

neighbourhoods_iso_map

tmap_mode("plot")

london_entropy_iso_map <- tm_shape(london_roads) + tm_lines(col = "black"
                                                        ,alpha = 0.6) + 
  tm_shape(entropy_iso) + tm_dots(col = "entropy"
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
london_entropy_iso_map


london_size_iso_map <- tm_shape(london_roads) + tm_lines(col = "black"
                                                     ,alpha = 0.6) + 
  tm_shape(entropy_iso) + tm_dots(col = "size"
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
london_size_iso_map

tmap_save(london_entropy_iso_map
          ,filename = "london_entropy_iso.pdf"
          ,width = w
          ,height = h
)

tmap_save(london_size_iso_map
          ,filename = "london_size_iso.pdf"
          ,width = w
          ,height = h
)


#### Analyzing the results  ####

#famous neighbourhoods that emerge: 
  
neighbourhood <- c("Notting Hill Gate", "Kensington", "South Kensington", "Marylebone", "Bloomsbury","Fitzrovia"
                   ,"Camden town","Shepherd's Bush", "Hampstead", "Kilburn", "Islington", "Stoke Newington", "Holloway", "Pimlico"
                   , "Elephant and Castle","Chelsea", "Greenwich", "Peckham", "Fulham"
                   , "Hackney", "Dalston", "Stratford", "Barbican", "Wembley") 

neighbourhoods <- tibble(neighbourhood) %>% 
  mutate(geometry = map(neighbourhood,get_neighbourhood)) %>% 
  st_as_sf(crs = 4326)

tmap_mode("plot")

bb <- c(-0.3337,51.4454,0.0439,51.5762) %>% bb_poly() %>% st_set_crs(4326)

neighbourhoods_map <- tm_shape(london_roads, bbox = bb) + tm_lines(col = "grey"
                                                        ,alpha = 0.5
                                                        ) +
  tm_shape(london_entropy_max) + tm_dots(col = "orange"
                                        ,size= 0.01
                                        ,alpha = 0.3) +
  tm_shape(thames) + tm_lines(col = "blue"
                              ,legend.col.show = TRUE) +
  tm_shape(neighbourhoods) +
  tm_text(text = "neighbourhood"
          ,size= 0.8
          ,alpha = 1
          #,ymod = -0.2
          ,auto.placement = TRUE
          ,col = "black") + 
  tm_layout(frame = TRUE) +
  tm_scale_bar(text.size = .7)

neighbourhoods_map

# famous neighbourhoods that don't emerge : SOHO, Mayfair. 

london_entropy <- sf::st_read("london_entropy_2nd/london_amenities_entropy.geojson")

st_crs(london_entropy) <- 27700

london_stations <- london_stations[c("osm_id","name","geometry")]
st_crs(london_stations) <- 4326
london_stations <- sf::st_transform(london_stations,27700)
london_entropy_max <- filter(london_entropy, entropy > 3.0)

#### Entropy and size summary statistics ####

london_entropy_case <- filter(london_entropy, size < 500)

london_max <- london_entropy[which(london_entropy$entropy == max(london_entropy$entropy)),]

plot(london_entropy_case$size,london_entropy_case$entropy)

strange_values <- filter(london_entropy, size < 350 & size > 250 & entropy > 2.0 & entropy < 2.3)

plot(london_entropy$size
     ,london_entropy$entropy
     #,type = "p"
     ,xlab = "density of amenities"
     ,ylab = "entropy"
     ,main = "entropy vs density"
     ,bty = "n"
     ,col = "tomato"
     ,srt = 45
     ,family = "Helvetica"
     ,bg="grey"
     ,cex.main = 1.3
     ,cex.lab = 1.3
     #,log = "x"
     ,cex = 0.1
     ,xaxt = "n"
     )
axis(1, at = seq(0, 1600, by = 100), las=2)

london_entropy$size[which(london_entropy$entropy == max(london_entropy$entropy))]

# entropy distribution
par(mai = c(1,1,1,1))
hist(london_entropy$entropy
     ,main = "Entropy distribution"
     ,xlab = "entropy"
     ,cex.main = 1.3
     ,cex.lab = 1.3
     )
abline(v = mean(london_entropy$entropy)
       ,col ="steelblue" 
       ,lty = 2
       ,lwd = 2.5
       )
text(2.7,17000, "mean = 2.74", col = "steelblue", adj = c(1,0))

# size distribution
par(mai = c(1,1,1,1))
hist(london_entropy$size
     ,main = "Density"
     ,xlab = "size"
     ,cex.main = 1.3
     ,cex.lab = 1.3
)
abline(v = mean(london_entropy$size)
       ,col ="steelblue" 
       ,lty = 2
       ,lwd = 2.5
)
text(mean(london_entropy$size),25000, "mean = 186",cex=1.3, col = "steelblue", adj = c(-0.1,0))

###
##
london_low_density <- filter(london_entropy, size < 400)

low_high_density_map <-  tm_shape(london_roads) + tm_lines(col = "black"
                                  ,alpha = 0.5) + 
  tm_shape(entropy_iso) + tm_dots(col = "size"
                                     ,size = 0.005
                                     ,breaks = c(0,400,16000)
                                     ,n = 3
                                     ,palette = "plasma"
                                     ,alpha = 0.3
                                     ,title = "Density") + 
  tm_layout(main.title = "High density areas"
            ,main.title.size = 0.5
            ,legend.outside = TRUE
            ,legend.outside.size = 1.3
            #,legend.title.size = 0.7
            )

#low_high_density_map

tmap_save(low_high_density_map
          ,filename = "low_high_density_map.pdf"
          ,height = h
          ,width = w
          ,units = "in"
          #,asp = 0
)

#london_low_density

ggplot(london_entropy, aes(x = size, y = entropy)) +
  geom_point()

tmap_mode("view")
qtm(strange_values)

qtm(strange_values)

hist(london_entropy$size)

hist(london_entropy$entropy)

map_1 <- ggplot(haringey_amenities, aes(X,Y)) +
  stat_density_2d(aes(fill = stat(nlevel)), 
                  geom = "polygon",
                  n = 100,bins = 10,contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A") +
  ggtitle("Entropy of amenities in Haringey") +
  theme_bw()
map_1


#### Plotting the entropy ####

tmap_mode("plot")

london_low_density_map <- tm_shape(london_entropy) + tm_dots(col = "black"
                                                                 ,palette = "viridis"
                                                                 ,size = 0.005
                                                                 ,style = "fisher"
                                                                 ,title = "Entropy Score"
                                                                 ,id = "amenity"
                                                                 ,legend.hist = TRUE
                                                                 ,n=8
                                                                 ,legend.hist.title = "Distribution of values"
) + tm_shape(london_low_density) + tm_dots(col = "sienna"
                                                             ,palette = "viridis"
                                                             ,size = 0.005
                                                             ,style = "fisher"
                                                             ,title = "Entropy Score"
                                                             ,id = "amenity"
                                                             ,legend.hist = TRUE
                                                             ,n=8
                                                             ,legend.hist.title = "Distribution of values"
) + 
  # tm_shape(london_stations) + tm_dots(col = "black"
  #                                     ,size = 0.02) +
  tm_layout(main.title = "Amenities of London") + 
  tm_legend(legend.outside=T) + 
  tm_scale_bar()

london_low_density_map

# tm_shape(london_roads) + tm_lines(col = "black"
#                                   ,alpha = 0.6) + 
  

london_entropy_map <- tm_shape(london_roads) + tm_lines(col = "black"
                                                        ,alpha = 0.6) + 
  tm_shape(london_entropy) + tm_dots(col = "entropy"
                                      ,palette = "viridis"
                                      ,size = 0.005
                                      ,style = "fisher"
                                      ,title = "Entropy Score"
                                      ,id = "amenity"
                                      ,legend.hist = TRUE
                                      ,n=8
                                      ,legend.hist.title = "Distribution of values"
                                      ) + 
  # tm_shape(london_stations) + tm_dots(col = "black"
  #                                     ,size = 0.02) +
  tm_layout(main.title = "Amenities of London") + 
  tm_legend(legend.outside=T) + 
  tm_scale_bar()

london_entropy_map

london_size_map <- tm_shape(london_roads) + tm_lines(col = "black"
                                                        ,alpha = 0.6) + 
  tm_shape(london_entropy) + tm_dots(col = "size"
                                     ,palette = "viridis"
                                     ,size = 0.005
                                     ,style = "fisher"
                                     ,title = "density"
                                     ,id = "amenity"
                                     ,legend.hist = TRUE
                                     ,n=8
                                     ,legend.hist.title = "Distribution of values"
  ) + 
  # tm_shape(london_stations) + tm_dots(col = "black"
  #                                     ,size = 0.02) +
  tm_layout(main.title = "Amenities of London") + 
  tm_legend(legend.outside=T) + 
  tm_scale_bar()

london_size_map

#### Saving maps ####

#sf::st_write(london_amenities,"london_amenities_entropy.geojson",delete_dsn = TRUE,layer = "entropy")
h <- 8.3
w <- 11.3

tmap_save(london_entropy_map
          ,filename = "london_entropy_2.pdf"
          ,height = h
          ,width = w
          ,units = "in"
          #,asp = 0
)

tmap_save(london_size_map
          ,filename = "london_size.pdf"
          ,height = h
          ,width = w
          ,units = "in"
          #,asp = 0
)

tmap_save(london_entropy_map
          ,filename = "london_entropy_max.html"
          #,asp = 0
)


tmap_save(neighbourhoods_map
          ,filename = "neighbourhood_maxes.pdf"
          ,height = h
          ,width = w
          ,units = "in"
          #,asp = 0
)
