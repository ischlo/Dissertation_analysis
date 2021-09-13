#### Libs ####

library(sf)
library(tidyverse)
library(doParallel)
library(parallel)
library(foreach)
library(tmap)
library(ggplot2)
library(profvis)
 
#### Functions ####

# more on parallel computing:
# https://groups.google.com/g/davis-rug/c/DIofOdFZgHI?pli=1 
# https://raw.githack.com/uo-ec607/lectures/master/12-parallel/12-parallel.html
#### LOAD DATA ####

signals <- readr::read_csv(file = "signals_set.csv"
                           ,delim =","
                           ,col_types = cols(
                             id = "c",
                             lat = col_double(),
                             lon = col_double(),
                             date = col_date(format = ""),
                             hour = "i"
                           ))

users <- sf::st_read(dsn = "FILEPATH/users.geojson") %>% 
  dplyr::select(userid,rlat,rlon)


london_amenities <- sf::st_read("FILEPATH/amenities_entropy_iso.geojson") %>% 
  st_set_crs(27700)

signals_joined <- NULL

s_max <- 4.56

#### Analyzing the results ####
# when a variable name ends with "_short", it means it is obtained from GPS signals emitted within less than 3km from home. 

london_hex_short <- list.load("FILEPATH/london_hex_short.rdata") %>% 
  st_set_crs(27700)

london_hex_short <- london_hex_short %>%
  dplyr::left_join(entropy_hex,by = c("ind"), ) %>%
  tidyr::replace_na(replace = list(entropy = -1))

london_hex_short %>% filter(visits>300) %>% nrow()


tm_shape(london_hex_short %>% filter(visits > 70)) + 
  tm_polygons(col = "visits"
             ,palette = "viridis"
             ,border.alpha = 0
             ,n = 10
             ) #+
  # tm_symbols(size = "visits"
  #            ,col = "visits"
  #            ,palette = "viridis"
  #            ,size.lim = c(300,800))

london_entropy_short <- london_hex_short %>%
  filter(entropy >= 0 & visits >= 1)

hist(london_entropy_short$visits, n = 100)
hist(london_entropy_short$entropy)

# fit of the number of visitors with respect to the local diversity. 

entropy_visits <- glm(log(visits) ~ entropy, family = gaussian, london_entropy_short)

summary(entropy_visits)

plot(london_entropy_short$entropy
     ,london_entropy_short$visits
     ,main = "Visitors vs Entropy"
     ,log = "y"
     ,ylab = "# visitors"
     ,xlab = "S of location"
     ,cex.lab = 1.3
     )
points(london_entropy_short$entropy
       ,exp(entropy_visits$fitted.values)
       ,col = "darkred")

# cor is surprisingly high...
cor(london_entropy_short$entropy,exp(entropy_visits$fitted.values), method = "pearson")^2

sum(entropy_visits$residuals) # is zero, which is good

plot(entropy_visits$fitted.values,entropy_visits$residuals)
     # %>% hist(breaks = 100
     #                              ,freq = FALSE
     #                              ,main = "Residuals")

# plot(london_entropy_short$entropy,exp(entropy_visits$fitted.values))

## FORMAT TO PLOT THE VISITORS vs ENTROPY PLOT

med_entr <- median(london_entropy_short$entropy)

med_visit <- median(london_entropy_short$visits)

visitors_entropy_plot <- ggplot(london_entropy_short, aes(x=entropy, y=visits) ) +
  geom_hex(bins = 70) + 
  geom_vline(xintercept = med_entr,colour = "orange") +
  geom_hline(yintercept=med_visit,colour = "orange") +
  scale_fill_continuous(type = "viridis") +
  scale_y_log10() +
  xlab("Entropy") +
  ylab("# of visitors") + 
  theme_bw(base_size = 14)

visitors_entropy_plot

#legend()

#### cleaning of signals ####

# signals of haringey

signals <- signals %>% 
  filter(hour < 23 & hour > 5) %>%
  st_coordinates() %>%
  data.frame() %>%
  tibble() %>%
  bind_cols(signals) %>%
  st_drop_geometry() %>% 
  group_by(id,date,hour) %>%
  distinct(X,Y,.keep_all = TRUE) %>% 
  st_sf()

signals_hex <- signals %>% st_join(.,london_hex["ind"], left = TRUE)


# signals of westminster
signals_west_hex <- signals_west %>% st_join(.,london_hex["ind"], left = TRUE)

signals_west_filtered <- signals_west_hex %>% 
  st_coordinates() %>%
  data.frame() %>%
  tibble() %>%
  mutate(X = round(X)
         ,Y = round(Y)) %>% 
  bind_cols(signals_west_hex) %>%
  group_by(id,date,hour) %>%
  distinct(X,Y,.keep_all = TRUE) %>% 
  st_sf() %>%
  st_drop_geometry() %>% 
  st_as_sf(coords = c("X","Y"), crs = 27700) %>% 
  ungroup() %>% filter(hour < 23 & hour > 5)
  
## looking at the most active users.

signals_users <- signals_west_filtered %>% ungroup() %>%
  st_drop_geometry() %>% 
  dplyr::count(id)

signals_users$n %>% hist(breaks = 100)

signals_density <- signals_west_filtered %>%
  st_drop_geometry() %>% 
  dplyr::group_by(hour) %>%
  summarise(usr = length(unique(id)), sig = n()) %>%
  mutate(dens = sig/usr)

signals_density$dens %>% hist(breaks = 100)

signals_density %>% plot()

active_users <- signals_users %>% 
  arrange(desc(n)) %>% 
  filter(n >2000)

active_users_stops <- signals_west_filtered %>% 
  st_drop_geometry() %>%
  filter(id %in% active_users$id) %>%
  #dplyr::group_by(id,date,hour,ind) %>% 
  # dplyr::mutate(n = dplyr::n()) %>%
  # dplyr::filter(n >=3) %>%
  # dplyr::ungroup() %>% 
  st_as_sf(coords = c("lon","lat"),crs = 4326) %>% 
  st_transform(27700) #%>%
  #filter(st_intersects(geometry,westminsterbb, sparse = FALSE))

active_users_stops$id %>% unique() %>% length()

## When signals are properly filtered, having a threshold of 3-4 
## per user per time per hex seems a good take.
# finally, after massive filtering, 1 is also a good value for the study.

active_users_stops$n %>% hist(breaks = 100)

tmap_mode("plot")

traces_example <- tm_shape(londonbb[1,]
                           ,bbox = westminsterbb
                           ) + tm_polygons(col = "grey"
                                        ,alpha = 0.3) +
  # tm_shape(westminsterbb) + tm_polygons(col = "grey"
  #                                        ,size = 0.1
  #                                        #,palette = "Set1"
  #                                        ) +
  tm_shape(active_users_stops) + tm_dots(col = "id"
                                         ,palette = "Set1"
                                         ,size = 0.05
                                         ,title = "Traces of selected users") +
  tm_scale_bar(text.size = 1.2,breaks = c(1,2,3)) +
  tm_layout(legend.show = FALSE)

traces_example

#
tmap_save(traces_example, "traces_example_zoom.pdf")


#
#### DATA ENGINEERING (NOT UP TO DATE), see above and in the RC files. ####

# tranforming into a sf object to easily switch coordinates 

users <- users %>%
  # transforming the users residential coordinates into 27700
  st_transform(27700) %>% 
  # setting new columns with east nor coords from 27700
  mutate(r_x = st_coordinates(.)[,1]
         ,r_y = st_coordinates(.)[,2])

# merging the signals with the user to compute the distance from home. 
signals$distance <- st_transform(signals,27700) %>% 
  # extracting the coordinates in the right projection as new variables
  mutate(x = st_coordinates(.)[,1]
         ,y = st_coordinates(.)[,2]) %>%
  # subsetting the only columns needed. 
  select(id,x,y) %>% 
  # dropping the geometry column
  st_drop_geometry() %>%
  # joining the signals coordinates and users coordinates by id. 
  #We get for each signal the residence of the user who emitted it. 
  left_join(users %>% st_drop_geometry(),by = c("id" = "userid")) %>% 
  # simple cartesian distance thanks to the projection. 
  #Very quick. No need to use the distance function. 
  mutate(distance = sqrt((x-r_x)^2+(y-r_y)^2)) %>%
  # pull function to retrieve only the column
  dplyr::pull(distance)


# westminster 
# merging the signals with the user to compute the distance from home. 
signals_west$distance <- signals_west %>% 
  # extracting the coordinates in the right projection as new variables
  mutate(x = st_coordinates(.)[,1]
         ,y = st_coordinates(.)[,2]) %>%
  # subsetting the only columns needed. 
  select(id,x,y) %>% 
  # dropping the geometry column
  st_drop_geometry() %>%
  # joining the signals coordinates and users coordinates by id. 
  #We get for each signal the residence of the user who emitted it. 
  left_join(users_westminster %>% st_drop_geometry(),by = c("id" = "userid")) %>% 
  # simple cartesian distance thanks to the projection. 
  #Very quick. No need to use the distance function. 
  mutate(distance = sqrt((x-r_x)^2+(y-r_y)^2)) %>%
  # pull function to retrieve only the column
  dplyr::pull(distance)

#### Saving the results. ####
h <- 8.3
w <- 11.3

pdf("histogram_distance.pdf")
hist(signals$distance
     ,breaks = 100
     ,xlab = "Distance (m)"
     ,main = "Distance distribution"
     ,freq = FALSE)
dev.off()