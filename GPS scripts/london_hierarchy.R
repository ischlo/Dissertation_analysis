london_hclust <- list.load("london_hierarchy/London_hclust.rdata")

# filtering that allows accelerate the computation by removing locations that had 
# a small number of visitor compared to others. However resulting in small holes here and # there in the final map

london_neighbourhood <- london_hex_short %>% filter(visits >= 70)


neighbourhoods_iso_hull

london_roads <- getOSMdata(londonbb %>% st_transform(4326)
                         ,k = "highway"
                         ,val = c("primary"
                                  ,"secondary"
                                  ,"tertiary"
                                  #,"residential"
                                  #,"pedestrian"
                                  )) %>%
  osm_poly2line() %>% 
  .$osm_lines %>% 
  select(osm_id, name,highway,geometry)%>%
  st_set_crs(4326) %>% 
  st_transform(27700)

# percolation on the number of different clusters.
n <- 1000
# Cut tree into n groups
registerDoParallel(numCores)
lnd_grps <- foreach(i = 2:n, .combine="rbind") %dopar% {
  tibble(group = as.character(cutree(london_hclust, k = i))) %>% 
    dplyr::count(group) %>% 
    mutate(frac = n / sum(n)
           ,dist = sqrt(n*sqrt(3)*3*190*190/2)) %>% 
    summarise(max = max(frac)
              ,second_max = sort(frac,decreasing = TRUE)[2]
              ,mean_dist = mean(dist)
              ,med_dist = median(dist)
              ,mena = median(frac)
              ,entropy = -sum(frac*log(frac)))
}
stopImplicitCluster()

#n_RC <- 380


thres_interest <- c(72,105,165, 380)

par(mfrow=c(2,1), mar = c(4,4,1,4), oma = c(1,1,1,1), pty = "m")
plot(2:n
     ,lnd_grps$max
     ,type = "p"
     ,log = "xy"
     ,xlab = ""
     ,ylab = "Size of biggest component"
     ,main = "London"
     ,cex = 0.5
     ,pch = 20
     )
abline(v = thres_interest,col = c("darkred", "purple","orange","black"), lwd = 2)

plot(2:n
     ,lnd_grps$med_dist
     ,xlab = "# clusters"
     ,log = "yx"
     ,ylab = "Median of typical distance distribution"
     ,cex = 0.5
     ,pch = 20)
abline(v = thres_interest,col = c("darkred", "purple","orange","black"), lwd = 2)
par(mfrow=c(1,1))

#which(lnd_grps$mean_dist < 3050 & lnd_grps$mean_dist > 2950)

g <- 4

london_neighbourhood_grouped <- london_neighbourhood %>% 
  mutate(groups = cutree(london_hclust, k = thres_interest[g])) %>% 
  group_by(groups) %>% 
  #filter(n()>10) %>%
  summarise(geometry = st_union(geometry,by_feature = FALSE))

london_neighbourhood_grouped %>% 
  st_area() %>% 
  round(-3) %>% 
  #'/'(10000) %>%
  sqrt() %>% 
  mean()
  hist(freq = FALSE
       ,breaks = 100
       ,xlim = c(0,10000)
       ,xlab = "Distance [m]"
       ,lwd = 2
       ,main =   paste("Typical distance distribution, k="
                       ,as.character(thres_interest[g])))

#tmap_mode("view")
#tmap_mode("plot")

london_map <- 
  tm_shape(london_neighbourhood_grouped) + tm_polygons(col = "MAP_COLORS"
                                                      #,style = "cat"
                                                      ,palette = "Pastel1"
                                                      ,alpha = 1
                                                      ,border.col = "dimgray") +
  tm_shape(thames) + tm_lines(col = "darkblue") +
  tm_scale_bar(text.size = 1.2
               ,lwd = 2
               ,position = c("left","bottom")) +
  tm_layout(main.title = paste("London neighbourhoods, n = ",as.character(thres_interest[g]), sep = "")
            ,legend.outside = TRUE)

london_map
#
#### Comparison with amenities neighbourhoods #### 

haringeybb <- haringeybb %>% st_buffer(dist = 1500)

# tm_shape(neighbourhoods_iso_hull #%>%
#            #t_intersection(south_londonbb)
#          ) + tm_polygons(col = "dimgray"
#                           ,style = "fischer"
#                           ,size = 0.03
#                           ,border.col = "red"
#                           ,alpha = 0.3
#                           ) +
  # tm_shape(amenities_entropy_iso #%>%
  #            #st_intersection(south_londonbb)
  #          ) + tm_dots(col = "entropy"
  #                                                      ,palette = "viridis"
  #                                                      ,style = "fisher"
  #                                                      ,size = 0.02
  #                                                      ,alpha = 1
  #        ) + 
  tm_shape(london_neighbourhood_grouped %>%
             st_intersection(tower_hamlets)
  ) + tm_polygons(col = "MAP_COLORS"
                  ,border.col = "black"
                  ,alpha = .6)
