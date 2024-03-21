library(sf)
 my_spdf <- st_read("data/Venezuela Esequibo/Estados_Venezuela.shp")

 # Events 
 ggplot() + 
   geom_sf(data = my_spdf, size = 1.5, color = "#3B3B3B", fill = "#FAFAFA") + 
   geom_point(data = pv_ven,
              aes(x = as.numeric(longitude),
                  y = as.numeric(latitude),
                  shape = event_type),
              size = 2) +
   coord_sf() +
   theme_void()
 
 # Sub events
 ggplot() + 
   geom_sf(data = my_spdf, size = 1.5, color = "#3B3B3B", fill = "#FAFAFA") + 
   geom_point(data = pv_ven,
              aes(x = as.numeric(longitude),
                  y = as.numeric(latitude),
                  shape = sub_event_type),
              size = 2) +
   coord_sf() +
   theme_void()
 