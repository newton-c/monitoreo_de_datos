# añadir una tableta con las cifras en el mapa

library(sf)
 my_spdf <- st_read("data/Venezuela Esequibo/Estados_Venezuela.shp") |>
   filter(ID != 24)

 # Events 
 ggplot() + 
   geom_sf(data = my_spdf, size = 1.5, color = "#3B3B3B", fill = "#FAFAFA") + 
   geom_point(data = pv_ven,
              aes(x = as.numeric(longitude),
                  y = as.numeric(latitude),
                  shape = event_type,
                  color = event_type),
              size = 4) +
   coord_sf() +
   labs(shape = "Event Type", color = "Event Type") +
   theme_void()
 
 pv_ven_event
 
 # Sub events
 ggplot() + 
   geom_sf(data = my_spdf, size = 1.5, color = "#3B3B3B", fill = "#FAFAFA") + 
   geom_point(data = pv_ven,
              aes(x = as.numeric(longitude),
                  y = as.numeric(latitude),
                  shape = sub_event_type,
                  color = sub_event_type),
              size = 4) +
   coord_sf() +
   labs(shape = "Sub-Event Type", color = "Sub-Event Type") +
   theme_void()
 
 # April test map
 pv_ven_last_month <- pv_ven |>
   filter(month(event_date) == month(max_date) - 1)
 
 ggplot() + 
   geom_sf(data = my_spdf, size = 1.5, color = "#3B3B3B", fill = "#FAFAFA") + 
   geom_point(data = pv_ven_last_month,
              aes(x = as.numeric(longitude),
                  y = as.numeric(latitude),
                  shape = sub_event_type,
                  color = sub_event_type),
              size = 4, alpha = .6) +
   coord_sf() +
   labs(shape = "Sub-Event Type", color = "Sub-Event Type",
        title = "Political Violence Events in Venezuela",
        caption = paste(plot_month,
                        "Source: Armed Conflict Location & Events Database",
                        sep = "\n"),
        subtitle = paste("During",
                         month(max(pv_ven_last_month$event_date), label = TRUE,
                                   abbr = FALSE),
        max(pv_ven_last_month$year), sep = " ")) +
   theme_void() +
   theme(plot.title = ggplot2::element_text(family = "Roboto Black", 
                                            size = 24, color = "#3B3B3B", 
                                            hjust = 0, margin = margin(b = 25)),
         plot.title.position = "plot",
         plot.subtitle = ggplot2::element_text(family = "Roboto", 
                                               size = 19, color = "#3b3b3b",
                                               hjust = 0,
                                               margin = ggplot2::margin(9, 0, 30, 0)),
        plot.caption = element_text(family = "Roboto", size = 11,
                                    color = "#b3b3b3", hjust = 0),
        plot.margin = margin(25, 25, 25, 25),
        legend.text = element_text(family = "Roboto", color = "#3B3B3B",
                                   size = 11),
        legend.title = element_text(size = 14))

#table
 pv_ven_last_month |>
   select(sub_event_type, event) |>
   group_by(sub_event_type) |>
   summarise(Events = sum(event, na.rm = TRUE)) |>
   rename(`Sub-Event Type` = sub_event_type) |>
   gt()
 