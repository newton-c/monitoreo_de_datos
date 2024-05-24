mex_shp_file <- paste("data/mex_admbnda_govmex_20210618_SHP",
                      "mex_admbnda_adm1_govmex_20210618.shp", sep = "/")
mex_spd <- st_read(mex_shp_file) 

# Events 
Sys.setlocale("LC_ALL", "en_US")
ggplot() + 
  geom_sf(data = mex_spd, size = 1.5, color = "#3B3B3B", fill = "#FAFAFA") + 
  geom_point(data = subset(mex_officials),
             aes(x = as.numeric(longitude),
                 y = as.numeric(latitude)),
             size = 4,
             shape = 21,
             color = "#B10A25",
             fill = "#B10A25",
             alpha = .3,
             stroke = 1) +
  labs(title = paste("Political Violence Events Targeting Local Administrators",
                     "and Government Officials in Mexico", sep = "\n"),
       subtitle = paste0("Data from ", format(min(as.Date(mex_officials$event_date)),
                                              "%B %d, %Y"),
                         " to ",
                       #  format(max(as.Date(mex_officials$event_date)),
                       #         "%B %d, to %Y")),
                       "May 17, 2024"),
       caption = paste(plot_month,
                       "Source: Armed Conflict Location & Events Database",
                       sep = "\n")) +
  coord_sf() +
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
        legend.title = element_text(size = 14)) +
  ic_logo(x = -115, y = 15, hjust = 0, size = 6)

 # En Español
Sys.setlocale("LC_ALL", "es_ES")

ggplot() + 
  geom_sf(data = mex_spd, size = 1.5, color = "#3B3B3B", fill = "#FAFAFA") + 
  geom_point(data = subset(mex_officials),
             aes(x = as.numeric(longitude),
                 y = as.numeric(latitude)),
             size = 4,
             shape = 21,
             color = "#B10A25",
             fill = "#B10A25",
             alpha = .3,
             stroke = 1) +
  labs(title = paste("Hechos de violencia política contra administradores",
                     "locales y funcionarios de gobierno en México", sep = "\n"),
       subtitle = paste0("Datos del ", format(min(as.Date(mex_officials$event_date)),
                                              "%d de %B de %Y"),
                         " al ",
                       #  format(max(as.Date(mex_officials$event_date)),
                        #        "%d de %B de %Y")),
                       "17 de mayo de 2024"),
       caption = paste(gráfico_mes,
                       "Fuente: Armed Conflict Location & Events Database",
                       sep = "\n")) +
  coord_sf() +
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
        legend.title = element_text(size = 14)) +
  ic_logo(x = -115, y = 15, hjust = 0, size = 6)
