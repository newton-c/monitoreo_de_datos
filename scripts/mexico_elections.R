mex_officials <- mex_data |>
  filter(grepl("local administrators", tags) == TRUE |
         grepl("government officials", tags) == TRUE) |>
  filter(disorder_type == "Political violence") |>
  mutate(month = months(as.Date(event_date)),
         event = 1) 
file_name_mex_off <- paste0("ACLED-targeting_officials-", max_date, ".csv")
write_excel_csv(mex_officials, file_name_mex_off)

mex_officials_month <- mex_officials |>
  group_by(month, year) |>
  summarise(events = sum(event, na.rm = TRUE)) |>
  mutate(year_mon = as.Date(paste(month, year, "01", sep = " "),
                            format = "%B %Y %d"))

Sys.setlocale("LC_ALL", "en_US")

ggplot(data = mex_officials_month, mapping = aes(x = year_mon, y = events)) +
  geom_line(linewidth = 2, color = "#B10A25") +
  geom_text(aes(label = events), vjust = -0.5, size = 6) +
  scale_y_continuous(expand = c(0, 3)) +
  scale_x_date(breaks = mex_officials_month$year_mon,
               labels = format(mex_officials_month$year_mon, "%b %y")) +
  xlab("") +
  ylab("") +
  hline +
  labs(title = paste("Political Violence Events Targeting Local Administrators",
                     "and Government Officials in Mexico", sep = "\n"),
       subtitle = paste0("Data from ", format(min(as.Date(mex_officials$event_date)),
                                              "%B %d, %Y"),
                         " to ",
                         format(max(as.Date(mex_officials$event_date)),
                                "%B %d, to %Y")),
       caption = paste(plot_month,
                       "Source: Armed Conflict Location & Events Database",
                       sep = "\n")) +
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
        plot.caption.position = "plot",
        plot.margin = margin(25, 25, 25, 25),
        legend.text = element_text(family = "Roboto", color = "#3B3B3B",
                                   size = 11),
        legend.title = element_text(size = 14)) +
  ic_logo(x = min(mex_officials_month$year_mon), y = 2, size = 6)

# en español
Sys.setlocale("LC_ALL", "es_ES")

ggplot(data = mex_officials_month, mapping = aes(x = year_mon, y = events)) +
  geom_line(linewidth = 2, color = "#B10A25") +
  geom_text(aes(label = events), vjust = -0.5, size = 6) +
  scale_y_continuous(expand = c(0, 3)) +
  scale_x_date(breaks = mex_officials_month$year_mon,
               labels = str_to_sentence(
                 as.character(format(mex_officials_month$year_mon, "%b %y")))) +
  xlab("") +
  ylab("") +
  hline +
  labs(title = paste("Hechos de violencia política contra administradores",
                     "locales y funcionarios de gobierno en México", sep = "\n"),
       subtitle = paste0("Datos del ", format(min(as.Date(mex_officials$event_date)),
                                              "%d de %B de %Y"),
                         " al ",
                         format(max(as.Date(mex_officials$event_date)),
                                "%d de %B de %Y")),
       caption = paste(gráfico_mes,
                       "Fuente: Armed Conflict Location & Events Database",
                       sep = "\n")) +
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
        plot.caption.position = "plot",
        plot.margin = margin(25, 25, 25, 25),
        legend.text = element_text(family = "Roboto", color = "#3B3B3B",
                                   size = 11),
        legend.title = element_text(size = 14)) +
  ic_logo(x = min(mex_officials_month$year_mon), y = 2, size = 6)

Sys.setlocale("LC_ALL", "en_US")
source("scripts/mexico_elections_map.R")

# geographic hotspots
mex_officials_admin1 <- mex_officials |>
  group_by(admin1) |>
  summarise(events = sum(event, na.rm = TRUE)) 

ggplot() +
  geom_col(data = mex_officials_admin1,
           mapping = aes(x = fct_reorder(admin1, events), y = events),
           fill = "#B10A25",
           width = .75) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("") +
  ylab("") +
  labs(title = paste("Political Violence Events Targeting Local Administrators",
                     "and Government Officials in Mexico", sep = "\n"),
       subtitle = paste0("Data from ", format(min(as.Date(mex_officials$event_date)),
                                              "%B %d, %Y"),
                         " to ",
                         format(max(as.Date(mex_officials$event_date)),
                                "%B %d, to %Y")),
       caption = paste(plot_month,
                       "Source: Armed Conflict Location & Events Database",
                       sep = "\n")) +
  coord_flip() +
  hline +
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
        plot.caption.position = "plot",
        plot.margin = margin(25, 25, 25, 25),
        legend.text = element_text(family = "Roboto", color = "#3B3B3B",
                                   size = 11),
        legend.title = element_text(size = 14),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(linetype = 2,
                                                   color = "#b3b3b3")) +
  ic_logo(x = 1, y = 12.5, hjust = 1, size = 6)

# en español
ggplot() +
  geom_col(data = mex_officials_admin1,
           mapping = aes(x = fct_reorder(admin1, events), y = events),
           fill = "#B10A25",
           width = .75) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("") +
  ylab("") +
  labs(title = paste("Hechos de violencia política contra administradores",
                     "locales y funcionarios del gobierno en México", sep = "\n"),
       subtitle = paste0("Datos del ", format(min(as.Date(mex_officials$event_date)),
                                             "%d de %B de %Y"),
                         " al ",
                         format(max(as.Date(mex_officials$event_date)),
                                    "%d de %B de %Y")),
       caption = paste(gráfico_mes,
                       "Fuente: Armed Conflict Location & Events Database",
                       sep = "\n")) +
  coord_flip() +
  hline +
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
        plot.caption.position = "plot",
        plot.margin = margin(25, 25, 25, 25),
        legend.text = element_text(family = "Roboto", color = "#3B3B3B",
                                   size = 11),
        legend.title = element_text(size = 14),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(linetype = 2,
                                                   color = "#b3b3b3")) +
  ic_logo(x = 1, y = 12.5, hjust = 1, size = 6)

# groups involved
mex_officials_group <- mex_officials |>
  group_by(admin1) |>
  summarise(events = sum(event, na.rm = TRUE)) 

ggplot() +
  geom_col(data = mex_officials_group,
           mapping = aes(x = fct_reorder(admin1, events), y = events),
           fill = "#B10A25") +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("") +
  ylab("") +
  labs(title = paste("Political Violence Events Targeting Local Administrators",
                     "and Government Officials", sep = "\n"),
       subtitle = paste0(min(mex_officials$event_date), " to ",
                         max(mex_officials$event_date)),
       caption = paste(plot_month,
                       "Source: Armed Conflict Location & Events Database",
                       sep = "\n")) +
  coord_flip() +
  hline +
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
        plot.caption.position = "plot",
        plot.margin = margin(25, 25, 25, 25),
        legend.text = element_text(family = "Roboto", color = "#3B3B3B",
                                   size = 11),
        legend.title = element_text(size = 14))
