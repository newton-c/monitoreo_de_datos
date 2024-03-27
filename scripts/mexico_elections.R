mex_officials <- mex_data |>
  filter(grepl("local administrators", tags) == TRUE |
         grepl("government officials", tags) == TRUE) |>
  filter(disorder_type == "Political violence") |>
  mutate(month = months(as.Date(event_date)),
         event = 1) 

mex_officials_month <- mex_officials |>
  group_by(month, year) |>
  summarise(events = sum(event, na.rm = TRUE)) |>
  mutate(year_mon = as.Date(paste(month, year, "01", sep = " "),
                            format = "%B %Y %d"))

ggplot(data = mex_officials_month, mapping = aes(x = year_mon, y = events)) +
  geom_line(linewidth = 2, color = "#B10A25") +
  geom_text(aes(label = events), vjust = -1) +
  scale_x_date(breaks = mex_officials_month$year_mon,
               labels = format(mex_officials_month$year_mon, "%b %y")) +
  xlab("") +
  ylab("") +
  hline +
  labs(title = paste("Political Violence Events Targeting Local Administrators",
                     "and Government Officials", sep = "\n"),
       subtitle = paste0(min(mex_officials$event_date), " to ",
                         max(mex_officials$event_date)),
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
        legend.title = element_text(size = 14))


source("scripts/mexico_elections_map.R")

# geographic hotspots
mex_officials_admin1 <- mex_officials |>
  group_by(admin1) |>
  summarise(events = sum(event, na.rm = TRUE)) 

ggplot() +
  geom_col(data = mex_officials_admin1,
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
