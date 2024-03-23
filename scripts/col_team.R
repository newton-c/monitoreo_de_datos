# Events:
# ACLED Events:
#     Battles
#     Explosions/Remote violence              
#     Strategic developments
#     Violence against civilians
# ACLED Sub Events:
#     Abduction/forced disappearance                  
#     Armed clash                       
#     Arrests
#     Attack      
#     Change to group/activity         
#     Disrupted weapons use
#     Grenade  
#     Remote explosive/landmine/IED               
#     Sexual violence

# Political violence
pv_col <- southam_data |>
  filter(disorder_type == "Political violence" & country == "Colombia") |>
  mutate(week = date2week(event_date, numeric = TRUE, week_start = "Saturday"),
         week_begins = as.POSIXct(paste(6, week - 2, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1)

pv_col_events <- pv_col |>
  group_by(week_begins) |>
  summarise(events = sum(event))

last_week_col <- filter(pv_col, event_date <= max_date & 
                          event_date >= as.Date(max_date) - 6)

two_weeks_ago_col <- filter(pv_col, event_date <= as.Date(max_date) - 7 & 
                              event_date >= as.Date(max_date) - 13)

last_two_weeks_col <- tibble(week = c(max(last_week_col$event_date),
                                      max(two_weeks_ago_col$event_date)),
                             events = c(nrow(last_week_col),
                                        nrow(two_weeks_ago_col)))

ggplot(data = last_two_weeks_col, aes(x = week, y = events)) +
  geom_col(fill = "#AB082D") +
  geom_text(aes(label = events), nudge_y = 2) +
  xlab("") +
  ylab("Number of Events") +
  labs(title = "Political Violence in Colombia") +
  hline +
  theme(plot.title.position = "plot")

ggplot(subset(pv_col_events, week_begins > min(week_begins, na.rm = TRUE)),
                     aes(x = week_begins, y = events)) +
  geom_text(aes(label = events), nudge_y = 5) +
  geom_line(color = "#AB082D", linewidth = 2) +
  xlab("") +
  ylab("Number of Events") +
  labs(title = "Political Violence in Colombia") +
  hline +
  theme(plot.title.position = "plot")

# Monthly event types
col_events <- c("Battles", "Remote violence", "Strategic developments",
                "Violence against civilians")

pv_col_event_month <- pv_col |>
  filter(event_type %in% col_events) |>
  mutate(month = format(as.Date(event_date), "%m")) |>
  group_by(month, year, event_type) |>
  summarise(events = sum(event, na.rm = TRUE)) |>
  mutate(begin = as.Date(paste(year, month, "01", sep = "-")))

ggplot() +
  geom_line(data = pv_col_event_month,
            mapping = aes(x = begin, y = events),
            linewidth = 1,
            color = "#AB082D") +
  geom_text(data = pv_col_event_month,
            mapping = aes(x = begin, y = events, label = events), vjust = -1) +
  facet_wrap(facets = ~event_type, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_x_date(breaks = pv_col_event_month$begin,
               labels = month(pv_col_event_month$begin, label = TRUE)) +
  labs(title = "Political Violence Events in Colombia",
       subtitle = "By event type",
       caption = paste(plot_month,
                       "Souce: Armed Conflcit Location & Events Database",
                       sep = "\n")) +
  ylab("") +
  xlab("") +
  theme(strip.background = element_blank(),
        strip.text = element_text(margin = margin(b = 10)),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(family = "Roboto", size = 11,
                                    color = "#b3b3b3", hjust = 0),
        plot.caption.position = "plot") +
  hline 




# Monthly sub-events
col_sub_events <- c("Abduction/forced disappearance", "Armed clash", "Arrests",
                    "Attack", "Change to group/activity",
                    "Disrupted weapons use", "Grenade",
                    "Remote explosive/landmine/IED", "Sexual violence")

pv_col_sub_event_month <- pv_col |>
  filter(sub_event_type %in% col_sub_events) |>
  mutate(month = format(as.Date(event_date), "%m")) |>
  group_by(month, year, sub_event_type) |>
  summarise(events = sum(event, na.rm = TRUE)) |>
  mutate(begin = as.Date(paste(year, month, "01", sep = "-")))

ggplot() +
  geom_line(data = pv_col_sub_event_month,
            mapping = aes(x = begin, y = events),
            linewidth = 1,
            color = "#AB082D") +
  geom_text(data = pv_col_sub_event_month,
            mapping = aes(x = begin, y = events, label = events), vjust = -1) +
  facet_wrap(facets = ~sub_event_type, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_x_date(breaks = pv_col_sub_event_month$begin,
               labels = month(pv_col_sub_event_month$begin, label = TRUE)) +
  labs(title = "Political Violence Events in Colombia",
       subtitle = "By sub-event type",
       caption = paste(plot_month,
                       "Souce: Armed Conflcit Location & Events Database",
                       sep = "\n")) +
  ylab("") +
  xlab("") +
  theme(strip.background = element_blank(),
        strip.text = element_text(margin = margin(b = 10)),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(family = "Roboto", size = 11,
                                    color = "#b3b3b3", hjust = 0),
        plot.caption.position = "plot") +
  hline 

  
