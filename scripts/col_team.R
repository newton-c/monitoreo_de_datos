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
  ylab("Numero de eventos") +
  labs(title = "Violencia política en Colombia") +
  hline +
  theme(plot.title.position = "plot")

ggplot(subset(pv_col_events, week_begins > min(week_begins, na.rm = TRUE)),
                     aes(x = week_begins, y = events)) +
  geom_text(aes(label = events), nudge_y = 5) +
  geom_line(color = "#AB082D", linewidth = 2) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en Colombia") +
  hline +
  theme(plot.title.position = "plot")

# Event types
pv_col_event_type <- pv_col |>
  group_by(week_begins, event_type) |>
  summarise(events = sum(event))

ggplot() +
  geom_line(subset(pv_col_event_type,
                   week_begins > min(week_begins, na.rm = TRUE)),
            mapping = aes(x = week_begins, y = events), linewidth = 1,
            color = "#AB082D") +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en Colombia") +
  hline +
  theme(plot.title.position = "plot") +
  facet_wrap(facets = ~event_type)

# Sub event types
pv_col_sub_event_type <- pv_col |>
  group_by(week_begins, sub_event_type) |>
  summarise(events = sum(event))

ggplot() +
  geom_line(subset(pv_col_sub_event_type,
                   week_begins > min(week_begins, na.rm = TRUE)),
            mapping = aes(x = week_begins, y = events), linewidth = 1,
            color = "#AB082D") +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en Colombia") +
  hline +
  theme(plot.title.position = "plot") +
  facet_wrap(facets = ~sub_event_type)
