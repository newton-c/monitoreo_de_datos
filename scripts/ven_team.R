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
pv_ven <- southam_data |>
  filter(disorder_type == "Political violence" & country == "Venezuela") |>
  mutate(week = date2week(event_date, numeric = TRUE, week_start = "Saturday"),
         week_begins = as.POSIXct(paste(6, week - 2, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1)

pv_ven_events <- pv_ven |>
  group_by(week_begins) |>
  summarise(events = sum(event))

last_week_ven <- filter(pv_ven, event_date <= max_date & 
                          event_date >= as.Date(max_date) - 6)

two_weeks_ago_ven <- filter(pv_ven, event_date <= as.Date(max_date) - 7 & 
                              event_date >= as.Date(max_date) - 13)

last_two_weeks_ven <- tibble(week = c(max(last_week_ven$event_date),
                                      max(two_weeks_ago_ven$event_date)),
                             events = c(nrow(last_week_ven),
                                        nrow(two_weeks_ago_ven)))

pv_2weeks_ven_plot <- ggplot(data = last_two_weeks_ven,
                             aes(x = week, y = events)) +
  geom_col(fill = "#AB082D") +
  geom_text(aes(label = events), nudge_y = 2) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en Venezuela") +
  hline +
  theme(plot.title.position = "plot")
pv_2weeks_ven_plot

pv_ven_plot <- ggplot(subset(pv_ven_events,
                             week_begins > min(week_begins, na.rm = TRUE)),
                     aes(x = week_begins, y = events)) +
  geom_text(aes(label = events), nudge_y = 2) +
  geom_line(color = "#AB082D", linewidth = 2) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en Venezuela") +
  hline +
  theme(plot.title.position = "plot")
pv_ven_plot

# Event types
pv_ven_event_type <- pv_ven |>
  group_by(week_begins, event_type) |>
  summarise(events = sum(event))

ven_event_plot <- ggplot() +
  geom_line(subset(pv_ven_event_type,
                   week_begins > min(week_begins, na.rm = TRUE)),
            mapping = aes(x = week_begins, y = events), linewidth = 1,
            color = "#AB082D") +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en Venezuela") +
  hline +
  theme(plot.title.position = "plot") +
  facet_wrap(facets = ~event_type)
ven_event_plot

# Sub event types
pv_ven_sub_event_type <- pv_ven |>
  group_by(week_begins, sub_event_type) |>
  summarise(events = sum(event))

ven_sub_event_plot <- ggplot() +
  geom_line(subset(pv_ven_sub_event_type,
                   week_begins > min(week_begins, na.rm = TRUE)),
            mapping = aes(x = week_begins, y = events), linewidth = 1,
            color = "#AB082D") +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en Venezuela") +
  hline +
  theme(plot.title.position = "plot") +
  facet_wrap(facets = ~sub_event_type)
ven_sub_event_plot

source("scripts/ven_events_map.R")
