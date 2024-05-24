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

mes_actual <- format(Sys.Date(), "%m")
# Political violence
pv_col <- southam_data |>
  filter(disorder_type == "Political violence" & country == "Colombia" &
           event_date < as.Date(paste0(format(Sys.Date(), "%Y-%m"), "-01"))) |>
  mutate(week = date2week(event_date, numeric = TRUE, week_start = "Saturday"),
         week_begins = as.POSIXct(paste(6, week - 2, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1)

pv_col_events <- pv_col |>
  group_by(week_begins) |>
  summarise(events = sum(event))


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
  labs(title = "Primary Indicators of Violence in Colombia",
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
  labs(title = "Primary Indicators of Violence in Colombia",
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


# Only looking at Cauca, Nariño and Valle del Cauca
departments <-c('Cauca', 'Narino', 'Valle del Cauca')

pv_CNVdC_event_month <- pv_col |>
  filter(event_type %in% col_events) |>
  filter(admin1 %in% departments) |>
  mutate(month = format(as.Date(event_date), "%m")) |>
  group_by(month, year, event_type) |>
  summarise(events = sum(event, na.rm = TRUE)) |>
  mutate(begin = as.Date(paste(year, month, "01", sep = "-")))

ggplot() +
  geom_line(data = pv_CNVdC_event_month,
            mapping = aes(x = begin, y = events),
            linewidth = 1,
            color = "#AB082D") +
  geom_text(data = pv_CNVdC_event_month,
            mapping = aes(x = begin, y = events, label = events), vjust = -1) +
  facet_wrap(facets = ~event_type, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_x_date(breaks = pv_CNVdC_event_month$begin,
               labels = month(pv_CNVdC_event_month$begin, label = TRUE)) +
  labs(title = "Primary Indicators of Violence in Colombia",
       subtitle = "By event type in Cauca, Nariño, and Valle de Cauca",
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

pv_CNVdC_sub_event_month <- pv_col |>
  filter(sub_event_type %in% col_sub_events) |>
  filter(admin1 %in% departments) |>
  mutate(month = format(as.Date(event_date), "%m")) |>
  group_by(month, year, sub_event_type) |>
  summarise(events = sum(event, na.rm = TRUE)) |>
  mutate(begin = as.Date(paste(year, month, "01", sep = "-")))

ggplot() +
  geom_line(data = pv_CNVdC_sub_event_month,
            mapping = aes(x = begin, y = events),
            linewidth = 1,
            color = "#AB082D") +
  geom_text(data = pv_CNVdC_sub_event_month,
            mapping = aes(x = begin, y = events, label = events), vjust = -1) +
  facet_wrap(facets = ~sub_event_type, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_x_date(breaks = pv_CNVdC_sub_event_month$begin,
               labels = month(pv_CNVdC_sub_event_month$begin, label = TRUE)) +
  labs(title = "Primary Indicators of Violence in Colombia",
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
