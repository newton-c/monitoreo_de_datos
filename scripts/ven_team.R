#ACLED Events:
# Battles
# Explosions/Remote violence               
# Violence against civilians
#ACLED Sub Events:
# Abduction/forced disappearance                   
# Armed clash                        
# Arrests
# Attack       
# Change to group/activity          
# Disrupted weapons use                      
# Grenade   
# Looting/property destruction
# Mob violence                          
# Other               
# Remote explosive/landmine/IED                
# Sexual violence
# Violent demonstration

ven_events_list <- c("Battles", "Explosions/Remote violence",
                     "Violence against civilians")

# Political violence
pv_ven <- southam_data |>
  filter(disorder_type == "Political violence" & country == "Venezuela")

all_ven_pv_filename <- paste0("ven/", "Ven_all_pv_events_", plot_month, ".csv")
write_excel_csv(pv_ven, all_ven_pv_filename)

pv_ven <- pv_ven |>
  mutate(event = 1)

# Monthly event types
pv_ven_event_type <- pv_ven |>
  filter(event_type %in% ven_events_list) |>
  mutate(month = months(as.Date(event_date))) |>
  group_by(month, year, event_type) |>
  summarise(events = sum(event)) |>
  mutate(year_mon = as.yearmon(paste(month, year, "01", sep = " "),
                               format = "%B %Y %d")) 

monthly_ven_events_filename <- paste0("ven/", "Ven_monthly_by_event_type_",
                                      plot_month, ".csv")
write_excel_csv(pv_ven_event_type, monthly_ven_events_filename)

ven_event_plot <- ggplot() +
  geom_line(pv_ven_event_type, 
            mapping = aes(x = year_mon, y = events), linewidth = 2,
            color = "#B20D2B", group = 1) +
  geom_point(pv_ven_event_type,
            mapping = aes(x = year_mon, y = events), size = 1,
            color = "#B20D2B") +
  geom_text(pv_ven_event_type,
            mapping = aes(x = year_mon, y = events, label = events),
            vjust = -1) +
  xlab("") +
  ylab("") +
  labs(title = "Monthly Political Violence Events in Venezuela",
       subtitle = "By event type",
       caption = paste(plot_month,
                       "Souce: Armed Conflcit Location & Events Database",
                       sep = "\n")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_x_yearmon(breaks = pv_ven_event_type$year_mon,
                  labels = month(pv_ven_event_type$year_mon,
                                 label = TRUE)) +
  hline +
  theme(strip.background = element_blank(),
        strip.text = element_text(margin = margin(b = 10, t = 20)),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(family = "Roboto", size = 11,
                                    color = "#b3b3b3", hjust = 0),
        plot.caption.position = "plot") +
  facet_wrap(facets = ~event_type, scales = "free", nrow = 2)
ven_event_plot

# Monthly sub-event types
ven_sub_events_list <- c("Abduction/forced disappearance", "Armed clash",
                         "Arrests", "Attack", "Change to group/activity",
                         "Disrupted weapons use", "Grenade",
                         "Looting/property destruction", "Mob violence",
                         "Other", "Remote explosive/landmine/IED",
                         "Sexual violence", "Violent demonstration") 
  
pv_ven_sub_event_type <- pv_ven |>
  filter(sub_event_type %in% ven_sub_events_list) |>
  mutate(month = months(as.Date(event_date))) |>
  group_by(month, year, sub_event_type) |>
  summarise(events = sum(event)) |>
  mutate(year_mon = as.yearmon(paste(month, year, "01", sep = " "),
                               format = "%B %Y %d"))

monthly_ven_sub_events_filename <- paste0("ven/",
                                          "Ven_monthly_by_sub_event_type_",
                                          plot_month, ".csv")
write_excel_csv(pv_ven_sub_event_type, monthly_ven_sub_events_filename)

ven_sub_event_plot <- ggplot() +
  geom_line(pv_ven_sub_event_type,
            mapping = aes(x = year_mon, y = events), linewidth = 2,
            color = "#B20D2B") +
  geom_point(pv_ven_sub_event_type,
            mapping = aes(x = year_mon, y = events), size = 1,
            color = "#B20D2B") +
  geom_text(pv_ven_sub_event_type,
            mapping = aes(x = year_mon, y = events, label = events),
            vjust = -1) +
  xlab("") +
  ylab("") +
  labs(title = "Monthly Political Violence Events in Venezuela",
       subtitle = "By sub-event type",
       caption = paste(plot_month,
                       "Souce: Armed Conflcit Location & Events Database",
                       sep = "\n")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_x_yearmon(breaks = pv_ven_sub_event_type$year_mon,
                  labels = month(pv_ven_sub_event_type$year_mon,
                                 label = TRUE)) +
  hline +
  theme(strip.background = element_blank(),
        strip.text = element_text(margin = margin(b = 10, t = 20)),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(family = "Roboto", size = 11,
                                    color = "#b3b3b3", hjust = 0),
        plot.caption.position = "plot") +
  facet_wrap(facets = ~sub_event_type, scales = "free")
ven_sub_event_plot


source("scripts/ven_events_map.R")
