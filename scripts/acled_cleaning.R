library(tidyverse)
library(ICplots)
library(lubridate)
library(aweek)

theme_set(theme_ic())
set_week_start("Saturday")

max_date <- Sys.Date() - wday(Sys.Date() + 1)

# Añadir los datos históricos
# reparar la definición de semana
# monitorear las elecciones 


# Political Violence -----------------------------------------------------------
# Mexico
pv_mex <- mex_data |>
  filter(disorder_type == "Political violence") |>
  mutate(week = date2week(event_date, numeric = TRUE, week_start = "Saturday"),
         week_begins = as.POSIXct(paste(6, week - 2, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1)

pv_mex_events <- pv_mex |>
  group_by(week_begins) |>
  summarise(events = sum(event))

last_week_mex <- filter(pv_mex, event_date <= max_date & 
                          event_date >= as.Date(max_date) - 6)

two_weeks_ago_mex <- filter(pv_mex, event_date <= as.Date(max_date) - 7 & 
                            event_date >= as.Date(max_date) - 13)

last_two_weeks_mex <- tibble(week = c(max(last_week_mex$event_date),
                                         max(two_weeks_ago_mex$event_date)),
                                events = c(nrow(last_week_mex),
                                           nrow(two_weeks_ago_mex)))

ggplot(data = last_two_weeks_mex, aes(x = week, y = events)) +
  geom_col(fill = "#B0182A") +
  geom_text(aes(label = events), nudge_y = 10) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en México") +
  hline +
  theme(plot.title.position = "plot")

ggplot(subset(pv_mex_events, week_begins > min(week_begins, na.rm = TRUE)),
                     aes(x = week_begins, y = events)) +
  geom_line(color = "#B0182A", linewidth = 2) +
  geom_text(aes(label = events), nudge_y = 10) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en México") +
  hline +
  theme(plot.title.position = "plot")

# colombia
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
  geom_text(aes(label = events), nudge_y = 10) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en Colombia") +
  hline +
  theme(plot.title.position = "plot")

ggplot(subset(pv_col_events, week_begins > min(week_begins, na.rm = TRUE)),
                     aes(x = week_begins, y = events)) +
  geom_text(aes(label = events), nudge_y = 10) +
  geom_line(color = "#B0182A", linewidth = 2) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en Colombia") +
  hline +
  theme(plot.title.position = "plot")

# Brasil
pv_bra <- southam_data |>
  filter(disorder_type == "Political violence" & country == "Brazil") |>
  mutate(week = date2week(event_date, numeric = TRUE, week_start = "Saturday"),
         week_begins = as.POSIXct(paste(6, week - 2, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1)

pv_bra_events <- pv_bra |>
  group_by(week_begins) |>
  summarise(events = sum(event))

last_week_bra <- filter(pv_bra, event_date <= max_date & 
                          event_date >= as.Date(max_date) - 6)

two_weeks_ago_bra <- filter(pv_bra, event_date <= as.Date(max_date) - 7 & 
                              event_date >= as.Date(max_date) - 13)

last_two_weeks_bra <- tibble(week = c(max(last_week_bra$event_date),
                                      max(two_weeks_ago_bra$event_date)),
                             events = c(nrow(last_week_bra),
                                        nrow(two_weeks_ago_bra)))

ggplot(data = last_two_weeks_bra, aes(x = week, y = events)) +
  geom_col(fill = "#589B42") +
  geom_text(aes(label = events), nudge_y = 10) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en Brasil") +
  hline +
  theme(plot.title.position = "plot")

ggplot(subset(pv_bra_events, week_begins > min(week_begins, na.rm = TRUE)),
                     aes(x = week_begins, y = events)) +
  geom_line(color = "#589B42", linewidth = 2) +
  geom_text(aes(label = events), nudge_y = 10) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en Brasil") +
  hline +
  theme(plot.title.position = "plot")

# Civilian Targeting -----------------------------------------------------------
# Mexico
ct_mex <- pv_mex |>
  filter(civilian_targeting == "Civilian targeting") |>
  mutate(week = week(event_date),
         week_begins = as.POSIXct(paste(1, week, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1) |>
  group_by(week_begins) |>
  summarise(events = sum(event))

ggplot(data = ct_mex, aes(x = week_begins, y = events)) +
  geom_line(color = "#B0182A", linewidth = 2) +
  geom_text(aes(label = events), nudge_y = 10) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia contra civíles en México") +
  hline +
  theme(plot.title.position = "plot")

# Colombia
ct_col <- pv_col |>
  filter(civilian_targeting == "Civilian targeting") |>
  mutate(week = week(event_date),
         week_begins = as.POSIXct(paste(1, week, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1) |>
  group_by(week_begins) |>
  summarise(events = sum(event))

ggplot(data = ct_col, aes(x = week_begins, y = events)) +
  geom_line(color = "#B0182A", linewidth = 2) +
  geom_text(aes(label = events), nudge_y = 10) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia contra civíles en Colombia") +
  hline +
  theme(plot.title.position = "plot")

# Brasil
ct_bra <- pv_bra |>
  filter(civilian_targeting == "Civilian targeting") |>
  mutate(week = week(event_date),
         week_begins = as.POSIXct(paste(1, week, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1) |>
  group_by(week_begins) |>
  summarise(events = sum(event))

ggplot(data = ct_bra, aes(x = week_begins, y = events)) +
  geom_line(color = "#B0182A", linewidth = 2) +
  geom_text(aes(label = events), nudge_y = 10) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia contra civíles en Brasil") +
  hline +
  theme(plot.title.position = "plot")

# Sexual Violence -----------------------------------------------------------
# Mexico
sv_mex <- pv_mex |>
  filter(sub_event_type == "Sexual violence") |>
  mutate(week = week(event_date),
         week_begins = as.POSIXct(paste(1, week, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1) |>
  group_by(week_begins) |>
  summarise(events = sum(event))

ggplot(data = sv_mex, aes(x = week_begins, y = events)) +
  geom_line(color = "#B0182A", linewidth = 2) +
  geom_text(aes(label = events), nudge_y = .25) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia sexual en México") +
  hline +
  theme(plot.title.position = "plot")

# Colombia
sv_col <- pv_col |>
  filter(sub_event_type == "Sexual violence") |>
  mutate(week = week(event_date),
         week_begins = as.POSIXct(paste(1, week, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1) |>
  group_by(week_begins) |>
  summarise(events = sum(event))

ggplot(data = sv_col, aes(x = week_begins, y = events)) +
  geom_line(color = "#B0182A", linewidth = 2) +
  geom_text(aes(label = events), nudge_y = .25) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia sexual en Colombia") +
  hline +
  theme(plot.title.position = "plot")

# Brasil
sv_bra <- pv_bra |>
  filter(sub_event_type == "Sexual violence") |>
  mutate(week = week(event_date),
         week_begins = as.POSIXct(paste(1, week, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1) |>
  group_by(week_begins) |>
  summarise(events = sum(event))

ggplot(data = sv_col, aes(x = week_begins, y = events)) +
  geom_line(color = "#B0182A", linewidth = 2) +
  geom_text(aes(label = events), nudge_y = .25) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia sexual en Brasil") +
  hline +
  theme(plot.title.position = "plot")


# latam
latam_data <- rbind(caribe_data, centam_data, southam_data, mex_data) |>
  filter(disorder_type == "Political violence") |>
  mutate(week = week(event_date),
         week_begins = as.POSIXct(paste(1, week, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1)

pv_latam_events <- latam_data |>
  group_by(week_begins) |>
  summarise(events = sum(event))

last_week_latam <- filter(latam_data, event_date <= max_date & 
                          event_date >= as.Date(max_date) - 7) |>
  group_by(country) |>
  summarise(events_last_week = sum(event))

two_weeks_ago_latam <- filter(latam_data,
                              event_date <= as.Date(max_date) - 8 & 
                              event_date >= as.Date(max_date) - 14) |>
  group_by(country) |>
  summarise(events_two_weeks_ago = sum(event))

last_two_weeks_latam <- full_join(last_week_latam, two_weeks_ago_latam) |>
  replace_na(list(events = 0, events_two_weeks_ago = 0)) |>
  mutate(change = events - events_two_weeks_ago)

last_two_weeks_latam$country <- factor(last_two_weeks_latam$country) %>%
  fct_reorder(last_two_weeks_latam$change)

ggplot(data = last_two_weeks_latam) +
  geom_col(aes(x = country, y = change), 
           fill = ifelse(last_two_weeks_latam$change > 0,
                         "#84163C", "darkblue")) +
  xlab("") +
  ylab("Cambio en el numero de evento") +
  coord_flip() +
  hline
