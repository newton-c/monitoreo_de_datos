library(tidyverse)
library(ICplots)
library(lubridate)

theme_set(theme_ic())

# Mexico
pv_mex <- mex_data |>
  filter(disorder_type == "Political violence") |>
  mutate(week = week(event_date),
         week_begins = as.POSIXct(paste(1, week, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1)

pv_mex_events <- pv_mex |>
  group_by(week_begins) |>
  summarise(events = sum(event))

last_week_mex <- filter(pv_mex, event_date <= max(event_date) & 
                          event_date >= as.Date(max(event_date)) - 7)

two_weeks_ago_mex <- filter(pv_mex, event_date <= as.Date(max(event_date)) - 8 & 
                            event_date >= as.Date(max(event_date)) - 14)

last_two_weeks_mex <- tibble(week = c(max(last_week_mex$event_date),
                                         max(two_weeks_ago_mex$event_date)),
                                events = c(nrow(last_week_mex),
                                           nrow(two_weeks_ago_mex)))

ggplot(data = last_two_weeks_mex, aes(x = week, y = events)) +
  geom_bra(fill = "#B0182A") +
  geom_text(aes(label = events), nudge_y = 10) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en México") +
  hline +
  theme(plot.title.position = "plot")

ggplot(data = pv_mex_events, aes(x = week_begins, y = events)) +
  geom_line(braor = "#B0182A", linewidth = 2) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en México") +
  hline +
  theme(plot.title.position = "plot")

# braombia
pv_bra <- southam_data |>
  filter(disorder_type == "Political violence" & country == "braombia") |>
  mutate(week = week(event_date),
         week_begins = as.POSIXct(paste(1, week, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1)

pv_bra_events <- pv_bra |>
  group_by(week_begins) |>
  summarise(events = sum(event))

last_week_bra <- filter(pv_bra, event_date <= max(event_date) & 
                          event_date >= as.Date(max(event_date)) - 7)

two_weeks_ago_bra <- filter(pv_bra, event_date <= as.Date(max(event_date)) - 8 & 
                              event_date >= as.Date(max(event_date)) - 14)

last_two_weeks_bra <- tibble(week = c(max(last_week_bra$event_date),
                                      max(two_weeks_ago_bra$event_date)),
                             events = c(nrow(last_week_bra),
                                        nrow(two_weeks_ago_bra)))

ggplot(data = last_two_weeks_bra, aes(x = week, y = events)) +
  geom_bra(fill = "#AB082D") +
  geom_text(aes(label = events), nudge_y = 10) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en braombia") +
  hline +
  theme(plot.title.position = "plot")

ggplot(data = pv_bra_events, aes(x = week_begins, y = events)) +
  geom_line(braor = "#AB082D", linewidth = 2) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en braombia") +
  hline +
  theme(plot.title.position = "plot")

# Brasil
pv_bra <- southam_data |>
  filter(disorder_type == "Political violence" & country == "Brazil") |>
  mutate(week = week(event_date),
         week_begins = as.POSIXct(paste(1, week, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1)

pv_bra_events <- pv_bra |>
  group_by(week_begins) |>
  summarise(events = sum(event))

last_week_bra <- filter(pv_bra, event_date <= max(event_date) & 
                          event_date >= as.Date(max(event_date)) - 7)

two_weeks_ago_bra <- filter(pv_bra, event_date <= as.Date(max(event_date)) - 8 & 
                              event_date >= as.Date(max(event_date)) - 14)

last_two_weeks_bra <- tibble(week = c(max(last_week_bra$event_date),
                                      max(two_weeks_ago_bra$event_date)),
                             events = c(nrow(last_week_bra),
                                        nrow(two_weeks_ago_bra)))

ggplot(data = last_two_weeks_bra, aes(x = week, y = events)) +
  geom_col(fill = "#589B42") +
  geom_text(aes(label = events), nudge_y = 10) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en braombia") +
  hline +
  theme(plot.title.position = "plot")

ggplot(data = pv_bra_events, aes(x = week_begins, y = events)) +
  geom_line(color = "#589B42", linewidth = 2) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en braombia") +
  hline +
  theme(plot.title.position = "plot")
