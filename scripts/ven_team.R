library(tidyverse)
library(ICplots)
library(lubridate)
library(aweek)

theme_set(theme_ic())
set_week_start("Saturday")

max_date <- Sys.Date() - wday(Sys.Date() + 1)

# Political Violence -----------------------------------------------------------
# Venezuela

ven_data <- filter(southam_data, country == "Venezuela")

pv_ven <- ven_data |>
  filter(disorder_type == "Political violence") |>
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

ggplot(data = last_two_weeks_ven, aes(x = week, y = events)) +
  geom_col(fill = "#B0182A") +
  geom_text(aes(label = events), nudge_y = 10) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en Venezuela") +
  hline +
  theme(plot.title.position = "plot")

ggplot(subset(pv_ven_events, week_begins > min(week_begins, na.rm = TRUE)),
                     aes(x = week_begins, y = events)) +
  geom_line(color = "#B0182A", linewidth = 2) +
  geom_text(aes(label = events), nudge_y = 5) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia política en Venezuela") +
  hline +
  theme(plot.title.position = "plot")

# Civilian Targeting -----------------------------------------------------------
ct_ven <- pv_ven |>
  filter(civilian_targeting == "Civilian targeting") |>
  mutate(week = week(event_date),
         week_begins = as.POSIXct(paste(1, week, year, sep = "-" ),
                                  format = "%u-%U-%Y"),
         event = 1) |>
  group_by(week_begins) |>
  summarise(events = sum(event))

ggplot(data = ct_ven, aes(x = week_begins, y = events)) +
  geom_line(color = "#B0182A", linewidth = 2) +
  geom_text(aes(label = events), nudge_y = 1) +
  xlab("") +
  ylab("Numero de eventos") +
  labs(title = "Violencia contra civíles en Venezuela") +
  hline +
  theme(plot.title.position = "plot")

