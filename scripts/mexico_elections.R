mex_officials <- mex_data |>
  filter(grepl("local administrators", tags) == TRUE |
         grepl("government officials", tags) == TRUE) |>
  filter(disorder_type == "Political violence") |>
  mutate(month = months(as.Date(event_date)),
         event = 1) 

mex_officials_month <- mex_officials |>
  group_by(month, year) |>
  summarise(events = sum(event, na.rm = TRUE)) |>
  mutate(year_mon = as.yearmon(paste(month, year, sep = " "), format = "%B %Y"))

ggplot(data = mex_officials_month, mapping = aes(x = year_mon, y = events)) +
  geom_line(linewidth = 2, color = "#B10A25") +
  geom_text(aes(label = events), vjust = -1) +
  xlab("") +
  ylab("") +
  hline

mex_official_last_month <- mex_officials |>
  filter(format(month, "%m")  == (max(format(month, "%m")) - 1))

source("scripts/mexico_elections_map.R")