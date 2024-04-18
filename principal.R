list.of.packages <- c("tidyverse", "ICplots", "lubridate", "aweek", "httr",
                      "jsonlite", "readr", "zoo", "gt", "sf")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm(list = c("list.of.packages", "new.packages"))

library(tidyverse)
library(ICplots)
library(lubridate)
library(aweek)
library(httr)
library(jsonlite)
library(readr)
library(zoo)
library(gt)
library(sf)

theme_set(theme_ic())
set_week_start("Saturday")

Sys.setlocale("LC_ALL", "es_ES")
gráfico_mes <- str_to_title(as.character(format((Sys.Date()), "%B %Y")))

Sys.setlocale("LC_ALL", "en_US")
max_date <- Sys.Date() - wday(Sys.Date() + 1)
plot_month <- as.character(format((Sys.Date()), "%B %Y"))

ic_logo <- function(x, y, hjust = 0, vjust = 0, size = 8) {
  geom_text(aes(x = x, y = y,
                label = "i\u200An\u200As\u200Ai\u200Ag\u200Ah\u200At\u200Ac\u200Ar\u200Ai\u200Am\u200Ae\u200A.\u200Ao\u200Ar\u200Ag"),
            size=size, family = "Noto Serif", fontface = "italic",
            colour = "#a5a5a5", hjust = hjust, vjust = vjust)
}

source("scripts/acled.R")
#source("scripts/ven_team.R")
source("scripts/col_team.R")
source("scripts/mexico_elections.R")
#source("scripts/acled_cleaning.R")