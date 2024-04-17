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

max_date <- Sys.Date() - wday(Sys.Date() + 1)
plot_month <- as.character(format((Sys.Date()), "%B %Y"))

source("scripts/acled.R")
#source("scripts/ven_team.R")
source("scripts/col_team.R")
source("scripts/mexico_elections.R")
#source("scripts/acled_cleaning.R")