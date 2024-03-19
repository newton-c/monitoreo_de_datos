library(tidyverse)
library(ICplots)
library(lubridate)
library(aweek)
library(httr)
library(jsonlite)
library(readr)

theme_set(theme_ic())
set_week_start("Saturday")

max_date <- Sys.Date() - wday(Sys.Date() + 1)

source("scripts/acled.R")
source("scripts/ven_team.R")
source("scripts/col_team.R")
#source("scripts/acled_cleaning.R")