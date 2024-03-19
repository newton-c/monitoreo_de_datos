library(tidyverse)
library(httr)
library(jsonlite)
library(readr)

# you need to add your api key and email (in that order) for you ACLED 
# account to a keys.txt file in the working directory for this code to work.
# if using git/github, add keys.txt to .gitignore so you don't end up sharing
# your private credentials

inputs <- read_delim("keys.txt", delim = "\n")
key <- inputs[[1, 1]]
email <- inputs[[2, 1]]

rows_returned <- 5000
page <- 1

#caribe_list <- tibble()
#get_caribe_data <- function() {
#if (rows_returned == 5000 & page < 20) {
#centam_url = paste0("https://api.acleddata.com/acled/read?key=",
#                key, "&email=", email, "&region=14", "&page=", page)
#centam_res = GET(centam_url)
#centam_data = fromJSON(rawToChar(centam_res$content))$data
#caribe_list[[length(caribe_list) + 1]] <- centam_data
#rows_returned <- nrow(centam_data)
#page <- page + 1
#return(caribe_list)
#} else
#  return(caribe_list)
#}
#test <- full_join(caribe_list, caribe_data)

southam_url = paste0("https://api.acleddata.com/acled/read?key=",
                    key, "&email=", email, "&region=15")
southam_res = GET(southam_url)
southam_data = fromJSON(rawToChar(southam_res$content))$data

caribe_url = paste0("https://api.acleddata.com/acled/read?key=",
                    key, "&email=", email, "&region=16")
caribe_res = GET(caribe_url)
caribe_data = fromJSON(rawToChar(caribe_res$content))$data

mex_url = paste0("https://api.acleddata.com/acled/read?key=",
                     key, "&email=", email, "&iso=484")
mex_res = GET(mex_url)
mex_data = fromJSON(rawToChar(mex_res$content))$data
