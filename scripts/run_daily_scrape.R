# scripts/run_daily_scrape.R

target_date <- as.Date(Sys.time(), tz = "Australia/Sydney") - 1
message("Running daily scrape for snapshot date: ", target_date)

source("scripts/02_fetch_google.R")
source("scripts/03_fetch_reddit.R")

message("Daily scrape complete for snapshot date: ", target_date)