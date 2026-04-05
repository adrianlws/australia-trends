# scripts/02_fetch_google.R

library(httr2)
library(xml2)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(janitor)

target_date <- as.Date(Sys.time(), tz = "Australia/Sydney") - 1
now_sydney <- with_tz(Sys.time(), tzone = "Australia/Sydney")

fetch_google_trends_au <- function(target_date) {
  rss_url <- "https://trends.google.com/trending/rss?geo=AU"
  
  resp <- request(rss_url) |>
    req_user_agent("australia-trends-hobby-project/0.1") |>
    req_perform()
  
  xml_text <- resp_body_string(resp)
  doc <- read_xml(xml_text)
  
  items <- xml_find_all(doc, ".//item")
  
  if (length(items) == 0) {
    stop("No Google Trends items found.")
  }
  
  google_df <- tibble(
    source = "google_trends",
    platform = "google",
    topic = xml_text(xml_find_first(items, "./title")),
    pub_date_raw = xml_text(xml_find_first(items, "./pubDate")),
    link = xml_text(xml_find_first(items, "./link")),
    description = xml_text(xml_find_first(items, "./description"))
  ) |>
    clean_names() |>
    mutate(
      fetched_at = now_sydney,
      pub_date = suppressWarnings(
        parse_date_time(pub_date_raw, orders = c("a, d b Y H:M:S z", "d b Y H:M:S z"))
      ),
      pub_date_sydney = with_tz(pub_date, tzone = "Australia/Sydney"),
      pub_date_local = as.Date(pub_date_sydney)
    ) |>
    filter(pub_date_local == target_date) |>
    mutate(
      snapshot_date = target_date
    ) |>
    select(
      snapshot_date, fetched_at, source, platform,
      topic, pub_date, pub_date_sydney, link, description
    )
  
  if (nrow(google_df) == 0) {
    warning("Google Trends returned no rows for target_date = ", target_date)
  }
  
  google_df
}

google_df <- fetch_google_trends_au(target_date)

out_file <- file.path(
  "data", "raw", "google",
  paste0("google_trends_", target_date, ".csv")
)

write_csv(google_df, out_file)

message("Saved Google Trends data to: ", out_file)
print(google_df)