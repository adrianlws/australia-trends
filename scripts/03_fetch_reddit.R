# scripts/03_fetch_reddit.R

library(httr2)
library(xml2)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(lubridate)
library(tibble)

target_date <- as.Date(Sys.time(), tz = "Australia/Sydney") - 1
now_sydney <- with_tz(Sys.time(), tzone = "Australia/Sydney")

subreddits <- read_csv("config/subreddits.csv", show_col_types = FALSE) |>
  filter(enabled)

safe_xml_text <- function(node, xpath) {
  found <- xml_find_first(node, xpath)
  if (inherits(found, "xml_missing")) return(NA_character_)
  xml_text(found)
}

fetch_subreddit_rss <- function(subreddit) {
  rss_url <- paste0("https://old.reddit.com/r/", subreddit, "/new/.rss")
  
  resp <- request(rss_url) |>
    req_user_agent("australia-trends-hobby-project/0.1 by u/yourusername") |>
    req_headers(
      Accept = "application/atom+xml, application/xml, text/xml;q=0.9, */*;q=0.8"
    ) |>
    req_timeout(30) |>
    req_perform()
  
  txt <- resp_body_string(resp)
  
  # Save raw response for debugging if needed
  raw_debug_file <- file.path(
    "data", "raw", "reddit",
    paste0("reddit_rss_raw_", subreddit, "_", target_date, ".xml")
  )
  writeLines(txt, raw_debug_file, useBytes = TRUE)
  
  doc <- read_xml(txt)
  entries <- xml_find_all(doc, ".//*[local-name()='entry']")
  
  if (length(entries) == 0) {
    message("No RSS entries found for r/", subreddit)
    return(tibble())
  }
  
  tibble(
    fetched_at = now_sydney,
    source = "reddit",
    platform = "reddit",
    subreddit = subreddit,
    post_id = map_chr(entries, ~ {
      id_txt <- safe_xml_text(.x, ".//*[local-name()='id']")
      if (is.na(id_txt)) return(NA_character_)
      str_extract(id_txt, "(?<=comments/)[a-z0-9]+")
    }),
    title = map_chr(entries, ~ safe_xml_text(.x, ".//*[local-name()='title']")),
    author = map_chr(entries, ~ safe_xml_text(.x, ".//*[local-name()='author']/*[local-name()='name']")),
    published = map_chr(entries, ~ safe_xml_text(.x, ".//*[local-name()='published']")),
    updated = map_chr(entries, ~ safe_xml_text(.x, ".//*[local-name()='updated']")),
    link = map_chr(entries, ~ {
      node <- xml_find_first(.x, ".//*[local-name()='link'][@rel='alternate']")
      if (inherits(node, "xml_missing")) return(NA_character_)
      xml_attr(node, "href")
    }),
    content = map_chr(entries, ~ safe_xml_text(.x, ".//*[local-name()='content']"))
  ) |>
    mutate(
      created_utc = suppressWarnings(ymd_hms(updated, tz = "UTC")),
      created_utc = coalesce(created_utc, suppressWarnings(ymd_hms(published, tz = "UTC"))),
      created_sydney = with_tz(created_utc, tzone = "Australia/Sydney"),
      created_local_date = as.Date(created_sydney),
      score = NA_real_,
      num_comments = NA_real_,
      over_18 = NA,
      is_self = NA
    ) |>
    filter(!is.na(title), !is.na(created_local_date))
}

reddit_all <- map_dfr(subreddits$subreddit, fetch_subreddit_rss)

if (nrow(reddit_all) == 0) {
  warning("No Reddit RSS data returned at all.")
  reddit_df <- tibble()
} else {
  reddit_yesterday <- reddit_all |>
    filter(created_local_date == target_date)
  
  # fallback: latest 10 per subreddit if yesterday is empty
  fallback_rows <- reddit_all |>
    group_by(subreddit) |>
    arrange(desc(created_sydney), .by_group = TRUE) |>
    mutate(row_num = row_number()) |>
    filter(row_num <= 10) |>
    ungroup()
  
  missing_subs <- setdiff(unique(reddit_all$subreddit), unique(reddit_yesterday$subreddit))
  
  reddit_fallback <- fallback_rows |>
    filter(subreddit %in% missing_subs)
  
  reddit_df <- bind_rows(reddit_yesterday, reddit_fallback) |>
    distinct(subreddit, post_id, .keep_all = TRUE) |>
    mutate(snapshot_date = target_date) |>
    select(
      snapshot_date, fetched_at, source, platform, subreddit, post_id,
      title, author, created_utc, created_sydney, created_local_date,
      link, content, score, num_comments, over_18, is_self
    )
}

out_file <- file.path(
  "data", "raw", "reddit",
  paste0("reddit_rss_", target_date, ".csv")
)

write_csv(reddit_df, out_file)

message("Saved Reddit RSS data to: ", out_file)
print(reddit_df)