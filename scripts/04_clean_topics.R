# scripts/04_clean_topics.R

library(dplyr)
library(readr)
library(stringr)

target_date <- as.Date(Sys.time(), tz = "Australia/Sydney") - 1

clean_topic_text <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("&amp;", "and") |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_replace_all("\\b(the|a|an|and|or|to|of|in|on|for|with|at|by|from)\\b", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

google_file <- file.path("data", "raw", "google", paste0("google_trends_", target_date, ".csv"))
reddit_file <- file.path("data", "raw", "reddit", paste0("reddit_rss_", target_date, ".csv"))

if (!file.exists(google_file)) stop("Missing Google raw file: ", google_file)
if (!file.exists(reddit_file)) stop("Missing Reddit raw file: ", reddit_file)

google_raw <- read_csv(google_file, show_col_types = FALSE)
reddit_raw <- read_csv(reddit_file, show_col_types = FALSE)

message("Google columns: ", paste(names(google_raw), collapse = ", "))
message("Reddit columns: ", paste(names(reddit_raw), collapse = ", "))

# Force link/content columns to character if readr guessed them as logical
if ("link" %in% names(google_raw)) {
  google_raw$link <- as.character(google_raw$link)
}
if ("description" %in% names(google_raw)) {
  google_raw$description <- as.character(google_raw$description)
}
if ("link" %in% names(reddit_raw)) {
  reddit_raw$link <- as.character(reddit_raw$link)
}
if ("content" %in% names(reddit_raw)) {
  reddit_raw$content <- as.character(reddit_raw$content)
}

# ---- CLEAN GOOGLE ----
google_df <- google_raw |>
  mutate(date = .data$snapshot_date) |>
  transmute(
    date,
    source = .data$source,
    platform = .data$platform,
    raw_text = .data$topic,
    clean_text = clean_topic_text(.data$topic),
    engagement_score = NA_real_,
    subreddit = NA_character_,
    url = .data$link
  )

# ---- CLEAN REDDIT ----
reddit_df <- reddit_raw |>
  mutate(date = .data$snapshot_date) |>
  transmute(
    date,
    source = .data$source,
    platform = .data$platform,
    raw_text = .data$title,
    clean_text = clean_topic_text(.data$title),
    engagement_score = 1,
    subreddit = .data$subreddit,
    url = .data$link
  )

clean_topics <- bind_rows(google_df, reddit_df) |>
  filter(!is.na(clean_text), clean_text != "") |>
  mutate(topic_key = clean_text)

out_file <- file.path(
  "data", "processed",
  paste0("clean_topics_", target_date, ".csv")
)

write_csv(clean_topics, out_file)

message("Saved cleaned topics to: ", out_file)
print(clean_topics)