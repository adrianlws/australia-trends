# scripts/05_build_story_leads.R

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(janitor)

target_date <- as.Date(Sys.time(), tz = "Australia/Sydney") - 1

clean_file <- file.path(
  "data", "processed",
  paste0("clean_topics_", target_date, ".csv")
)

if (!file.exists(clean_file)) stop("Missing cleaned topics file: ", clean_file)

topics <- read_csv(clean_file, show_col_types = FALSE)

story_leads <- topics |>
  group_by(date, topic_key) |>
  summarise(
    google_present = any(platform == "google"),
    reddit_present = any(platform == "reddit"),
    reddit_mentions = sum(platform == "reddit", na.rm = TRUE),
    reddit_engagement = sum(ifelse(platform == "reddit", engagement_score, 0), na.rm = TRUE),
    sample_google_topic = dplyr::first(raw_text[platform == "google"], default = NA_character_),
    sample_reddit_topic = dplyr::first(raw_text[platform == "reddit"], default = NA_character_),
    .groups = "drop"
  ) |>
  mutate(
    cross_platform_bonus = ifelse(google_present & reddit_present, 20, 0),
    story_score = cross_platform_bonus + reddit_mentions * 5 + reddit_engagement * 0.05,
    trend_type = case_when(
      google_present & reddit_present ~ "cross_platform",
      google_present & !reddit_present ~ "search_spike",
      !google_present & reddit_present ~ "reddit_heavy",
      TRUE ~ "other"
    ),
    display_label = coalesce(sample_google_topic, sample_reddit_topic, topic_key)
  ) |>
  arrange(desc(story_score), display_label)

out_file <- file.path(
  "data", "processed",
  paste0("story_leads_", target_date, ".csv")
)

write_csv(story_leads, out_file)

message("Saved story leads to: ", out_file)
print(story_leads)