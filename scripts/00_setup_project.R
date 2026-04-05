# scripts/00_setup_project.R

dirs <- c(
  "data",
  "data/raw",
  "data/raw/google",
  "data/raw/reddit",
  "data/processed",
  "scripts",
  "config"
)

for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

message("Project folders created.")