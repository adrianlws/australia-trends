# scripts/01_install_packages.R

packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "purrr",
  "lubridate",
  "jsonlite",
  "httr2",
  "tidyr",
  "janitor",
  "here"
)

installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)

if (length(to_install) > 0) {
  install.packages(to_install)
}

message("Packages ready.")