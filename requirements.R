# requirements.R
# Run this file to install and load all required packages

packages <- c(
  "sandwich",
  "ggplot2",
  "dplyr",
  "lmtest",
  "purrr"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

invisible(lapply(packages, install_if_missing))
invisible(lapply(packages, library, character.only = TRUE))

rm(list = ls())
message("All packages loaded successfully.")