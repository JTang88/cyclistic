load_or_install <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

# List of packages. 
packages <- c(
  "naniar", "patchwork", "tidyverse", "lubridate",
  "skimr", "sf", "ggplot2", "ggmap", "gridExtra", "glue", "scales"
)

# Load or install packages
lapply(packages, load_or_install)

# load utility functions
source("scripts/utilities.R")

