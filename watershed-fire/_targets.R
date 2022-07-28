# Load packages required to define the pipeline
library(targets)
suppressPackageStartupMessages(library(tidyverse))

# Set target options
tar_option_set(
  packages = c("rgeos", "rgdal", "tidyverse", "sf", "USAboundaries", "ggshadow", "ggnewscale", "terra", "tidyterra", "gganimate", "transformr", "maptiles", "magick", "cowplot"), # packages that your targets need to run
  format = "rds" # default storage format
)
options(tidyverse.quiet = TRUE)

# Load function scripts
source("1_fetch.R")
source("2_process.R")
source("3_visualize.R")

# Define parameters
year_range <- c(1984, 2020)
sf::sf_use_s2(FALSE)
crs <- 9311
conus <- state.abb %>%
  subset(!. %in% c("AK", "HI"))

# Return list of targets
c(p1_targets, p2_targets, p3_targets)