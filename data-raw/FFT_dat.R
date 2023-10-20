## code to prepare `filges2015_dat` dataset goes here

library(revtools)
library(dplyr)

# Setting seed
set.seed(20092023)

# Helper function
sample_ref <- function(dat, x, with_replacement = FALSE, prob_vec = rep(1/x, nrow(dat))) {
  dat[sample(NROW(dat), size = x, replace = with_replacement, prob = prob_vec),]
}

#####################
# Excluded references
#####################

exclude_ris_raw <-
  revtools::read_bibliography("data-raw/raw data/FFT_exclude.ris") |>
  suppressWarnings()

exclude_ris <-
  exclude_ris_raw |>
  select(author, eppi_id, title, abstract) |>
  as_tibble() |>
  mutate(
    eppi_id = factor(eppi_id, levels = unique(eppi_id)),
    human_code = 0 # Indicating exclusion
  )

exclude_ris_180 <- sample_ref(exclude_ris, 180)

# Revert back to ris format
tutorial_ris_dat_excl <-
  exclude_ris_raw |>
  filter(eppi_id %in% exclude_ris_180$eppi_id) |>
  mutate(
    eppi_id = factor(eppi_id, levels = exclude_ris_180$eppi_id)
  ) |>
  arrange(eppi_id) |>
  mutate(
    eppi_id = as.character(eppi_id)
  )

# Creating example ris file data
write_bibliography(tutorial_ris_dat_excl, "inst/extdata/excl_tutorial.ris", format = "ris")

#####################
# Included references
#####################
include_ris_raw <- revtools::read_bibliography("data-raw/raw data/FFT_include.ris") |>
  suppressWarnings()

include_ris <-
  include_ris_raw |>
  select(author, eppi_id, title, abstract) |>
  as_tibble() |>
  mutate(
    eppi_id = factor(eppi_id, levels = unique(eppi_id)),
    human_code = 1 # Indicating inclusion
  )

#include_ris_90 <- include_ris
#
## Revert back to ris used for vignette
#tutorial_ris_dat_incl <-
#  include_ris_raw |>
#  filter(eppi_id %in% include_ris_90$eppi_id) |>
#  mutate(
#    eppi_id = factor(eppi_id, levels = include_ris_90$eppi_id)
#  ) |>
#  arrange(eppi_id) |>
#  mutate(
#    eppi_id = as.character(eppi_id)
#  )

# Creating example ris file data
write_bibliography(include_ris_raw, "inst/extdata/incl_tutorial.ris", format = "ris")

filges2015_dat <-
  dplyr::bind_rows(exclude_ris_180, include_ris) |>
  dplyr::mutate(
    eppi_id = as.character(eppi_id),
    studyid = 1:n()
  ) |>
  dplyr::relocate(studyid, .after = eppi_id)

usethis::use_data(filges2015_dat, overwrite = TRUE)

