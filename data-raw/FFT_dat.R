## code to prepare `FFT_dat` dataset goes here

library(revtools)
library(dplyr)

# Setting seed
set.seed(20092023)

# Helper function
sample_ref <- function(dat, x, with_replacement = FALSE, prob_vec = rep(1/x, nrow(dat))) {
  dat[sample(NROW(dat), size = x, replace = with_replacement, prob = prob_vec),]
}

exclude_ris_raw <- revtools::read_bibliography("data-raw/raw data/FFT_exclude.ris") |>
  suppressWarnings()

exclude_ris <-
  exclude_ris_raw |>
  select(studyid = eppi_id, title, abstract) |>
  as_tibble() |>
  mutate(
    human_code = 0 # Indicating exclusion
  )

exclude_ris_100 <- sample_ref(exclude_ris, 100)

# Revert back to ris used for vignettte
#tutorial_ris_dat_excl <-
#  exclude_ris_raw |>
#  filter(eppi_id %in% exclude_ris_100$studyid)
#
#write_bibliography(tutorial_ris_dat_excl, "excl_tutorial.ris", format = "ris")

include_ris_raw <- revtools::read_bibliography("data-raw/raw data/FFT_include.ris") |>
  suppressWarnings()

include_ris <-
  include_ris_raw |>
  select(studyid = eppi_id, title, abstract) |>
  as_tibble() |>
  mutate(
    human_code = 1 # Indicating inclusion
  )

include_ris_50 <- sample_ref(include_ris, 50)

# Revert back to ris used for vignette
#tutorial_ris_dat_incl <-
#  include_ris_raw |>
#  filter(eppi_id %in% include_ris_50$studyid)
#
#write_bibliography(tutorial_ris_dat_incl, "incl_tutorial.ris", format = "ris")

FFT_dat <- dplyr::bind_rows(exclude_ris_100, include_ris_50)
usethis::use_data(FFT_dat, overwrite = TRUE)


# library(readxl)

#FFT_raw <- read_excel("data-raw/raw data/Campbell_FFT take 2.xlsx")
#
#FFT_dat <-
#  FFT_raw |>
#  dplyr::mutate(
#    studyid = 1:dplyr::n(),
#    human_code = `Human code`
#  ) |>
#  dplyr::relocate(studyid, .before = Title) |>
#  dplyr::rename(title = Title, abstract = Abstract) |>
#  dplyr::select(studyid, title:abstract, human_code)
#

