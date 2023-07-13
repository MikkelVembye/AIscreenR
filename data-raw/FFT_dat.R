## code to prepare `FFT_dat` dataset goes here

library(readxl)

FFT_raw <- read_excel("data-raw/raw data/Campbell_FFT take 2.xlsx")

FFT_dat <-
  FFT_raw |>
  dplyr::mutate(
    studyid = 1:dplyr::n(),
    human_code = `Human code`
  ) |>
  dplyr::relocate(studyid, .before = Title) |>
  dplyr::rename(title = Title, abstract = Abstract) |>
  dplyr::select(studyid, title:abstract, human_code)

usethis::use_data(FFT_dat, overwrite = TRUE)
