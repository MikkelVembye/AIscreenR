## code to prepare `internal_result_object` dataset goes here

library(AIscreenR)
library(dplyr)
library(future)

paths <-  system.file("extdata", c("word_pormpt_1.docx", "word_pormpt_2.docx"), package = "AIscreenR")
prompts <-
  purrr::map_chr(
    paths, ~ {
      readtext::readtext(.x)$text |>
        stringr::str_remove_all("\n")
    }
  )

approximate_price_gpt(
  data = filges2015_dat,
  prompt = prompts,
  studyid = studyid,
  title = title,
  abstract = abstract,
  model = c("gpt-3.5-turbo-0613", "gpt-3.5-turbo-0613", "gpt-4"),
  reps = c(10, 1, 1),
  top_p = c(1)
)

plan(multisession)

system.time(
  result_object <-
    tabscreen_gpt(
      data = filges2015_dat,
      prompt = prompts,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-3.5-turbo-0613", "gpt-3.5-turbo-0613", "gpt-4"),
      reps = c(1, 10, 1),
      rpm = c(10000, 10000, 200),
      max_tries = 12

    )
)

plan(sequential)

usethis::use_data(result_object, internal = TRUE)
