## code to prepare `internal_result_object` dataset goes here

library(AIscreenR)
library(dplyr)
library(future)

path <-  system.file("extdata", "word_pormpt_1.docx", package = "AIscreenR")
prompt <-
  readtext::readtext(path)$text |>
  stringr::str_remove_all("\n")

plan(multisession)

system.time(
  result_object <-
    tabscreen_gpt(
      data = filges2015_dat,
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-3.5-turbo-0613", "gpt-4"),
      reps = c(10, 1), # Number of times the same question is asked to ChatGPT
      rpm = c(3500, 200),
      max_tries = 6

    )
)

plan(sequential)

usethis::use_data(result_object, overwrite = TRUE, internal = TRUE)
