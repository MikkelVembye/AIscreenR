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

dat1 <- filges2015_dat[c(1:90, 181:225),]
dat2 <- filges2015_dat[c(91:180, 226:270),]

approximate_price_gpt(
  data = dat1,
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
  res_object1 <-
    tabscreen_gpt(
      data = dat1[1:2,],
      prompt = prompts,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-3.5-turbo-0613", "gpt-3.5-turbo-0613", "gpt-4"),
      reps = c(1, 10, 1),
      rpm = c(10000, 10000, 200),
      max_tries = 11
    )
)

plan(sequential)

usethis::use_data(result_object, overwrite = TRUE, internal = TRUE)
