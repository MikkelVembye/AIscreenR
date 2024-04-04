## code to prepare `internal_result_object` datasets goes here

library(AIscreenR)
library(dplyr)
library(future)

#paths <-  system.file("extdata", c("word_pormpt_1.docx", "word_pormpt_2.docx"), package = "AIscreenR")
#prompts <-
#  purrr::map_chr(
#    paths, ~ {
#      readtext::readtext(.x)$text |>
#        stringr::str_remove_all("\n")
#    }
#  )
#
#dat1 <- filges2015_dat[c(1:90, 181:225),]
#dat2 <- filges2015_dat[c(91:180, 226:270),]
#
#approximate_price_gpt(
#  data = dat1,
#  prompt = prompts,
#  studyid = studyid,
#  title = title,
#  abstract = abstract,
#  model = c("gpt-3.5-turbo-0613", "gpt-3.5-turbo-0613", "gpt-4"),
#  reps = c(10, 1, 1),
#  top_p = c(1)
#)
#
#plan(multisession)
#
#system.time(
#  res_object1 <-
#    tabscreen_gpt(
#      data = dat1,
#      prompt = prompts,
#      studyid = studyid,
#      title = title,
#      abstract = abstract,
#      model = c("gpt-3.5-turbo-0613", "gpt-3.5-turbo-0613", "gpt-4"),
#      reps = c(1, 10, 1),
#      rpm = c(10000, 10000, 200),
#      max_tries = 11,
#      top_p = c(0.001, 1)
#    )
#)
#
#result_object <- res_object1
#result_object_no_err <- result_object |> screen_errors()
#
#result_object$
#
#
#system.time(
#  res_object2 <-
#    tabscreen_gpt(
#      data = dat2,
#      prompt = prompts,
#      studyid = studyid,
#      title = title,
#      abstract = abstract,
#      model = c("gpt-3.5-turbo-0613", "gpt-3.5-turbo-0613", "gpt-4"),
#      reps = c(1, 10, 1),
#      rpm = c(10000, 10000, 200),
#      top_p = c(0.001, 1)
#    )
#)
#
#result_object2 <- res_object2
#result_object2_no_err <- result_object2 |> screen_errors()
#plan(sequential)

# Model and prices info

deprecated_models <-
  tibble::tibble(
    model = c("gpt-4-0314", "gpt-4-32k-0314"),
    input = c(0.03/1000, 0.06/1000),
    output = c(0.06/1000, 0.12/1000)
  )

# Find information aboth these here:
# https://platform.openai.com/docs/models

# These models names could't be retrieved with the price_table function. They are
# therefore added manually.

further_models <-
  tibble::tibble(
    model = c("gpt-4-0613", "gpt-4-32k-0613", "gpt-3.5-turbo-16k", "gpt-3.5-turbo"),
    input = c(0.03/1000, 0.06/1000, 3/1e6, 0.5/1e6),
    output = c(0.06/1000, 0.12/1000, 4/1e6, 1.5/1e6)
  )

models_and_prices_040424 <-
  AIscreenR:::price_table() |>
  dplyr::bind_rows(further_models) |>
  dplyr::bind_rows(deprecated_models) |>
  dplyr::filter(!stringr::str_detect(model, "instruct"))

result_object <- AIscreenR:::result_object
result_object_no_err <- AIscreenR:::result_object_no_err
result_object2_no_err <- AIscreenR:::result_object2_no_err

usethis::use_data(models_and_prices_040424, result_object, result_object_no_err, result_object2_no_err, overwrite = TRUE, internal = TRUE)
