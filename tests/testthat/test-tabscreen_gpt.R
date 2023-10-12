paths <- system.file("extdata", c("word_pormpt_1.docx", "word_pormpt_2.docx"), package = "AIscreenR")

prompts <-
  purrr::map_chr(
    paths, ~ {
      readtext::readtext(.x)$text |>
        stringr::str_remove_all("\n")
    }
  )

prompt <- prompts[1]
prompt2 <- prompts[2]

models <- c("gpt-3.5-turbo-0613", "gpt-4")
reps <- c(10, 1)
rpm <- c(3500, 200)

skip <- TRUE
skip_github_action <- FALSE

future::plan(future::multisession)

test_that("tabscreen_gpt() works with single parameter values.",{

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      title = title,
      abstract = abstract,
      model = "gpt-3.5-turbo-0613",
      reps = 1,
      max_tries = 0
    )

  )

  expect_true(is.numeric(test_obj$answer_data_all$decision_binary))

  expect_equal(nrow(test_obj$answer_data_all), 1L)
  expect_equal(nrow(test_obj$answer_data_sum), 1L)
  expect_equal(nrow(test_obj$price_data), 1L)
  expect_length(test_obj$price_dollar, 1L)


  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-3.5-turbo-0613", "gpt-4"),
      reps = 1
    )

  )

  expect_equal(nrow(test_obj$answer_data_all), 2L)
  expect_equal(nrow(test_obj$answer_data_sum), 2L)
  expect_equal(nrow(test_obj$price_data), 2L)
  expect_length(test_obj$price_dollar, 1L)

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = c(prompt, prompt2),
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-3.5-turbo-0613"),
      reps = 1
    )

  )

  expect_equal(nrow(test_obj$answer_data_all), 2L)
  expect_equal(nrow(test_obj$answer_data_sum), 2L)
  expect_equal(nrow(test_obj$price_data), 1L)
  expect_length(test_obj$price_dollar, 1L)



})

test_that("tabscreen_gpt() works with multiple parameter values.",{

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-3.5-turbo-0613", "gpt-4"),
      reps = c(2, 1)
    )

  )

  expect_equal(nrow(test_obj$answer_data_all), 3L)
  expect_equal(nrow(test_obj$answer_data_sum), 2L)
  expect_equal(nrow(test_obj$price_data), 2L)
  expect_length(test_obj$price_dollar, 1L)

  expect_gt(nrow(test_obj$answer_data_all), nrow(test_obj$answer_data_sum))

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1:2,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-3.5-turbo-0613", "gpt-4"),
      reps = c(2, 1)
    )

  )

  expect_equal(nrow(test_obj$answer_data_all), 6L)
  expect_equal(nrow(test_obj$answer_data_sum), 4L)
  expect_equal(nrow(test_obj$price_data), 2L)
  expect_length(test_obj$price_dollar, 1L)


  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1:2,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-3.5-turbo-0613", "gpt-4"),
      reps = c(2, 1),
      top_p = c(0.2, 1)
    )

  )

  expect_equal(nrow(test_obj$answer_data_all), 12L)
  expect_equal(nrow(test_obj$answer_data_sum), 2*2*2)
  expect_equal(nrow(test_obj$price_data), 2L)
  expect_length(test_obj$price_dollar, 1L)

})

test_that("tabscreen_gpt() don't return time and token info.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-3.5-turbo-0613",
      reps = 1,
      time_info = FALSE,
      token_info = FALSE
    )

  )

  time_token_not_include <- any(!stringr::str_detect(names(test_obj$answer_data_all), "time|token"))
  expect_true(time_token_not_include)

  expect_equal(length(test_obj), 2)

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-3.5-turbo-0613",
      reps = 2,
      api_key = 1234,
      time_info = FALSE,
      token_info = FALSE
    )

  ) |>
    suppressMessages()

  expect_equal(length(test_obj), 3)

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-3.5-turbo-0613",
      reps = 1,
      time_info = TRUE,
      token_info = FALSE
    )

  )

  token_not_include <- any(!stringr::str_detect(names(test_obj$answer_data_all), "token"))
  expect_true(token_not_include)

  time_include <- any(stringr::str_detect(names(test_obj$answer_data_all), "time"))
  expect_true(time_include)

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-3.5-turbo-0613",
      reps = 1,
      time_info = FALSE,
      token_info = TRUE
    )

  )

  time_not_include <- any(!stringr::str_detect(names(test_obj$answer_data_all), "time"))
  expect_true(time_not_include)

  token_include <- any(stringr::str_detect(names(test_obj$answer_data_all), "token"))
  expect_true(token_include)

})

test_that("tabscreen_gpt() works with detailed fucntions and ... .", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1:2,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-3.5-turbo",
      reps = 1,
      functions = AIscreenR:::incl_function,
      function_call_name = list(name = "inclusion_decision")
    )

  ) |>
    suppressMessages()


  expect_true(is.character(test_obj$answer_data_all$detailed_description))
  expect_identical(nrow(test_obj$answer_data_all), nrow(test_obj$answer_data_sum))

  complete_tokens <- mean(test_obj$answer_data_all$completion_tokens, na.rm = TRUE)
  expect_gt(complete_tokens, 13)


  # Working with max_tokens, i.e. ... calls
  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-3.5-turbo",
      reps = 1,
      functions = AIscreenR:::incl_function,
      function_call_name = list(name = "inclusion_decision"),
      max_tokens = 40
    )

  ) |>
    suppressMessages()

  complete_tokens <- test_obj$answer_data_all$completion_tokens
  expect_lt(complete_tokens, 41)

  # Working with temperature, i.e. ... calls

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-3.5-turbo",
      reps = 3,
      functions = AIscreenR:::incl_function,
      function_call_name = list(name = "inclusion_decision"),
      progress = FALSE,
      temperature = 0.001
    )

  ) |>
    suppressMessages()

  n_distinct_detailed_answers <- test_obj$answer_data_all$detailed_description |> dplyr::n_distinct()

  expect_lt(n_distinct_detailed_answers, 3)

})

test_that("Message behavior.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-4",
      reps = 1,
      messages = FALSE,
      progress = FALSE
    )

  )

  expect_no_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-3.5-turbo",
      reps = 1,
      messages = FALSE,
      progress = FALSE
    )

  )
  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-4",
      reps = 2
    ),

    "* Consider to reduce reps to 1 for gpt-4 models."

  )
  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-3.5-turbo",
      reps = 1,
      functions = AIscreenR:::incl_function,
      function_call_name = list(name = "inclusion_decision")
    )
  ) |>
    suppressMessages()
  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-4",
      reps = 1,
      functions = AIscreenR:::incl_function,
      function_call_name = list(name = "inclusion_decision")
    ),

    paste0(
      "\n* Be aware that getting detailed reponses from ChatGPT ",
      "will substantially increase the prize of the screening relative to the noted approximate prize."
    )

  ) |>
    suppressMessages()

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[148,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract
    ),

    paste0(
      "* Consider removing references that has no abstract ",
      "since these can distort the accuracy of the screening"
    )

  ) |>
    suppressMessages()

})

test_that("tabscreen_gpt() expected errors.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_error(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = models,
      reps = c(10, 1, 1)
    )

  )

  expect_error(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = models,
      reps = c(10, 1),
      rpm = c(3500, 200, 200)
    )

  )

  expect_error(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-3.5turbo-0613", "gpt-4"),
      reps = reps,
      rpm = rpm
    )

  )

  # With detaled description functions

  expect_error(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = models,
      reps = c(10, 1, 1),
      functions = AIscreenR:::incl_function,
      function_call_name = list(name = "inclusion_decision")
    )

  )

  expect_error(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = models,
      reps = c(10, 1),
      rpm = c(3500, 200, 200),
      functions = AIscreenR:::incl_function,
      function_call_name = list(name = "inclusion_decision")
    )

  )

  expect_error(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-3.5turbo-0613", "gpt-4"),
      reps = reps,
      rpm = rpm,
      functions = AIscreenR:::incl_function,
      function_call_name = list(name = "inclusion_decision")
    )

  )

})

test_that("API error.",{

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      api_key = 1234,
      progress = FALSE
    )

  ) |>
    suppressMessages()

  error_text <- unique(test_obj$answer_data_all$decision_gpt)
  expect_identical(error_text, "Error 401 Unauthorized [invalid api]")

  expect_no_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      api_key = 1234,
      messages = FALSE,
      progress = FALSE
    )

  )

  error_text <- unique(test_obj$answer_data_all$decision_gpt)
  expect_identical(error_text, "Error 401 Unauthorized [invalid api]")

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      api_key = 1234,
      progress = FALSE,
      functions = AIscreenR:::incl_function,
      function_call_name = list(name = "inclusion_decision")
    )

  ) |>
    suppressMessages()

  error_text <- unique(test_obj$answer_data_all$decision_gpt)
  expect_identical(error_text, "Error 401 Unauthorized [invalid api]")

  expect_no_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      api_key = 1234,
      messages = FALSE,
      progress = FALSE,
      functions = AIscreenR:::incl_function,
      function_call_name = list(name = "inclusion_decision")
    )

  )

  error_text <- unique(test_obj$answer_data_all$decision_gpt)
  expect_identical(error_text, "Error 401 Unauthorized [invalid api]")


})

future::plan(future::sequential)

# Test parallel

test_that("That paralell processing works.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  future::plan(future::multisession, workers = future::availableCores())

  expect_message(

    tm_par <- system.time(
      test_obj <- tabscreen_gpt(
        data = filges2015_dat[1:2,],
        prompt = prompt,
        title = title,
        abstract = abstract,
        model = "gpt-3.5-turbo-0613",
        reps = 10
      )
    )
  )

  future::plan(future::sequential)

  expect_lt(tm_par[["elapsed"]], sum(test_obj$answer_data_all$run_time))

  gpt_answers <-
    test_obj$answer_data_all |>
    filter(!is.na(decision_gpt)) |>
    pull(decision_gpt)

  # Testing function call work
  gpt_answers |>
    stringr::str_detect("1|0|rror") |>
    unique() |>
    expect_true()



})

test_that("max_tokens < 11 work",{

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-3.5-turbo",
      reps = 1,
      max_tokens = 9
    )

  ) |>
    suppressMessages()

  expect_false(is.na(test_obj$answer_data_all$decision_gpt))

})

test_that("Print and class expectation for chatgpt object.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    random_name <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-3.5-turbo-0613",
      reps = 1,
      max_tries = 0
    )

  )

  # class check
  expect_s3_class(random_name, "chatgpt")
  expect_s3_class(random_name, "list")

  expect_output(print(random_name), "random_name")

  expect_output(print.chatgpt(random_name), "random_name\\$answer_data_all")
  expect_output(print.chatgpt(random_name), "random_name\\$answer_data_sum")
  expect_output(print.chatgpt(random_name), "random_name\\$price_dollor")

  print_out1 <- paste0(
    "Find data with all answers by executing\\n random_name\\$answer_data_all\\n\\nFind ",
    "data with the result aggregated across multiple answers by executing\\n ",
    "random_name\\$answer_data_sum\\n\\nFind total price for the screening by executing\\n random_name\\$price_dollor"
  )
  expect_output(print(random_name), print_out1)

  expect_message(

    random_name <- tabscreen_gpt(
      data = filges2015_dat[c(1:2),],
      prompt = c(prompt),
      studyid = studyid, # indicate the variable with the studyid in the data
      title = title, # indicate the variable with the titles in the data
      abstract = abstract,
      api_key = 1234
    )
  ) |>
    suppressMessages()


  expect_output(print(random_name), "random_name\\$answer_data_all")
  expect_output(print(random_name), "random_name\\$answer_data_sum")
  expect_output(print(random_name), "random_name\\$price_dollor")
  expect_output(print(random_name), "random_name\\$error_data")

  print_out2 <- paste0(
    "Find data with all answers by executing\\n random_name\\$answer_data_all\\n\\nFind ",
    "data with the result aggregated across multiple answers by executing\\n ",
    "random_name\\$answer_data_sum\\n\\nFind total price for the screening by executing\\n random_name\\$price_dollor"
  )

  expect_output(print(random_name), print_out2)

  output2 <- capture.output(random_name)

  expect_length(output2, 23)
  expect_equal(output2[20], "value$price_dollor")

})


rm(list=ls())

