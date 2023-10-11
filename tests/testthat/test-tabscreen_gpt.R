prompt <- "Evaluate the following study based on the selection criteria
for a systematic review on the effects of family-based interventions on drug abuse
reduction for young people in treatment for non-opioid drug use.
A family-based intervention (FFT) is equivalent to a behavior focused
family therapy, where young people’s drug use is understood in relation to family
behavior problems. Family-based interventions also includes manual-based family therapies as
it targets young people and their families as a system throughout treatment, and thereby recognizes
the important role of the family system in the development and treatment of young people’s drug use problems.
FFT was developed in the late 1980s on request from the US National Institute on Drug Abuse (NIDA).
The development of FFT was initially heavily inspired by the alcohol abuse program
Community Reinforcement Approach (CRA), which was aimed at restructuring the environment
to reinforce non-alcohol associated activities. FFT developed to have more emphasis on
contingency contracting, impulse control strategies specific to drug use,
and increased emphasis on involvement of family members in treatment.
FFT is designed to accommodate diverse populations of youths with a variety of behavioral,
cultural and individual preferences. FFT has evolved for use in severe behavioral disturbances
known to co-exist with substance use and dependence, and the core interventions
have been enhanced to address several mental health related problems commonly occurring
as comorbid conditions in drug use treatment participant.  For each study,
I would like you to assess:  1) Is the study about a family-based intervention,
such as Functional Family Therapy, Multidimensional Family Therapy, or
Behavioral Family Therapy? (Outpatient manual-based interventions of any
duration delivered to young people and their families). If not, exclude study.
2) Are the participants in outpatient drug treatment primarily
for non-opioid drug use? 3) Are the participants within age 11–21?"

prompt2 <- "Evaluate the following study based on the selection criteria
for a systematic review on the effects of family-based interventions on drug abuse
reduction for young people in treatment for non-opioid drug use.
A family-based intervention (FFT) is equivalent to a behavior focused
family therapy, where young people’s drug use is understood in relation to family
behavior problems. Family-based interventions also includes manual-based family therapies as
it targets young people and their families as a system throughout treatment, and thereby recognizes
the important role of the family system in the development and treatment of young people’s drug use problems.
FFT was developed in the late 1980s on request from the US National Institute on Drug Abuse (NIDA).
The development of FFT was initially heavily inspired by the alcohol abuse program
Community Reinforcement Approach (CRA), which was aimed at restructuring the environment
to reinforce non-alcohol associated activities. FFT developed to have more emphasis on
contingency contracting, impulse control strategies specific to drug use,
and increased emphasis on involvement of family members in treatment.
FFT is designed to accommodate diverse populations of youths with a variety of behavioral,
cultural and individual preferences. FFT has evolved for use in severe behavioral disturbances
known to co-exist with substance use and dependence, and the core interventions
have been enhanced to address several mental health related problems commonly occurring
as comorbid conditions in drug use treatment participant.  For each study,
I would like you to assess:  1) Is the study about a family-based intervention,
such as Functional Family Therapy, Multidimensional Family Therapy, or
Behavioral Family Therapy? (Outpatient manual-based interventions of any
duration delivered to young people and their families). If not, exclude study."

models <- c("gpt-3.5-turbo-0613", "gpt-4")
reps <- c(10, 1)
rpm <- c(3500, 200)

future::plan(future::multisession)

test_that("tabscreen_gpt() works with single parameter values.",{

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
    )

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

})

test_that("tabscreen_gpt() expected errors.", {

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



})

test_that("max_tokens < 11 work",{

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


rm(list=ls())
