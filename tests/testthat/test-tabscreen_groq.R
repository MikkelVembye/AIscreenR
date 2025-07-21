# Updated test for tabscreen_groq functions

test_that(".groq_engine return errors correctly", {

  tools_choice_name <- list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_simple"
    )
  )

  # Specifying unknown model
  body <- list(
    model = "llama-7",
    messages = list(list(
      role = "user",
      content = question
    )),
    tools = tools_simple,
    tool_choice = tools_choice_name,
    top_p = 1
  )

  res <- .groq_engine(
    body = body,
    RPM = 10000,
    time_inf = T,
    token_inf = T,
    api_key = get_api_key_groq(),
    max_t = 4,
    max_s = 10,
    is_trans = groq_is_transient,
    back = NULL,
    aft = NULL
  )


  expect_equal(ncol(res), 5)
  expect_true(stringr::str_detect(res$decision_gpt, "404"))

  # Specifying wrong api key
  body <- list(
    model = "llama3-8b-8192",
    messages = list(list(
      role = "user",
      content = question
    )),
    tools = tools_simple,
    tool_choice = tools_choice_name,
    top_p = 1
  )

  res <- .groq_engine(
    body = body,
    RPM = 10000,
    time_inf = T,
    token_inf = T,
    api_key = 1234,
    max_t = 4,
    max_s = 10,
    is_trans = groq_is_transient,
    back = NULL,
    aft = NULL
  )

  expect_equal(ncol(res), 5)
  expect_true(stringr::str_detect(res$decision_gpt, "401"))

  # Specifying ineligible role
  body <- list(
    model = "llama3-8b-8192",
    messages = list(list(
      role = "use",
      content = question
    )),
    tools = tools_simple,
    tool_choice = tools_choice_name,
    top_p = 1
  )

  res <- .groq_engine(
    body = body,
    RPM = 10000,
    time_inf = T,
    token_inf = T,
    api_key = get_api_key_groq(),
    max_t = 4,
    max_s = 10,
    is_trans = groq_is_transient,
    back = NULL,
    aft = NULL
  )

  expect_equal(ncol(res), 5)
  expect_true(stringr::str_detect(res$decision_gpt, "400"))

  # Specifying ineligible role
  body <- list(
    model = "llama3-8b-8192",
    messages = list(list(
      role = "use",
      content = question
    )),
    tools = tools_simple,
    tool_choice = tools_choice_name,
    top_p = 1
  )

  # Transient is correctly add to function
  .groq_engine(
    body = body,
    RPM = 10000,
    time_inf = T,
    token_inf = T,
    api_key = get_api_key_groq(),
    max_t = 4,
    max_s = 10,
    is_trans = groq_is_transient(),
    back = NULL,
    aft = NULL
  ) |>
    expect_error()

})


test_that(".rep_groq_engine controls errrors correctly", {

  iterations <- 3

  # Ineligible role
  res <- .rep_groq_engine(
    question = question,
    model_gpt = "llama3-8b-8192",
    topp = 1,
    role_gpt = "use",
    tool = tools_simple,
    t_choice = "inclusion_decision_simple",
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    token_inf = T,
    api_key = get_api_key_groq(),
    max_t = 4,
    max_s = 10,
    is_trans = groq_is_transient,
    ba = NULL,
    af = NULL
  )

  expect_equal(ncol(res), 6)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "400")))

  # Ineligible api key
  res <- .rep_groq_engine(
    question = question,
    model_gpt = "llama3-8b-8192",
    topp = 1,
    role_gpt = "user",
    tool = tools_simple,
    t_choice = "inclusion_decision_simple",
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    token_inf = T,
    api_key = 1,
    max_t = 4,
    max_s = 10,
    is_trans = groq_is_transient,
    ba = NULL,
    af = NULL
  )

  expect_equal(ncol(res), 6)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "401")))

  # Ineligible model
  res <- .rep_groq_engine(
    question = question,
    model_gpt = "llama-min",
    topp = 1,
    role_gpt = "user",
    tool = tools_simple,
    t_choice = "inclusion_decision_simple",
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    token_inf = T,
    api_key = get_api_key_groq(),
    max_t = 4,
    max_s = 10,
    is_trans = groq_is_transient,
    ba = NULL,
    af = NULL
  )

  expect_equal(ncol(res), 6)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(any(stringr::str_detect(res$decision_gpt, "404|400")))

})

# Old tests (tabscreen_gpt.original)
# The following tests are adapted for tabscreen_groq
paths <- system.file("extdata", "test_prompts.rds", package = "AIscreenR")
prompts <- readRDS(file = paths)

#prompts <-
#  purrr::map_chr(
#    paths, ~ {
#      readtext::readtext(.x)$text |>
#        stringr::str_remove_all("\n")
#    }
#  )

prompt <- prompts[1]
prompt2 <- prompts[2]


models <- c("llama3-8b-8192", "gemma-7b-it")
reps <- c(2, 1)
rpm <- c(30, 30)

skip <- TRUE
skip_github_action <- FALSE

future::plan(future::multisession)

test_that("tabscreen_groq() works with single parameter values.",{

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      max_tries = 2
    )

  )

  expect_true(is.numeric(test_obj$answer_data$decision_binary))

  expect_equal(nrow(test_obj$answer_data), 1L)
  expect_null(test_obj$answer_data_aggregated)
  expect_null(test_obj$price_data)
  expect_null(test_obj$price_dollar)


  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("llama3-8b-8192", "gemma-7b-it"),
      reps = 1
    )

  )

  expect_equal(nrow(test_obj$answer_data), 2L)
  expect_null(test_obj$answer_data_aggregated)
  expect_null(test_obj$price_data)
  expect_null(test_obj$price_dollar)

  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = c(prompt, prompt2),
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("llama3-8b-8192"),
      reps = 1
    )

  )

  expect_equal(nrow(test_obj$answer_data), 2L)
  expect_null(test_obj$answer_data_aggregated)
  expect_null(test_obj$price_data)
  expect_null(test_obj$price_dollar)



})

test_that("tabscreen_groq() works with multiple parameter values.",{

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("llama3-8b-8192", "gemma-7b-it"),
      reps = c(2, 1)
    )

  )

  expect_equal(nrow(test_obj$answer_data), 3L)
  expect_equal(nrow(test_obj$answer_data_aggregated), 2L)
  expect_null(test_obj$price_data)
  expect_null(test_obj$price_dollar)

  expect_gt(nrow(test_obj$answer_data), nrow(test_obj$answer_data_aggregated))

  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1:2,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("llama3-8b-8192", "gemma-7b-it"),
      reps = c(2, 1)
    )

  )

  expect_equal(nrow(test_obj$answer_data), 6L)
  expect_equal(nrow(test_obj$answer_data_aggregated), 4L)
  expect_null(test_obj$price_data)
  expect_null(test_obj$price_dollar)


  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1:2,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("llama3-8b-8192", "gemma-7b-it"),
      reps = c(2, 1),
      top_p = c(0.2, 1)
    )

  )

  expect_equal(nrow(test_obj$answer_data), 12L)
  expect_equal(nrow(test_obj$answer_data_aggregated), 2*2*2)
  expect_null(test_obj$price_data)
  expect_null(test_obj$price_dollar)

})

test_that("tabscreen_groq() don't return time and token info.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      time_info = FALSE,
      token_info = FALSE
    )

  )

  time_token_not_include <- any(!stringr::str_detect(names(test_obj$answer_data), "time|token"))
  expect_true(time_token_not_include)

  expect_equal(length(test_obj), 2)

  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 2,
      api_key = 1234,
      time_info = FALSE,
      token_info = FALSE
    )

  ) |>
    suppressMessages()

  expect_equal(length(test_obj), 3)

  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      time_info = TRUE,
      token_info = FALSE
    )

  )

  token_not_include <- any(!stringr::str_detect(names(test_obj$answer_data), "token"))
  expect_true(token_not_include)

  time_include <- any(stringr::str_detect(names(test_obj$answer_data), "time"))
  expect_true(time_include)

  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      time_info = FALSE,
      token_info = TRUE
    )

  )

  time_not_include <- any(!stringr::str_detect(names(test_obj$answer_data), "time"))
  expect_true(time_not_include)

  token_include <- any(stringr::str_detect(names(test_obj$answer_data), "token"))
  expect_true(token_include)

})

test_that("tabscreen_groq() works with detailed functions and ... .", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1:2,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      decision_description = TRUE
    )

  ) |>
    suppressMessages()


  expect_true(is.character(test_obj$answer_data$detailed_description))
  expect_null(test_obj$answer_data_aggregated)

  complete_tokens <- mean(test_obj$answer_data$completion_tokens, na.rm = TRUE)
  expect_gt(complete_tokens, 13)


  # Working with max_tokens, i.e. ... calls
  expect_error(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      decision_description = TRUE,
      max_tokens = 40
    )

  )

  # Working with temperature, i.e. ... calls

  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 3,
      decision_description = TRUE,
      progress = FALSE,
      temperature = 0.001
    )

  ) |>
    suppressMessages()

  n_distinct_detailed_answers <- test_obj$answer_data$detailed_description |> dplyr::n_distinct()

  expect_lt(n_distinct_detailed_answers, 3)

})

test_that("Message behavior.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_no_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      messages = FALSE,
      progress = FALSE
    )

  )

  expect_no_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      messages = FALSE,
      progress = FALSE
    )

  )
  expect_no_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gemma-7b-it",
      reps = 2
    )
  )
  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      decision_description = TRUE
    )
  ) |>
    suppressMessages()
  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gemma-7b-it",
      reps = 1,
      decision_description = TRUE
    ),

    paste0(
      "* Be aware that getting descriptive, detailed responses will substantially increase",
      " the prize of the screening relative to the noted approximate prize."
    )

  ) |>
    suppressMessages()

  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[148,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract
    ),

    paste0(
      "* Consider removing references without abstracts ",
      "since these can distort the accuracy of the screening."
    )

  ) |>
    suppressMessages()

})

test_that("tabscreen_groq() expected errors.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_error(

    test_obj <- tabscreen_groq(
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

    test_obj <- tabscreen_groq(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = models,
      reps = c(10, 1),
      rpm = c(30, 30, 30)
    )

  )

  expect_error(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("llama3-8b-8192-x", "gemma-7b-it"),
      reps = reps,
      rpm = rpm
    )

  )

  # With detailed description functions

  expect_error(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = models,
      reps = c(10, 1, 1),
      decision_description = TRUE
    )

  )

  expect_error(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = models,
      reps = c(10, 1),
      rpm = c(30, 30, 30),
      decision_description = TRUE
    )

  )

  expect_error(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("llama3-8b-8192-x", "gemma-7b-it"),
      reps = reps,
      rpm = rpm,
      decision_description = TRUE
    )

  )

  expect_error(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      incl_cutoff_upper = 0.4,
      incl_cutoff_lower = 0.5
    )

  )

})

test_that("API error.",{

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_groq(
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

  error_text <- unique(test_obj$answer_data$decision_gpt)
  expect_true(stringr::str_detect(error_text, "401"))

  expect_no_message(

    test_obj <- tabscreen_groq(
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

  error_text <- unique(test_obj$answer_data$decision_gpt)
  expect_true(stringr::str_detect(error_text, "401"))

  expect_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      api_key = 1234,
      progress = FALSE,
      decision_description = TRUE
    )

  ) |>
    suppressMessages()

  error_text <- unique(test_obj$answer_data$decision_gpt)
  expect_true(stringr::str_detect(error_text, "401"))

  expect_no_message(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      api_key = 1234,
      messages = FALSE,
      progress = FALSE,
      decision_description = TRUE
    )

  )

  error_text <- unique(test_obj$answer_data$decision_gpt)
  expect_true(stringr::str_detect(error_text, "401"))


})

future::plan(future::sequential)

# Test parallel

test_that("That parallel processing works.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  future::plan(future::multisession, workers = future::availableCores())

  expect_message(

    tm_par <- system.time(
      test_obj <- tabscreen_groq(
        data = filges2015_dat[1:2,],
        prompt = prompt,
        title = title,
        abstract = abstract,
        model = "llama3-8b-8192",
        reps = 10
      )
    )
  )

  future::plan(future::sequential)

  expect_lt(tm_par[["elapsed"]], sum(test_obj$answer_data$run_time))

  groq_answers <-
    test_obj$answer_data |>
    filter(!is.na(decision_gpt)) |>
    pull(decision_gpt)

  # Testing function call work
  groq_answers |>
    stringr::str_detect("1|0|rror") |>
    unique() |>
    expect_true()



})

test_that("max_tokens < 9 work",{

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_error(

    test_obj <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      max_tokens = 8
    )

  )

})

test_that("Print and class expectation for groq object.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    random_name <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      max_tries = 2
    )

  )

  # class check
  expect_s3_class(random_name, "groq")
  expect_s3_class(random_name, "list")

  expect_output(print(random_name), "random_name")

  expect_output(print.groq(random_name), "random_name\\$answer_data")
  expect_false("answer_data_aggregated" %in% names(random_name))
  expect_false("price_dollar" %in% names(random_name))

  print_out1 <- paste0(
    "Find data with all answers by executing\\n random_name\\$answer_data"
  )
  expect_output(print(random_name), print_out1)

  expect_message(

    random_name <- tabscreen_groq(
      data = filges2015_dat[c(1:2),],
      prompt = c(prompt),
      studyid = studyid, # indicate the variable with the studyid in the data
      title = title, # indicate the variable with the titles in the data
      abstract = abstract,
      api_key = 1234
    )
  ) |>
    suppressMessages()

  expect_identical(names(random_name$answer_data), names(random_name$error_data))

  expect_output(print(random_name), "random_name\\$answer_data")
  expect_output(print(random_name), "random_name\\$error_data")

  print_out2 <- paste0(
    "Find data with all answers by executing\\n random_name\\$answer_data"
  )

  expect_output(print(random_name), print_out2)

  #############
  expect_message(

    random_name <- tabscreen_groq(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      token_info = FALSE
    )

  )

  # class check
  expect_s3_class(random_name, "groq")
  expect_s3_class(random_name, "list")

  expect_output(print(random_name), "random_name")

  expect_output(print.groq(random_name), "random_name\\$answer_data")

  expect_false("price_dollar" %in% names(random_name))

  expect_message(

    random_name <- tabscreen_groq(
      data = filges2015_dat[c(1:2),],
      prompt = c(prompt),
      studyid = studyid, # indicate the variable with the studyid in the data
      title = title, # indicate the variable with the titles in the data
      abstract = abstract,
      api_key = 1234,
      token_info = FALSE
    )
  ) |>
    suppressMessages()


  expect_output(print(random_name), "random_name\\$answer_data")
  expect_output(print(random_name), "random_name\\$error_data")

  expect_false("price_dollar" %in% names(random_name))


})

