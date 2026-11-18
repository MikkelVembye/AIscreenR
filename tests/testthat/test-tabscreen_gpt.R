# Updated test for tabscreen_gpt.default functions

# Fake OpenAI replies, so these tests never reach the real API.
# httr2::local_mocked_responses() answers every req_perform() with this function.
mock_openai_errors <- function(req) {
  # Unknown models give 404, a tool with an invalid schema gives 400,
  # and any other request gets 401 (wrong key)
  if (req$body$data$model %in% c("gpt-7", "gpt-5-nan")) {
    httr2::response_json(
      status_code = 404,
      body = list(error = list(message = "The model does not exist"))
    )
  } else if (identical(req$body$data$tools[[1]]$parameters$type, "invalid_schema_type")) {
    httr2::response_json(
      status_code = 400,
      body = list(error = list(message = "Invalid schema for function: invalid type"))
    )
  } else {
    httr2::response_json(
      status_code = 401,
      body = list(error = list(message = "Incorrect API key provided"))
    )
  }
}

# A valid reply with one function call, as the Responses API returns it.
# The detailed tools also need a detailed_description.
mock_openai_ok <- function(req) {
  tool_name <- req$body$data$tools[[1]]$name
  detailed <- tool_name %in% c("inclusion_decision", "inclusion_decision_binary")
  args <- if (detailed) {
    '{"decision_gpt":"1","detailed_description":"Mocked answer"}'
  } else {
    '{"decision_gpt":"1"}'
  }

  httr2::response_json(
    status_code = 200,
    body = list(
      model = req$body$data$model, # like the real API, echo the requested model
      output = list(list(type = "function_call", arguments = args)),
      usage = list(input_tokens = 10, output_tokens = 5)
    )
  )
}

# Everything a screening test needs to run without any real API call:
# mocked replies, a dummy key and a sequential plan (mocks do not reach
# parallel workers). All of it is undone when the test ends.
local_gpt_mock <- function(mock = mock_openai_ok, env = parent.frame()) {
  local_mock_api(mock, env = env)
  withr::local_envvar(OPENAI_API_KEY = "dummy-key", .local_envir = env)
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan), envir = env)
}

test_that(".gpt_engine_responses return errors correctly", {

  local_mock_api(mock_openai_errors)

  tools_choice_name <- list(
    type = "function",
    name = "inclusion_decision_simple"
  )

  # Specifying unknown model
  body <- list(
    model = "gpt-7",
    input = question,
    tools = tools_simple,
    tool_choice = tools_choice_name
  )

  res <- .gpt_engine_responses(
    body = body,
    RPM = 10000,
    timeinf = T,
    tokeninf = T,
    key = "dummy-key", # not a real key, the request is mocked
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient,
    back = NULL,
    aft = NULL,
    endpoint_url = "https://api.openai.com/v1/responses"
  )


  expect_equal(ncol(res), 7)
  expect_true(stringr::str_detect(res$decision_gpt, "404"))

  # Specifying wrong api key
  body <- list(
    model = "gpt-6-luna",
    input = question,
    tools = tools_simple,
    tool_choice = tools_choice_name
  )

  res <- .gpt_engine_responses(
    body = body,
    RPM = 10000,
    timeinf = T,
    tokeninf = T,
    key = 1234,
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient,
    back = NULL,
    aft = NULL,
    endpoint_url = "https://api.openai.com/v1/responses"
  )

  expect_equal(ncol(res), 7)
  expect_true(stringr::str_detect(res$decision_gpt, "401"))

  # Transient is correctly add to function
  .gpt_engine_responses(
    body = body,
    RPM = 10000,
    timeinf = T,
    tokeninf = T,
    key = "dummy-key", # not a real key, the request is mocked
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient(),
    back = NULL,
    aft = NULL,
    endpoint_url = "https://api.openai.com/v1/responses"
  ) |>
    expect_error()

})


test_that(".rep_gpt_engine_responses controls errrors correctly", {

  local_mock_api(mock_openai_errors)

  iterations <- 3

  # Ineligible tool
  test_tool <- list(
    list(
      type = "function",
      "function" = list(
        name = "inclusion_decision_simple",
        description = "A test function",
        strict = TRUE,
        parameters = list(
          # Invalid schema type
          type = "invalid_schema_type",
          properties = list(
            decision = list(
              type = "string",
              description = "The decision of the screening. Should be either 1 for include, 0 for exclude, or NA for uncertain."
            ),
            reason = list(
              type = "string",
              description = "A brief reason for the decision."
            )
          )
        )
      )
    )
  )

  res <- .rep_gpt_engine_responses(
    question = question,
    model_gpt = "gpt-5-nano",
    tool = test_tool,
    t_choice = "inclusion_decision_simple",
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    token_inf = T,
    apikey = "dummy-key", # not a real key, the request is mocked
    maxt = 4,
    maxs = 10,
    istrans = gpt_is_transient,
    ba = NULL,
    af = NULL,
    endpoint_url = "https://api.openai.com/v1/responses",
    reasoning_effort = NULL,
    verbosity = NULL
  )

  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(c("decision_gpt", "decision_binary", "n") %in% names(res)))
  expect_true(all(stringr::str_detect(res$decision_gpt, "400|invalid|schema|type")))

  # Ineligible api key
  res <- .rep_gpt_engine_responses(
    question = question,
    model_gpt = "gpt-5-nano",
    tool = tools_simple,
    t_choice = "inclusion_decision_simple",
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    token_inf = T,
    apikey = 1,
    maxt = 4,
    maxs = 10,
    istrans = gpt_is_transient,
    ba = NULL,
    af = NULL,
    endpoint_url = "https://api.openai.com/v1/responses",
    reasoning_effort = NULL,
    verbosity = NULL
  )

  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "401")))

  # Ineligible model
  res <- .rep_gpt_engine_responses(
    question = question,
    model_gpt = "gpt-5-nan",
    tool = tools_simple,
    t_choice = "inclusion_decision_simple",
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    token_inf = T,
    apikey = "dummy-key", # not a real key, the request is mocked
    maxt = 4,
    maxs = 10,
    istrans = gpt_is_transient,
    ba = NULL,
    af = NULL,
    endpoint_url = "https://api.openai.com/v1/responses",
    reasoning_effort = NULL,
    verbosity = NULL
  )

  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "404")))

})






test_that("tabscreen_gpt() stops for deprecated gpt-4 and gpt-3.5 models.", {

  deprecated_models <- c(
    "gpt-4", "gpt-4-turbo", "gpt-4o", "gpt-4o-mini", "gpt-4.1", "gpt-4.1-nano",
    "gpt-3.5-turbo", "gpt-3.5-turbo-0125"
  )

  for (m in deprecated_models) {

    expect_error(
      tabscreen_gpt(
        data = filges2015_dat[1:2,],
        prompt = "Is this study about FFT?",
        studyid = studyid,
        title = title,
        abstract = abstract,
        model = m,
        api_key = "1234"
      ),
      "deprecated"
    )

    # Also when custom_model = TRUE
    expect_error(
      tabscreen_gpt(
        data = filges2015_dat[1:2,],
        prompt = "Is this study about FFT?",
        studyid = studyid,
        title = title,
        abstract = abstract,
        model = m,
        custom_model = TRUE,
        api_key = "1234"
      ),
      "deprecated"
    )

    expect_error(
      tabscreen_gpt.tools(
        data = filges2015_dat[1:2,],
        prompt = "Is this study about FFT?",
        studyid = studyid,
        title = title,
        abstract = abstract,
        model = m,
        api_key = "1234"
      ),
      "deprecated"
    )

  }

  # top_p has been removed from the OpenAI functions
  expect_false("top_p" %in% names(formals(tabscreen_gpt)))
  expect_false("top_p" %in% names(formals(tabscreen_gpt.tools)))
  expect_false("top_p" %in% names(formals(approximate_price_gpt)))

})

# Skipped tests that call the API
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


models <- c("gpt-5-nano", "gpt-6-luna")
reps <- c(10, 1)
rpm <- c(3500, 200)

skip <- TRUE
skip_github_action <- FALSE

test_that("tabscreen_gpt() overinclusive chooses default tools", {

  # No real API calls: the replies are mocked and run in this R process
  # (mocks do not reach parallel workers, hence the sequential plan).
  # The mock also stores each request body, so we can check which tool is sent.
  sent <- list()
  record_and_reply <- function(req) {
    sent[[length(sent) + 1]] <<- req$body$data
    mock_openai_ok(req)
  }
  local_mock_api(record_and_reply)
  withr::local_envvar(OPENAI_API_KEY = "dummy-key")
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))

  # Sends one record with the given settings and returns the request body
  body_for <- function(overinclusive, decision_description) {
    sent <<- list()
    tabscreen_gpt(
      data = filges2015_dat[1, ],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-5-nano",
      overinclusive = overinclusive,
      decision_description = decision_description,
      messages = FALSE,
      progress = FALSE
    )
    sent[[1]]
  }

  # Binary default tools when overinclusive = FALSE
  body <- body_for(overinclusive = FALSE, decision_description = FALSE)
  expect_identical(body$tool_choice$name, "inclusion_decision_simple_binary")
  expect_identical(body$tools[[1]]$name, "inclusion_decision_simple_binary")

  body <- body_for(overinclusive = FALSE, decision_description = TRUE)
  expect_identical(body$tool_choice$name, "inclusion_decision_binary")
  expect_identical(body$tools[[1]]$name, "inclusion_decision_binary")

  # Over-inclusive tools (supports 1.1) when overinclusive = TRUE
  body <- body_for(overinclusive = TRUE, decision_description = FALSE)
  expect_identical(body$tool_choice$name, "inclusion_decision_simple")
  expect_identical(body$tools[[1]]$name, "inclusion_decision_simple")

  body <- body_for(overinclusive = TRUE, decision_description = TRUE)
  expect_identical(body$tool_choice$name, "inclusion_decision")
  expect_identical(body$tools[[1]]$name, "inclusion_decision")

})

suppressWarnings(future::plan(future::multisession))

test_that("tabscreen_gpt() works with single parameter values.",{

  local_gpt_mock()

  # reps = 1, so there is no aggregated data
  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      title = title,
      abstract = abstract,
      model = "gpt-5-nano",
      reps = 1,
      max_tries = 0
    )

  )

  expect_true(is.numeric(test_obj$answer_data$decision_binary))

  expect_equal(nrow(test_obj$answer_data), 1L)
  expect_null(test_obj$answer_data_aggregated)
  expect_equal(nrow(test_obj$price_data), 1L)
  expect_length(test_obj$price_dollar, 1L)


  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-5-nano", "gpt-6-luna"),
      reps = 1
    )

  )

  expect_equal(nrow(test_obj$answer_data), 2L)
  expect_equal(nrow(test_obj$price_data), 2L)
  expect_length(test_obj$price_dollar, 1L)

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = c(prompt, prompt2),
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-5-nano"),
      reps = 1
    )

  )

  expect_equal(nrow(test_obj$answer_data), 2L)
  # price_data has one row per prompt, model and number of reps
  expect_equal(nrow(test_obj$price_data), 2L)
  expect_length(test_obj$price_dollar, 1L)

})

test_that("tabscreen_gpt() works with multiple parameter values.",{

  local_gpt_mock()

  # 1 record, 2 models: gpt-5-nano is asked twice and gpt-6-luna once
  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-5-nano", "gpt-6-luna"),
      reps = c(2, 1)
    )

  )

  expect_equal(nrow(test_obj$answer_data), 3L)
  expect_equal(nrow(test_obj$answer_data_aggregated), 2L)
  expect_equal(nrow(test_obj$price_data), 2L)
  expect_length(test_obj$price_dollar, 1L)

  expect_gt(nrow(test_obj$answer_data), nrow(test_obj$answer_data_aggregated))

  # 2 records
  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1:2,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-5-nano", "gpt-6-luna"),
      reps = c(2, 1)
    )

  )

  expect_equal(nrow(test_obj$answer_data), 6L)
  expect_equal(nrow(test_obj$answer_data_aggregated), 4L)
  expect_equal(nrow(test_obj$price_data), 2L)
  expect_length(test_obj$price_dollar, 1L)

  # 2 records and 2 prompts
  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1:2,],
      prompt = c(prompt, prompt2),
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-5-nano", "gpt-6-luna"),
      reps = c(2, 1)
    )

  )

  expect_equal(nrow(test_obj$answer_data), 12L)
  expect_equal(nrow(test_obj$answer_data_aggregated), 2*2*2)
  # price_data has one row per prompt, model and number of reps
  expect_equal(nrow(test_obj$price_data), 2*2)
  expect_length(test_obj$price_dollar, 1L)

})

test_that("tabscreen_gpt() don't return time and token info.", {

  local_gpt_mock()

  # Columns added by time_info and token_info
  time_col <- "run_time"
  token_cols <- c("prompt_tokens", "completion_tokens")

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-5-nano",
      reps = 1,
      time_info = FALSE,
      token_info = FALSE
    )

  )

  expect_false(any(c(time_col, token_cols) %in% names(test_obj$answer_data)))

  # Only answer_data and run_date are returned (no price data without token info)
  expect_equal(length(test_obj), 2)

  # With reps > 1, the aggregated data is returned too
  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-5-nano",
      reps = 2,
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
      model = "gpt-5-nano",
      reps = 1,
      time_info = TRUE,
      token_info = FALSE
    )

  )

  expect_false(any(token_cols %in% names(test_obj$answer_data)))
  expect_true(time_col %in% names(test_obj$answer_data))

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-5-nano",
      reps = 1,
      time_info = FALSE,
      token_info = TRUE
    )

  )

  expect_false(time_col %in% names(test_obj$answer_data))
  expect_true(all(token_cols %in% names(test_obj$answer_data)))

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
      model = "gpt-5-nano",
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
      model = "gpt-5-nano",
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
      model = "gpt-5-nano",
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
      model = "gpt-6-luna",
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
      model = "gpt-5-nano",
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
      model = "gpt-5-nano",
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
      model = "gpt-6-luna",
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
      model = c("gpt-5-nano", "gpt-6-luna"),
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
      model = c("gpt-5-nano", "gpt-6-luna"),
      reps = reps,
      rpm = rpm,
      functions = AIscreenR:::incl_function,
      function_call_name = list(name = "inclusion_decision")
    )

  )

  expect_error(

    test_obj <- tabscreen_gpt(
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

suppressWarnings(future::plan(future::sequential))

# Test parallel

test_that("That paralell processing works.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  suppressWarnings(future::plan(future::multisession, workers = future::availableCores()))

  expect_message(

    tm_par <- system.time(
      test_obj <- tabscreen_gpt(
        data = filges2015_dat[1:2,],
        prompt = prompt,
        title = title,
        abstract = abstract,
        model = "gpt-5-nano",
        reps = 10
      )
    )
  )

  suppressWarnings(future::plan(future::sequential))

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
      model = "gpt-5-nano",
      reps = 1,
      max_tokens = 9
    )

  ) |>
    suppressMessages()

  expect_false(is.na(test_obj$answer_data_all$decision_gpt))

})

