# Tests for setup and AI screening in the screening app. All API calls are mocked.

# Answers every request: include if the study number is odd, and fail the study
# numbers in `fail`
screening_handler <- function(fail = integer(0), reasons = FALSE) {
  function(req) {
    study <- as.integer(sub(".*Study ([0-9]+):.*", "\\1", req$body$data$input[1]))
    if (study %in% fail) {
      return(json_resp(list(error = list(message = "Mock failure")), status = 401))
    }
    args <- list(decision_gpt = if (study %% 2 == 1) "1" else "0")
    if (reasons) args$detailed_description <- paste("Reason", study)
    json_resp(mock_responses_body(args))
  }
}

local_ai_project <- function(n, chunk_size = 2, env = parent.frame()) {
  path <- local_screening_project(n, env = env)
  .update_screening_project(path, function(p) {
    .set_screening_setup(p, "Is it relevant?", "gpt-6-luna", FALSE)
  })
  withr::local_options(AIscreenR.screening_chunk_size = chunk_size, .local_envir = env)
  local_sequential_plan(env)
  path
}

test_that("only OpenAI models are offered", {
  models <- .screening_models()

  expect_true("gpt-6-luna" %in% models)
  expect_true(all(startsWith(models, "gpt-")))
})

test_that("the setup is checked and locked once the AI has answered", {
  path <- local_screening_project(2)
  p <- .load_screening_project(path)

  expect_error(.set_screening_setup(p, "  ", "gpt-6-luna", FALSE), "Write a screening question")
  expect_error(.set_screening_setup(p, "q", "unknown", FALSE))

  p <- .add_ai_answers(.set_screening_setup(p, "q", "gpt-6-luna", TRUE), fake_ai_answers(1))
  expect_error(.set_screening_setup(p, "other", "gpt-6-luna", FALSE), "Delete the AI answers")

  p <- .delete_ai_answers(p)
  expect_null(p$ai)
  expect_true("ai" %in% names(p))
  expect_equal(.set_screening_setup(p, "other", "gpt-6-luna", FALSE)$question, "other")
})

test_that(".ai_cost() covers only the records without an AI answer", {
  path <- local_screening_project(4)
  p <- .set_screening_setup(.load_screening_project(path), "q", "gpt-6-luna", FALSE)

  all_records <- .ai_cost(p)
  half <- .ai_cost(.add_ai_answers(p, fake_ai_answers(c(1, 0))))

  expect_gt(all_records, 0)
  expect_gt(all_records, half)
  expect_equal(.ai_cost(.add_ai_answers(p, fake_ai_answers(c(1, 0, 1, 0)))), 0)
})

test_that("the app screens all records in chunks and never saves the API key", {
  skip_if_no_app()
  path <- local_ai_project(5)
  mock <- local_mock_api(screening_handler(), .env = environment())

  shiny::testServer(app_dir(), {
    session$setInputs(api_key = "sk-very-secret", start_ai = 1)
    for (i in 1:5) session$elapse(500)

    expect_false(ai_running())
    expect_equal(.ai_decisions(project()), c(1L, 0L, 1L, 0L, 1L))
    expect_match(output$ai_panel$html, "5 of 5 records")
    expect_match(output$ai_panel$html, "AI screening is done")
  })

  expect_equal(mock$n, 5)
  saved <- readBin(path, "raw", file.size(path))
  expect_length(grepRaw("sk-very-secret", saved, fixed = TRUE), 0)
})

test_that("prices are added up and formatted", {
  p <- list(ai_price = 0)
  p <- .add_ai_price(.add_ai_price(p, 0.5), 0.25)

  expect_equal(p$ai_price, 0.75)
  expect_equal(.add_ai_price(p, NULL)$ai_price, 0.75)
  expect_equal(.format_price(0.75), "$0.75")
  expect_equal(.format_price(12), "$12.00")
  expect_equal(.format_price(0.0000531), "$0.000053")
  expect_equal(.delete_ai_answers(p)$ai_price, 0)
})

test_that(".token_price() uses the tokens and prices without rounding per request", {
  prices <- model_prizes[model_prizes$model == "gpt-6-luna", ]
  answers <- tibble::tibble(
    model = "gpt-6-luna",
    prompt_tokens = c(11, 11, NA), # The last request failed
    completion_tokens = c(7, 7, NA)
  )

  expect_equal(
    .token_price(answers),
    2 * (11 * prices$price_in_per_token + 7 * prices$price_out_per_token)
  )
  expect_gt(.token_price(answers), 0) # Not rounded away
})

test_that("the total price of all chunks and runs is shown and printed", {
  skip_if_no_app()
  path <- local_ai_project(6)
  local_mock_api(screening_handler(), .env = environment())
  # Every mocked request uses 11 input and 7 output tokens
  prices <- model_prizes[model_prizes$model == "gpt-6-luna", ]
  six_records <- 6 * (11 * prices$price_in_per_token + 7 * prices$price_out_per_token)

  shiny::testServer(app_dir(), {
    session$setInputs(api_key = "dummy", start_ai = 1)
    session$setInputs(stop_ai = 1)
    session$elapse(500) # Two of the three chunks, then stop
    after_stop <- project()$ai_price
    expect_gt(after_stop, 0)

    printed <- testthat::capture_messages({
      session$setInputs(start_ai = 2)
      session$elapse(500)
    })

    expect_equal(after_stop, six_records * 4 / 6)
    expect_equal(project()$ai_price, six_records)
    expect_match(output$ai_panel$html, "Total price of the AI screening: \\$0\\.")
    expect_match(paste(printed, collapse = " "), "Total price of the AI screening: \\$0\\.")
  })
})

test_that("the AI panel does not show decisions while running", {
  skip_if_no_app()
  path <- local_ai_project(4)
  local_mock_api(screening_handler(), .env = environment())

  shiny::testServer(app_dir(), {
    session$setInputs(api_key = "dummy", start_ai = 1)

    expect_true(ai_running())
    expect_no_match(output$ai_panel$html, "Include|Exclude")
    expect_match(output$setup_panel$html, "setup is locked")
  })
})

test_that("Stop finishes the current chunk, and Continue runs the rest", {
  skip_if_no_app()
  path <- local_ai_project(6)
  mock <- local_mock_api(screening_handler(), .env = environment())

  shiny::testServer(app_dir(), {
    session$setInputs(api_key = "dummy", start_ai = 1)
    session$setInputs(stop_ai = 1)
    session$elapse(500)
    session$elapse(500)

    expect_false(ai_running())
    expect_equal(.ai_missing(project()), c(5, 6))
    expect_match(output$ai_panel$html, "Stopped")
    expect_match(output$ai_panel$html, "Continue/Retry")

    session$setInputs(start_ai = 2)
    session$elapse(500)

    expect_length(.ai_missing(project()), 0)
  })

  expect_equal(mock$n, 6) # No record was paid for twice
})

test_that("failed requests are not saved and can be retried", {
  skip_if_no_app()
  path <- local_ai_project(4)
  local_mock_api(screening_handler(fail = 3), .env = environment())

  shiny::testServer(app_dir(), {
    session$setInputs(api_key = "dummy", start_ai = 1)
    for (i in 1:3) session$elapse(500)

    expect_equal(.ai_missing(project()), 3)
    expect_match(output$ai_panel$html, "1 records got no answer")
  })

  local_mock_api(screening_handler(), .env = environment())
  shiny::testServer(app_dir(), {
    session$setInputs(api_key = "dummy", start_ai = 1)
    session$elapse(500)

    expect_length(.ai_missing(project()), 0)
  })
})

test_that("the run stops when every request in a chunk fails", {
  skip_if_no_app()
  path <- local_ai_project(6)
  mock <- local_mock_api(screening_handler(fail = 1:6), .env = environment())

  shiny::testServer(app_dir(), {
    session$setInputs(api_key = "wrong", start_ai = 1)
    for (i in 1:3) session$elapse(500)

    expect_false(ai_running())
    expect_null(project()$ai) # So the setup is not locked
    expect_match(output$ai_panel$html, "every request in a chunk failed")
    expect_match(output$ai_panel$html, "Mock failure")
  })

  expect_equal(mock$n, 2) # Only the first chunk was sent
})

test_that("AI reasons are saved when asked for", {
  skip_if_no_app()
  path <- local_ai_project(2)
  .update_screening_project(path, function(p) {
    .set_screening_setup(p, "Is it relevant?", "gpt-6-luna", TRUE)
  })
  local_mock_api(screening_handler(reasons = TRUE), .env = environment())

  shiny::testServer(app_dir(), {
    session$setInputs(api_key = "dummy", start_ai = 1)
    session$elapse(500)

    expect_equal(.screening_results(project())$ai_reason, c("Reason 1", "Reason 2"))
  })
})

test_that("a cost above $15 must be accepted first", {
  skip_if_no_app()
  path <- local_ai_project(2)
  mock <- local_mock_api(screening_handler(), .env = environment())
  testthat::local_mocked_bindings(.ai_cost = function(project) 20)

  shiny::testServer(app_dir(), {
    session$setInputs(api_key = "dummy", start_ai = 1)
    expect_false(ai_running())
    expect_match(output$ai_panel$html, "I accept this cost")

    session$setInputs(accept_cost = TRUE, start_ai = 2)
    session$elapse(500)
    expect_length(.ai_missing(project()), 0)
  })

  expect_equal(mock$n, 2)
})

test_that("deleting the AI answers needs a confirmation", {
  skip_if_no_app()
  path <- local_ai_project(2)
  .update_screening_project(path, function(p) .add_ai_answers(p, fake_ai_answers(c(1, 0))))

  shiny::testServer(app_dir(), {
    session$setInputs(delete_ai = 1)
    expect_equal(nrow(project()$ai), 2)

    session$setInputs(confirm_delete = 1)
    expect_null(project()$ai)
    expect_match(output$setup_panel$html, "Screening question")
  })
})
