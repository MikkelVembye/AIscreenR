flaky_api <- function(api, env = parent.frame()) {
  mode <- new.env()
  mode$fail <- TRUE
  ok <- llm_handler(api)
  bad <- llm_handler(api, status = 500L)
  local_mock_api(function(req) if (mode$fail) bad(req) else ok(req), .env = env)
  mode
}

screen_with_errors <- function(fun, api, ..., env = parent.frame()) {
  mode <- flaky_api(api, env)
  res <- suppressMessages(fun(
    data = mock_dat(3), prompt = "Is it relevant?", studyid = studyid, title = title,
    abstract = abstract, api_key = "k", progress = FALSE, max_tries = 1, ...
  ))
  list(res = res, mode = mode)
}

test_that("screen_errors() rescreens failed gpt requests", {
  x <- screen_with_errors(tabscreen_gpt, "gpt", model = "gpt-5-nano")
  expect_equal(nrow(x$res$error_data), 3)

  x$mode$fail <- FALSE
  out <- suppressMessages(screen_errors(x$res, api_key = "k"))
  expect_null(out$error_data)
  expect_equal(out$answer_data$decision_binary, c(1, 1, 1))
})

test_that("screen_errors() rescreens failed gpt requests with repeated answers", {
  x <- screen_with_errors(tabscreen_gpt, "gpt", model = "gpt-5-nano", reps = 2)
  x$mode$fail <- FALSE
  out <- suppressMessages(screen_errors(x$res, api_key = "k"))
  expect_equal(nrow(out$answer_data), 6)
  expect_equal(out$answer_data$decision_binary, rep(1, 6))
})

test_that("screen_errors() keeps errors that persist", {
  x <- screen_with_errors(tabscreen_gpt, "gpt", model = "gpt-5-nano")
  out <- suppressMessages(screen_errors(x$res, api_key = "k"))
  expect_equal(nrow(out$error_data), 3)
})

test_that("screen_errors() handles a mix of successes and failures", {
  calls <- 0
  ok <- llm_handler("gpt")
  bad <- llm_handler("gpt", status = 400L)
  mode <- new.env()
  mode$all_ok <- FALSE
  local_mock_api(function(req) {
    calls <<- calls + 1
    if (!mode$all_ok && calls == 1) bad(req) else ok(req)
  })
  res <- suppressMessages(tabscreen_gpt(
    mock_dat(3), prompt = "p", studyid = studyid, title = title, abstract = abstract,
    api_key = "k", progress = FALSE, model = "gpt-5-nano", max_tries = 1
  ))
  expect_equal(nrow(res$error_data), 1)

  mode$all_ok <- TRUE
  out <- suppressMessages(screen_errors(res, api_key = "k"))
  expect_equal(nrow(out$answer_data), 3)
  expect_null(out$error_data)
})

test_that("screen_errors.gpt() rescreens directly", {
  x <- screen_with_errors(
    tabscreen_gpt, "gpt_chat", model = "gpt-5-nano",
    tools = tools_simple, tool_choice = "inclusion_decision_simple"
  )
  x$mode$fail <- FALSE
  out <- suppressMessages(screen_errors.gpt(x$res, api_key = "k"))
  expect_equal(out$answer_data$decision_binary, c(1, 1, 1))
  expect_null(out$error_data)

  expect_identical(screen_errors.gpt(out), out)
})

test_that("screen_errors.gpt() combines old successes with new answers and strips markup after HTTP 400", {
  mode <- new.env()
  mode$fail <- TRUE
  ok <- llm_handler("gpt_chat")
  bad <- llm_handler("gpt_chat", status = 400L)
  calls <- 0
  local_mock_api(function(req) {
    calls <<- calls + 1
    if (mode$fail && calls > 1) bad(req) else ok(req)
  })
  dat <- mock_dat(4)
  dat$abstract[2:4] <- "<b>bold</b> & text"
  res <- suppressMessages(tabscreen_gpt(
    dat, prompt = "p", studyid = studyid, title = title, abstract = abstract,
    api_key = "k", progress = FALSE, model = "gpt-5-nano", max_tries = 1,
    tools = tools_simple, tool_choice = "inclusion_decision_simple"
  ))
  expect_equal(nrow(res$error_data), 3)

  mode$fail <- FALSE
  out <- suppressMessages(screen_errors.gpt(res, api_key = "k"))
  expect_equal(nrow(out$answer_data), 4)
  expect_null(out$error_data)
})

test_that("screen_errors.gpt() recovers repeated answers", {
  x <- screen_with_errors(
    tabscreen_gpt, "gpt_chat", model = "gpt-5-nano", reps = 2,
    incl_cutoff_upper = 0.8, incl_cutoff_lower = 0.2, tools = tools_simple, tool_choice = "inclusion_decision_simple"
  )
  x$mode$fail <- FALSE
  out <- suppressMessages(screen_errors.gpt(x$res, api_key = "k", max_seconds = 5))
  expect_equal(nrow(out$answer_data), 6)
  expect_s3_class(out$answer_data_aggregated, "gpt_agg_tbl")
})

test_that("screen_errors() rescreens failed groq requests", {
  x <- screen_with_errors(tabscreen_groq, "chat")
  x$mode$fail <- FALSE
  out <- suppressMessages(screen_errors(x$res, api_key = "k"))
  expect_equal(out$answer_data$decision_binary, c(1, 1, 1))
})

test_that("screen_errors() rejects unsupported objects", {
  obj <- structure(list(error_data = tibble::tibble()), class = "gpt", arg_list = list(api_url = "ollama"))
  expect_error(screen_errors(structure(list(), class = "other")), "Unknown object backend")

  res <- run_screen(tabscreen_ollama, "ollama", model = "llama3")
  attr(res, "arg_list")$api_url <- "http://127.0.0.1:11434/api/chat"
  class(res) <- c("ollama", class(res))
  expect_error(screen_errors(res), "not supported for Ollama")
})

test_that("screen_errors() validates the columns it needs", {
  x <- screen_with_errors(tabscreen_gpt, "gpt", model = "gpt-5-nano")
  res <- x$res
  res$error_data <- dplyr::rename(res$error_data, record = studyid)
  expect_error(screen_errors(res), "studyid")

  res <- x$res
  res$error_data <- dplyr::rename(res$error_data, ti = title)
  res$error_data$abstract <- NULL
  expect_error(screen_errors(res), "infer title/abstract")
})

test_that("screen_errors() accepts explicit column names", {
  x <- screen_with_errors(tabscreen_gpt, "gpt", model = "gpt-5-nano")
  res <- x$res
  res$error_data <- dplyr::rename(res$error_data, TI = title, AB = abstract)
  x$mode$fail <- FALSE
  out <- suppressMessages(screen_errors(res, studyid = studyid, title = TI, abstract = AB, api_key = "k"))
  expect_equal(nrow(out$answer_data), 3)
})

test_that("screen_errors() detects backend from the api url and the prompt", {
  x <- screen_with_errors(tabscreen_gpt, "gpt", model = "gpt-5-nano")
  res <- x$res
  class(res) <- setdiff(class(res), "gpt")
  attr(res, "arg_list")$api_url <- "https://api.openai.com/v1/responses"
  x$mode$fail <- FALSE
  out <- suppressMessages(screen_errors(res, api_key = "k"))
  expect_equal(nrow(out$answer_data), 3)

  res$answer_data$prompt <- NULL
  res$error_data$prompt <- NULL
  expect_error(screen_errors(res), "original prompt")
})

test_that("screen_errors() detects groq and unknown-url backends and reads the prompt from alternatives", {
  x <- screen_with_errors(tabscreen_groq, "chat")
  x$mode$fail <- FALSE
  res <- x$res
  class(res) <- c("groq", "list")
  out <- suppressMessages(screen_errors(res, api_key = "k"))
  expect_equal(nrow(out$answer_data), 3)

  y <- screen_with_errors(tabscreen_gpt, "gpt", model = "gpt-5-nano")
  y$mode$fail <- FALSE
  res <- y$res
  attr(res, "arg_list")$api_url <- "https://custom.example.com/v1/responses"
  out <- suppressMessages(screen_errors(res, api_key = "k"))
  expect_equal(nrow(out$answer_data), 3)

  res <- y$res
  res$answer_data <- NULL
  out <- suppressMessages(screen_errors(res, api_key = "k"))
  expect_equal(nrow(out$answer_data), 3)

  res <- y$res
  res$answer_data <- NULL
  res$error_data$prompt <- NULL
  attr(res, "arg_list")$prompt <- "Is it relevant?"
  out <- suppressMessages(screen_errors(res, api_key = "k"))
  expect_equal(nrow(out$answer_data), 3)

  res <- y$res
  attr(res, "arg_list")$api_url <- "http://localhost:11434/api/chat"
  class(res) <- "list"
  expect_error(screen_errors(res), "Ollama")
})

test_that("screen_errors() reports when there is nothing to rescreen", {
  res <- run_screen(tabscreen_gpt, "gpt", model = "gpt-5-nano")
  res$error_data <- tibble::tibble()
  expect_message(out <- screen_errors(res), "No errors to rescreen")
  expect_identical(out, res)
})

