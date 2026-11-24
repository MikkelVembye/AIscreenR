local_legacy <- function(env = parent.frame()) {
  withr::local_options(lifecycle_verbosity = "quiet", .local_envir = env)
}

legacy_screen <- function(..., model = "gpt-4", api = "legacy", status = 200L, internet = TRUE, env = parent.frame()) {
  local_legacy(env)
  local_mock_api(llm_handler(api, status = status), internet = internet, .env = env)
  suppressMessages(tabscreen_gpt.original(
    data = mock_dat(2), prompt = "Is it relevant?", studyid = studyid, title = title,
    abstract = abstract, api_key = "k", progress = FALSE, max_tries = 1, model = "gpt-4", ...
  ))
}

test_that("tabscreen_gpt.original() screens with the legacy functions interface", {
  res <- legacy_screen()
  expect_s3_class(res, "chatgpt")
  expect_equal(res$answer_data_all$decision_binary, c(1, 1))
  expect_null(res$error_data)
  expect_output(print(res))

  rep_res <- legacy_screen(reps = 3, force = TRUE)
  expect_equal(nrow(rep_res$answer_data_all), 6)
  expect_equal(rep_res$answer_data_sum$final_decision_gpt, c("Include", "Include"))

  res <- legacy_screen(
    decision_description = TRUE, functions = incl_function,
    function_call_name = list(name = "inclusion_decision")
  )
  expect_equal(res$answer_data_all$detailed_description, rep("Mock reasoning.", 2))
})

test_that("tabscreen_gpt.original() records failures", {
  res <- legacy_screen(status = 401L)
  expect_equal(nrow(res$error_data), 2)
  expect_match(res$answer_data_all$decision_gpt, "Error", all = TRUE)

  res <- legacy_screen(internet = FALSE)
  expect_match(res$answer_data_all$decision_gpt, "internet", all = TRUE)
})

test_that("tabscreen_gpt.original() validates arguments", {
  expect_error(legacy_screen(reps = 11), "force")
  expect_error(legacy_screen(model = c("gpt-4", "gpt-4o"), rpm = 1:3), "same length")
  expect_error(legacy_screen(model = c("gpt-4", "gpt-4o"), reps = 1:3, force = TRUE), "same length")
  expect_error(legacy_screen(incl_cutoff_upper = 0.2, incl_cutoff_lower = 0.5, reps = 2, force = TRUE), "must not exceed")
  expect_error(legacy_screen(max_tokens = 5), "tokens below")
})

test_that("screen_errors.chatgpt() rescreens failed legacy requests", {
  local_legacy()
  mode <- new.env()
  mode$fail <- TRUE
  ok <- llm_handler("legacy")
  bad <- llm_handler("legacy", status = 500L)
  local_mock_api(function(req) if (mode$fail) bad(req) else ok(req))
  res <- suppressWarnings(suppressMessages(tabscreen_gpt.original(
    mock_dat(2), prompt = "p", studyid = studyid, title = title, abstract = abstract,
    api_key = "k", progress = FALSE, max_tries = 1, model = "gpt-4"
  )))
  expect_equal(nrow(res$error_data), 2)

  mode$fail <- FALSE
  out <- suppressWarnings(suppressMessages(screen_errors.chatgpt(res, api_key = "k")))
  expect_equal(out$answer_data_all$decision_binary, c(1, 1))
  expect_null(out$error_data)
})

test_that("tabscreen_gpt.tools() screens with chat completions", {
  local_legacy()
  local_mock_api(llm_handler("chat"))
run <- function(..., model = "gpt-5-nano") {
    suppressMessages(tabscreen_gpt.tools(
      mock_dat(2), prompt = "p", studyid = studyid, title = title, abstract = abstract,
      api_key = "k", progress = FALSE, max_tries = 1, model = model, ...
    ))
  }
  res <- run()
  expect_equal(res$answer_data$decision_binary, c(1, 1))

  res <- run(reps = 2, decision_description = TRUE, confidence = TRUE)
  expect_true(all(c("longest_answer", "mean_confidence") %in% names(res$answer_data_aggregated)))

  res <- run(overinclusive = FALSE)
  expect_equal(nrow(res$answer_data), 2)

  expect_error(run(reps = 11), "force")
  expect_error(run(model = "gpt-4o"), "deprecated")
  expect_error(run(model = "unknown"), "Unknown gpt model")
  expect_error(run(tools = "x"), "list")
  expect_error(run(tool_choice = "auto"), "tool")
  expect_error(run(reasoning_effort = "extreme"), "reasoning_effort")
  expect_error(run(max_tokens = 2), "tokens below 9")
  expect_error(run(reps = 2, incl_cutoff_lower = 0.5), "incl_cutoff_upper")
})

test_that("tabscreen_gpt.tools() records failures", {
  local_mock_api(llm_handler("chat", status = 500L))
  res <- suppressMessages(tabscreen_gpt.tools(
    mock_dat(2), prompt = "p", studyid = studyid, title = title, abstract = abstract,
    api_key = "k", progress = FALSE, max_tries = 1, model = "gpt-5-nano"
  ))
  expect_equal(nrow(res$error_data), 2)
})

test_that("tabscreen_gpt.tools() validates further arguments and handles models", {
  local_legacy()
  local_mock_api(llm_handler("chat"))
run <- function(..., model = "gpt-5-nano", prompt = "p") {
    suppressMessages(tabscreen_gpt.tools(
      mock_dat(2), prompt = prompt, studyid = studyid, title = title, abstract = abstract,
      api_key = "k", progress = FALSE, max_tries = 1, model = model, ...
    ))
  }
  expect_error(run(model = c("gpt-5-nano", "gpt-5-mini"), rpm = 1:3), "rpm")
  expect_error(run(model = c("gpt-5-nano", "gpt-5-mini"), reps = 1:3), "reps")
  expect_error(run(prompt = c("a", "a")), "same prompt twice")
  expect_error(run(reps = 2, incl_cutoff_upper = 0.2, incl_cutoff_lower = 0.5), "must not exceed")
  expect_error(run(reps = 2, incl_cutoff_upper = "a"), "numeric")
  expect_error(run(reps = 2, incl_cutoff_upper = 2), "between 0 and 1")
  expect_error(run(reps = 2, incl_cutoff_upper = 0.5, incl_cutoff_lower = "a"), "numeric")
  expect_error(run(reps = 2, incl_cutoff_upper = 0.5, incl_cutoff_lower = -1), "between 0 and 1")
  expect_error(run(verbosity = "loud"), "verbosity")
  expect_error(run(decision_description = TRUE, max_tokens = 20), "descriptive")

  res <- run(model = "gpt-5-nano", reps = 2, incl_cutoff_upper = 0.8)
  expect_equal(nrow(res$answer_data_aggregated) > 0, TRUE)

  res <- run(model = "my-model", custom_model = TRUE)
  expect_null(res$price_data)
  res <- run(model = c("gpt-5-nano", "gpt-5-mini"), reps = c(1, 2), rpm = c(10, 20))
  expect_equal(length(unique(res$answer_data$model)), 2)
  res <- run(model = c("gpt-5-nano", "gpt-5-nano"))
  expect_equal(length(unique(res$answer_data$model)), 1)
  res <- run(token_info = FALSE, time_info = FALSE)
  expect_false("prompt_tokens" %in% names(res$answer_data))
  res <- run(tools = tools_simple, seed_par = 1)
  expect_equal(nrow(res$answer_data), 2)
  res <- run(tools = tools_simple, tool_choice = "inclusion_decision_simple", overinclusive = FALSE)
  expect_equal(nrow(res$answer_data), 2)
  res <- run(overinclusive = FALSE, decision_description = TRUE, confidence = TRUE)
  expect_equal(nrow(res$answer_data), 2)
  res <- run(overinclusive = FALSE, confidence = TRUE)
  expect_equal(nrow(res$answer_data), 2)
  res <- run(model = "gpt-5-nano", fine_tuned = TRUE)
  expect_equal(nrow(res$answer_data), 2)
  res <- run(model = "my-model", custom_model = TRUE, reasoning_effort = "high")
  expect_equal(nrow(res$answer_data), 2)

})

test_that("tabscreen_gpt.tools() stops when the approximate price is too high", {
  local_legacy()
  local_mocked_bindings(.price_gpt = \(x) tibble::tibble(total_price_dollar = 100))
  local_mock_api(llm_handler("chat"))
  expect_error(
    tabscreen_gpt.tools(mock_dat(2), prompt = "p", studyid = studyid, title = title, abstract = abstract, api_key = "k", model = "gpt-5-nano"),
    "force"
  )
})

test_that("screen_errors.chatgpt() handles HTTP 400 errors, repeated answers and detailed descriptions", {
  local_legacy()
  mode <- new.env()
  mode$fail <- TRUE
  calls <- 0
  ok <- llm_handler("legacy")
  bad <- llm_handler("legacy", status = 400L)
  local_mock_api(function(req) {
    calls <<- calls + 1
    if (mode$fail && calls %in% c(1, 3, 4)) bad(req) else ok(req)
  })
  dat <- mock_dat(3)
  dat$abstract[1] <- "<b>markup</b> & more"
  res <- suppressWarnings(suppressMessages(tabscreen_gpt.original(
    dat, prompt = "p", studyid = studyid, title = title, abstract = abstract,
    api_key = "k", progress = FALSE, max_tries = 1, model = "gpt-4", reps = 2, force = TRUE,
    decision_description = TRUE, functions = incl_function,
    function_call_name = list(name = "inclusion_decision")
  )))
  expect_gt(nrow(res$error_data), 0)

  mode$fail <- FALSE
  out <- suppressWarnings(suppressMessages(screen_errors.chatgpt(res, api_key = "k", max_seconds = 5, is_transient = gpt_is_transient, backoff = NULL, after = NULL)))
  expect_equal(nrow(out$answer_data_all), 6)
  expect_null(out$error_data)
  expect_true("longest_answer" %in% names(out$answer_data_sum))

  expect_identical(screen_errors.chatgpt(out), out)
})

