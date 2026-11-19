# Fake Claude errors, so these tests never reach the real API:
# unknown models give 404, a wrong role gives 400, anything else gives 401.
mock_claude_errors <- function(req) {
  model <- req$body$data$model
  role <- req$body$data$messages[[1]]$role

  if (model %in% c("claude-haiku-1")) {
    status <- 404
    msg <- "The model does not exist"
  } else if (!identical(role, "user")) {
    status <- 400
    msg <- "Invalid role"
  } else {
    status <- 401
    msg <- "Invalid x-api-key"
  }

  httr2::response_json(status_code = status, body = list(type = "error", error = list(message = msg)))
}

test_that(".claude_engine return errors correctly", {

  local_mock_api(mock_claude_errors)

  # Specifying unknown model
  body <- list(
    model = "claude-haiku-1",
    max_tokens = 1024,
    messages = list(list(
      role = "user",
      content = question
    )),
    tools = tools_simple_claude
  )

  res <- .claude_engine(
    body = body,
    RPM = 10000,
    time_inf = T,
    token_inf = T,
    api_key = "dummy-key", # not a real key, the request is mocked
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient,
    back = NULL,
    aft = NULL,
    endpoint_url = "https://api.anthropic.com"
  )


  expect_equal(ncol(res), 7)
  expect_true(stringr::str_detect(res$decision_gpt, "404"))

  # Specifying wrong api key
  body <- list(
    model = "claude-haiku-4-5",
    max_tokens = 1024,
    messages = list(list(
      role = "user",
      content = question
    )),
    tools = tools_simple_claude
  )

  res <- .claude_engine(
    body = body,
    RPM = 10000,
    time_inf = T,
    token_inf = T,
    api_key = 1234,
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient,
    back = NULL,
    aft = NULL,
    endpoint_url = "https://api.anthropic.com"
  )

  expect_equal(ncol(res), 7)
  expect_true(stringr::str_detect(res$decision_gpt, "401"))

  # Specifying ineligible role
  body <- list(
    model = "claude-haiku-4-5",
    max_tokens = 1024,
    messages = list(list(
      role = "use",
      content = question
    )),
    tools = tools_simple_claude
  )

  res <- .claude_engine(
    body = body,
    RPM = 10000,
    time_inf = T,
    token_inf = T,
    api_key = "dummy-key", # not a real key, the request is mocked
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient,
    back = NULL,
    aft = NULL,
    endpoint_url = "https://api.anthropic.com"
  )

  expect_equal(ncol(res), 7)
  expect_true(stringr::str_detect(res$decision_gpt, "400"))

  # Specifying ineligible role
  body <- list(
    model = "claude-haiku-4-5",
    max_tokens = 1024,
    messages = list(list(
      role = "use",
      content = question
    )),
    tools = tools_simple_claude
  )

  # Transient is correctly add to function
  .claude_engine(
    body = body,
    RPM = 10000,
    time_inf = T,
    token_inf = T,
    api_key = "dummy-key", # not a real key, the request is mocked
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient(),
    back = NULL,
    aft = NULL,
    endpoint_url = "https://api.anthropic.com"
  ) |>
    expect_error()

})


test_that(".rep_claude_engine controls errrors correctly", {

  local_mock_api(mock_claude_errors)

  iterations <- 3

  # Ineligible role
  res <- .rep_claude_engine(
    question = question,
    model_gpt = "claude-haiku-4-5",
    role_gpt = "use",
    tool = tools_simple_claude,
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    token_inf = T,
    api_key = "dummy-key", # not a real key, the request is mocked
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient,
    back = NULL,
    aft = NULL,
    endpoint_url = "https://api.anthropic.com"
  )

  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "400")))

  # Ineligible api key
  res <- .rep_claude_engine(
    question = question,
    model_gpt = "claude-haiku-4-5",
    role_gpt = "user",
    tool = tools_simple_claude,
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    token_inf = T,
    api_key = 123,
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient,
    back = NULL,
    aft = NULL,
    endpoint_url = "https://api.anthropic.com"
  )

  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "401")))

  # Ineligible model
  res <- .rep_claude_engine(
    question = question,
    model_gpt = "claude-haiku-1",
    role_gpt = "user",
    tool = tools_simple_claude,
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    token_inf = T,
    api_key = "dummy-key", # not a real key, the request is mocked
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient,
    back = NULL,
    aft = NULL,
    endpoint_url = "https://api.anthropic.com"
  )

  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "404")))

})