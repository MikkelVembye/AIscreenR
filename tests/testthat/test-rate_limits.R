models <- c("gpt-6-luna", "gpt-5-nano")

skip <- FALSE

# No real API calls: the replies are mocked. Mocks do not reach parallel
# workers, so the tests run sequentially (and restore the plan afterwards).
local_sequential <- function(env = parent.frame()) {
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan), envir = env)
}

# A reply with the rate limit headers that OpenAI sends back
mock_rate_limits_ok <- function(req) {
  httr2::response_json(
    status_code = 200,
    headers = list(
      "x-ratelimit-limit-requests" = "5000",
      "x-ratelimit-limit-tokens" = "2000000"
    ),
    body = list()
  )
}

mock_unauthorized <- function(req) {
  httr2::response_json(
    status_code = 401,
    body = list(error = list(message = "Incorrect API key provided"))
  )
}

test_that("General working of rate_limits_per_minute().", {

  if(skip) skip()
  skip_on_cran()

  local_mock_api(mock_rate_limits_ok)
  local_sequential()
  withr::local_envvar(OPENAI_API_KEY = "dummy-key") # not a real key

  rate_limits_per_minute() |>
    nrow() |>
    expect_equal(1L)

  rlpm <- rate_limits_per_minute()

  expect_true(is.numeric(rlpm$requests_per_minute))
  expect_true(is.numeric(rlpm$tokens_per_minute))
  expect_equal(rlpm$requests_per_minute, 5000)
  expect_equal(rlpm$tokens_per_minute, 2000000)

  rate_limits_per_minute(model = models) |>
    nrow() |>
    expect_equal(2L)

  rlpm <- rate_limits_per_minute(model = models)
  expect_length(rlpm$requests_per_minute, 2)
  expect_output(print(rlpm), "tibble")

})

test_that("rate_limits_per_minute() casts errors correctly.", {

  #if(skip) skip()
  #skip_on_cran()

  local_mock_api(mock_unauthorized)
  local_sequential()

  expect_error(
    rate_limits_per_minute(model = "gpt-3"),
    "Unknown model"
  )

  expect_error(
    rate_limits_per_minute(model = "gpt-4o-mini"),
    "Unknown model"
  )

  rlpm <- rate_limits_per_minute(api_key = 1234)$model
  expect_true(stringr:::str_detect(rlpm, "401"))

  expect_error(
    rate_limits_per_minute(AI_tool = "Bard")
  )


})
