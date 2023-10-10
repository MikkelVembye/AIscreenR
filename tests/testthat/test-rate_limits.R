models <- c("gpt-3.5-turbo-0613", "gpt-4")

test_that("General working of rate_limits_per_minute().", {

  skip_on_cran()

  rate_limits_per_minute(model = models) |>
    nrow() |>
    expect_equal(2)

  rlpm <- rate_limits_per_minute(model = models)
  expect_equal(rlpm$requests_per_minute[1], 3500L)
  expect_equal(rlpm$tokens_per_minute[1], 90000L)
  expect_output(print(rlpm), "tibble")

})

test_that("rate_limits_per_minute() casts errors correctly.", {

  skip_on_cran()

  rlpm <- rate_limits_per_minute(model = "gpt-3")
  expect_identical(rlpm$model, "Error 404 [check gpt model]")

  rlpm <- rate_limits_per_minute(api_key = 1234)
  expect_identical(rlpm$model, "Error 401 Unauthorized [invalid api]")

})


