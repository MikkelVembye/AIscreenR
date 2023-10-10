models <- c("gpt-3.5-turbo-0613", "gpt-4")

test_that("General working of rate_limits_per_minute().", {

  skip_on_cran()

  rate_limits_per_minute(model = models) |>
    nrow() |>
    expect_equal(2)


})

test_that("rate_limits_per_minute() casts NAs.", {

  skip_on_cran()

  rlpm <- rate_limits_per_minute(model = "gpt-3")

  expect_identical(rlpm$model, "Error 404 [check gpt model]")

})
