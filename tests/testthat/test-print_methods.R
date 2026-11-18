# Tests for print.gpt(), print.chatgpt() and print.groq()

printed <- function(x) paste(capture.output(print(x)), collapse = "\n")

df <- data.frame(a = 1)

# ---- print.gpt() -----------------------------------------------------------

test_that("print.gpt() points to the aggregated data when it exists", {
  x <- structure(list(answer_data = df, answer_data_aggregated = df), class = "gpt")
  expect_output(print(x), "result_object$answer_data_aggregated", fixed = TRUE)
})

test_that("print.gpt() points to answer_data when there is no aggregated data", {
  x <- structure(list(answer_data = df), class = "gpt")
  out <- printed(x)

  expect_match(out, "result_object$answer_data", fixed = TRUE)
  expect_false(grepl("aggregated", out))
})

# ---- print.chatgpt() -------------------------------------------------------
# Four cases: with/without error_data, and with/without a prompt_tokens column.

make_chatgpt <- function(tokens, errors) {
  all_data <- if (tokens) data.frame(a = 1, prompt_tokens = 10) else df
  x <- list(answer_data_all = all_data, answer_data_sum = df)
  if (errors) x$error_data <- df
  structure(x, class = "chatgpt")
}

test_that("print.chatgpt() shows price and error data when both exist", {
  out <- printed(make_chatgpt(tokens = TRUE, errors = TRUE))

  expect_match(out, "answer_data_all", fixed = TRUE)
  expect_match(out, "answer_data_sum", fixed = TRUE)
  expect_match(out, "price_dollar", fixed = TRUE)
  expect_match(out, "error_data", fixed = TRUE)
})

test_that("print.chatgpt() shows only the price when there is no error data", {
  out <- printed(make_chatgpt(tokens = TRUE, errors = FALSE))

  expect_match(out, "price_dollar", fixed = TRUE)
  expect_false(grepl("error_data", out, fixed = TRUE))
})

test_that("print.chatgpt() shows only error data when there are no tokens", {
  out <- printed(make_chatgpt(tokens = FALSE, errors = TRUE))

  expect_match(out, "error_data", fixed = TRUE)
  expect_false(grepl("price_dollar", out, fixed = TRUE))
})

test_that("print.chatgpt() shows only the answer data in the simplest case", {
  out <- printed(make_chatgpt(tokens = FALSE, errors = FALSE))

  expect_match(out, "answer_data_all", fixed = TRUE)
  expect_match(out, "answer_data_sum", fixed = TRUE)
  expect_false(grepl("price_dollar", out, fixed = TRUE))
  expect_false(grepl("error_data", out, fixed = TRUE))
})

# ---- print.groq() ----------------------------------------------------------

test_that("print.groq() shows the simple message and returns x invisibly", {
  x <- structure(list(answer_data = df), class = "groq")

  out <- NULL
  capture.output(out <- withVisible(print(x)))

  expect_false(out$visible)
  expect_identical(out$value, x)

  txt <- printed(x)
  expect_match(txt, "`x$answer_data`", fixed = TRUE)
  expect_false(grepl("aggregated", txt))
  expect_false(grepl("Total screening cost", txt))
  expect_false(grepl("error(s)", txt, fixed = TRUE))
})

test_that("print.groq() shows aggregated data, price and number of errors", {
  x <- structure(
    list(
      answer_data            = df,
      answer_data_aggregated = df,
      price_dollar           = 1.23456,
      error_data             = data.frame(a = 1:2)
    ),
    class = "groq"
  )
  txt <- printed(x)

  expect_match(txt, "`x$answer_data_aggregated`", fixed = TRUE)
  expect_match(txt, "Total screening cost: $1.2346", fixed = TRUE)
  expect_match(txt, "x$price_data", fixed = TRUE)
  expect_match(txt, "2 error(s)", fixed = TRUE)
})

test_that("print.groq() ignores NULL aggregated and error data", {
  x <- structure(
    list(answer_data = df, answer_data_aggregated = NULL, error_data = NULL),
    class = "groq"
  )
  txt <- printed(x)

  expect_false(grepl("aggregated", txt))
  expect_false(grepl("error(s)", txt, fixed = TRUE))
})
