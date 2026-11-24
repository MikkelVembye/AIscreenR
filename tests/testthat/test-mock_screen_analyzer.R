screen_with_human <- function(..., reps = 1, decision = "1") {
  dat <- mock_dat(4)
  dat$human_code <- c(1, 0, 1, 0)
  local_mock_api(llm_handler("gpt", decision = decision), .env = parent.frame())
  suppressMessages(tabscreen_gpt(
    dat, prompt = "p", studyid = studyid, title = title, abstract = abstract,
    api_key = "k", progress = FALSE, model = "gpt-5-nano", max_tries = 1, reps = reps, ...
  ))
}

test_that("screen_analyzer() evaluates single screenings", {
  res <- screen_with_human()
  out <- screen_analyzer(res)
  expect_equal(out$recall, 1)
  expect_equal(out$specificity, 0)
  expect_equal(out$p_agreement, 0.5)
  expect_null(attr(out, "p_incl_data"))

  full <- screen_analyzer(res, key_result = FALSE)
  expect_true(all(c("precision", "npv", "bacc", "F2", "mcc", "irr") %in% names(full)))
})

test_that("screen_analyzer() evaluates repeated screenings", {
  res <- screen_with_human(reps = 2, incl_cutoff_upper = 0.8, incl_cutoff_lower = 0.2)
  out <- screen_analyzer(res)
  expect_equal(out$reps, 2)
  expect_equal(nrow(attr(out, "p_incl_data")), 10)

  expect_s3_class(screen_analyzer(res$answer_data_aggregated), "data.frame")
  tbl <- screen_analyzer(res$answer_data)
  expect_equal(tbl$reps, 2)

  agg <- screen_analyzer(res$answer_data_aggregated)
  expect_true("criteria" %in% names(agg))
})

test_that("screen_analyzer() reports one screening for aggregated single reps", {
  res <- screen_with_human(reps = 1)
  agg <- res$answer_data |>
    dplyr::mutate(final_decision_gpt_num = decision_binary, reps = iterations, incl_p = 1) |>
    tibble::new_tibble(class = "gpt_agg_tbl")
  attr(agg, "incl_cutoff_lower") <- 0.5
  out <- screen_analyzer(agg)
  expect_equal(out$criteria, "One screening used.")
})

test_that("screen_analyzer() accepts alternative human decision columns and errors clearly", {
  res <- screen_with_human()
  res$answer_data$expert <- res$answer_data$human_code
  expect_equal(screen_analyzer(res, human_decision = expert)$n_screened, 4)

  expect_error(screen_analyzer(res, human_decision = nothing), "human decision")
  expect_error(screen_analyzer("x"), "must be of either class")
})

test_that("screen_analyzer() handles legacy chatgpt objects", {
  dat <- tibble::tibble(
    studyid = 1:4, promptid = 1L, model = "gpt-4", reps = 1L,
    human_code = c(1, 0, 1, 0), final_decision_gpt_num = c(1, 0, 0, 0)
  )
  legacy_tbl <- structure(dat, class = c("chatgpt_tbl", class(dat)))
  expect_equal(screen_analyzer(legacy_tbl)$n_screened, 4)

  legacy <- structure(list(answer_data_sum = dat), class = "chatgpt")
  expect_equal(screen_analyzer(legacy)$recall, 0.5)
})

test_that("inherits helpers identify classes", {
  expect_true(is_chatgpt(structure(list(), class = "chatgpt")))
  expect_true(is_chatgpt_tbl(structure(list(), class = "chatgpt_tbl")))
  expect_true(is_gpt(structure(list(), class = "gpt")))
  expect_true(is_gpt_tbl(structure(list(), class = "gpt_tbl")))
  expect_true(is_gpt_agg_tbl(structure(list(), class = "gpt_agg_tbl")))
  expect_false(is_gpt(1))
})


test_that("print methods point to the result data", {
  expect_output(print(structure(list(answer_data = 1), class = "gpt")), "result_object\\$answer_data$")
  expect_output(
    print(structure(list(answer_data_aggregated = 1), class = "gpt")),
    "answer_data_aggregated"
  )

  groq_plain <- structure(list(answer_data = 1), class = "groq")
  expect_snapshot(print(groq_plain))

  groq_full <- structure(
    list(
      answer_data = 1, answer_data_aggregated = 1, price_dollar = 0.12345,
      error_data = data.frame(a = 1:2)
    ),
    class = "groq"
  )
  expect_snapshot(print(groq_full))
})

test_that("print.chatgpt() covers all combinations of errors and prices", {
  priced <- structure(list(answer_data_all = data.frame(prompt_tokens = 1)), class = "chatgpt")
  plain <- structure(list(answer_data_all = data.frame(x = 1)), class = "chatgpt")
  priced_err <- structure(c(unclass(priced), list(error_data = 1)), class = "chatgpt")
  plain_err <- structure(c(unclass(plain), list(error_data = 1)), class = "chatgpt")

  expect_output(print(priced_err), "price_dollar.*error_data")
  expect_output(print(priced), "price_dollar")
  expect_output(print(plain_err), "error_data")
  expect_output(print(plain), "answer_data_sum")
})

