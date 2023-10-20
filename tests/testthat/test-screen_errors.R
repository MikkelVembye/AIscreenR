result_object <- AIscreenR:::result_object

skip <- TRUE

test_that("Errors are replaced", {

  if(skip) skip()
  skip_on_cran()

  rescreen_obj <- result_object |> screen_errors()

  expect_true(!"error_data" %in% names(rescreen_obj))
  expect_true(is_chatgpt(rescreen_obj))
  expect_true(is_chatgpt_tbl(rescreen_obj$answer_data_all))
  expect_true(is_chatgpt_tbl(rescreen_obj$answer_data_sum))

  errors <- rescreen_obj$answer_data_sum |> filter(n_mis_answers > 0) |> nrow()

  expect_identical(errors, 0L)

})

test_that("Left_join works in screen_error function", {

  if(skip) skip()
  skip_on_cran()

  rescreen_obj <- result_object |> screen_errors()

  rows <- rescreen_obj$answer_data_sum |>  nrow()

  expect_identical(rows, 810L)

})

# Show identical arguments
