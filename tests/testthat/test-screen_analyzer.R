
result_obj <- AIscreenR:::result_object

test_that("returns tibble", {

  screen_perform <-
    result_obj |>
    screen_analyzer()

  expect_true(tibble::is_tibble(screen_perform))

  screen_perform <-
    result_obj$answer_data_sum |>
    screen_analyzer()

  expect_true(tibble::is_tibble(screen_perform))


})
