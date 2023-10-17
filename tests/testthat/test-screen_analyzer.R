
result_obj <- AIscreenR:::result_object

y <- 1
class(y) <- "somethingelse"

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

test_that("Stop functions work.", {

  expect_error(

    screen_analyzer(y)

  )

  expect_error(

    screen_perform <-
      result_obj$answer_data_sum |>
      screen_analyzer(human_decision = human_name)

  )

})
