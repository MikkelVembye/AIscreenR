
paths <- system.file("extdata", c("word_pormpt_1.docx", "word_pormpt_2.docx"), package = "AIscreenR")

prompts <-
  purrr::map_chr(
    paths, ~ {
      readtext::readtext(.x)$text |>
        stringr::str_remove_all("\n")
    }
  )

prompt <- prompts[1]

test_that("approximate_price_gpt() takes multiple inputs", {

  app_obj <- approximate_price_gpt(
    data = filges2015_dat,
    prompt = prompts,
    studyid = studyid,
    title = title,
    abstract = abstract,
    model = c("gpt-3.5-turbo-0613", "gpt-4"),
    reps = c(10, 1),
    top_p = c(0.2, 1)
  )

  expect_output(print(app_obj), "\\$30\\.2902\\.")

  gpt_price_ratio <- app_obj$price_data$input_price_dollar[2]/app_obj$price_data$input_price_dollar[1]

  expect_equal(gpt_price_ratio, 2L, tolerance = .01)

  app_obj <- approximate_price_gpt(
    data = filges2015_dat[c(1:20),],
    prompt = prompt,
    #studyid = studyid,
    title = title,
    abstract = abstract,
    model = c("gpt-3.5-turbo-0613", "gpt-4"),
    reps = 1
  )

  expect_output(print(app_obj), "\\$0\\.7598\\.")

  gpt_price_ratio <- app_obj$price_data$input_price_dollar[2]/app_obj$price_data$input_price_dollar[1]

  expect_equal(gpt_price_ratio, 20L, tolerance = .01)

})

test_that("approximate_price_gpt() error structure.", {

  expect_error(
    app_obj <- approximate_price_gpt(
      data = filges2015_dat[c(1:20),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-3.5turbo-0613", "gpt-4")
    )
  )

  expect_error(
    app_obj <- approximate_price_gpt(
      data = filges2015_dat[c(1:20),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-3.5-turbo-0613", "gpt-4"),
      reps = c(10, 1, 2)
    )
  )

  expect_error(
    app_obj <- approximate_price_gpt(
      data = filges2015_dat[c(1:20),],
      prompt = c(prompt, prompt),
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("gpt-3.5-turbo-0613", "gpt-4"),
      reps = c(10, 1)
    )
  )

})

