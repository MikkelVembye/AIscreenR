
paths <- system.file("extdata", "test_prompts.rds", package = "AIscreenR")
prompts <- readRDS(file = paths)

prompt <- prompts[1]

test_that("approximate_price_gpt() takes multiple inputs", {


  app_obj <- approximate_price_gpt(
    data = filges2015_dat[c(1:20),],
    prompt = prompts,
    studyid = studyid,
    title = title,
    abstract = abstract,
    model = c("gpt-4o", "gpt-4"),
    reps = c(2, 1),
    top_p = c(0.2, 1)
  )

  expect_output(print(app_obj), "\\$3\\.7862\\.")
  expect_equal(app_obj$price_dollar, 3.7862, tolerance = 0.1)

  obj_names_length <- app_obj |> names() |> length()
  expect_equal(obj_names_length, 2)

  data_var_length <- app_obj$price_data |> names() |> length()
  expect_equal(data_var_length, 6)


  app_obj <- approximate_price_gpt(
    data = filges2015_dat[c(1:20),],
    prompt = prompt,
    #studyid = studyid,
    title = title,
    abstract = abstract,
    model = c("gpt-4o", "gpt-4"),
    reps = 1
  )

  # Remember to update when prices changes
  price_ratio <- app_obj$price_data$total_price_dollar[2]/app_obj$price_data$total_price_dollar[1]
  expect_equal(price_ratio, 6, tolerance = 0.1)

})

test_that("approximate_price_gpt() error structure.", {

  #skip()
  #skip_on_ci()

  # UPDATE
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
      model = c("gpt-4o", "gpt-4"),
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
      model = c("gpt-4o", "gpt-4"),
      reps = c(10, 1)
    )
  )

})

