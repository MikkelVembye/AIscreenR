path <- system.file("extdata", "word_pormpt_1.docx", package = "AIscreenR")

prompt <-
  readtext::readtext(path)$text |>
  stringr::str_remove_all("\n")


test_that("Print expectation for chatgpt object.", {

  skip_on_cran()

  expect_message(

    random_name <- tabscreen_gpt(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gpt-3.5-turbo-0613",
      reps = 1,
      max_tries = 0
    )

  )

  # class check
  expect_s3_class(random_name, "chatgpt")
  expect_s3_class(random_name, "list")

  expect_output(print(random_name), "random_name")


  expect_message(

    random_name <- tabscreen_gpt(
      data = filges2015_dat[c(1:2),],
      prompt = c(prompt),
      studyid = studyid, # indicate the variable with the studyid in the data
      title = title, # indicate the variable with the titles in the data
      abstract = abstract,
      api_key = 1234
    )
  ) |>
    suppressMessages()


  expect_output(print(random_name), "random_name\\$error_data")


})
