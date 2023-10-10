path <- system.file("extdata", "word_pormpt_1.docx", package = "AIscreenR")

prompt <-
  readtext::readtext(path)$text |>
  stringr::str_remove_all("\n")


test_that("Print expectation for chatgpt object.", {

  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_gpt(
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
  expect_s3_class(test_obj, "chatgpt")
  expect_s3_class(test_obj, "list")

  print(test_obj) |> expect_output("object_name\\$answer_data_all")
  print(test_obj) |> expect_output("object_name\\$answer_data_sum")
  print(test_obj) |> expect_output("object_name\\$price_dollor")

  expect_equal(
    capture.output(test_obj)[8],
    " object_name$price_dollor"
  )

  # Print expectation

  #print(test_obj) |> expect_output("object_name\\$error_data")

  expect_message(

    test_obj <- tabscreen_gpt(
      data = filges2015_dat[c(1:2),],
      prompt = c(prompt),
      studyid = studyid, # indicate the variable with the studyid in the data
      title = title, # indicate the variable with the titles in the data
      abstract = abstract,
      api_key = 1234
    )
  ) |>
    suppressMessages()

  print(test_obj) |> expect_output("object_name\\$error_data")

  expect_equal(
    capture.output(test_obj)[11],
    " object_name$error_data"
  )


})
