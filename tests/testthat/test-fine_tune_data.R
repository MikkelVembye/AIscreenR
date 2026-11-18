# Tests for create_fine_tune_data() (R/fine_tune_data.R)

dat <- data.frame(
  studyid  = c("a", "b"),
  title    = c("T1", "T2"),
  abstract = c("A1\n\nmore", "A2\nx"),
  stringsAsFactors = FALSE
)

test_that("create_fine_tune_data() builds the question and sets the class", {
  out <- create_fine_tune_data(
    data = dat, prompt = "Q?",
    studyid = studyid, title = title, abstract = abstract
  )

  expect_s3_class(out, "fine_tune_data")
  expect_named(out, c("studyid", "title", "abstract", "question"))
  expect_equal(levels(out$studyid), c("a", "b"))

  # "\n\n" becomes a space and a single "\n" is removed
  expect_equal(
    out$question[1],
    "Q? Now, evaluate the following title and abstract for Study a: -Title: T1 -Abstract: A1 more"
  )
  expect_equal(
    out$question[2],
    "Q? Now, evaluate the following title and abstract for Study b: -Title: T2 -Abstract: A2x"
  )
})

test_that("create_fine_tune_data() numbers the studies when studyid is missing", {
  out <- create_fine_tune_data(
    data = dat[, c("title", "abstract")], prompt = "Q?",
    title = title, abstract = abstract
  )

  expect_equal(levels(out$studyid), c("1", "2"))
  expect_match(out$question[2], "Study 2:", fixed = TRUE)
})
