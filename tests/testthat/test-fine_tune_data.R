ft_dat <- function(n = 10) {
  dat <- tibble::tibble(
    id = seq_len(n),
    ti = paste("Title", seq_len(n)),
    ab = paste0("Abstract\n\n", seq_len(n), "\nwith breaks"),
    human = rep(c(1, 0), length.out = n)
  )
  create_fine_tune_data(dat, prompt = "Relevant?", studyid = id, title = ti, abstract = ab)
}

test_that("create_fine_tune_data() builds questions", {
  dat <- ft_dat()
  expect_s3_class(dat, "fine_tune_data")
  expect_equal(nrow(dat), 10)
  expect_match(dat$question[1], "^Relevant\\? Now, evaluate the following title and abstract for Study 1:")
  expect_false(any(grepl("\n", dat$question)))
  expect_false("question_raw" %in% names(dat))

  no_id <- create_fine_tune_data(
    tibble::tibble(ti = "T", ab = "A"), prompt = "p", title = ti, abstract = ab
  )
  expect_equal(as.character(no_id$studyid), "1")
})

test_that("create_fine_tune_data() builds the question and sets the class", {
  dat <- data.frame(
    studyid = c("a", "b"),
    title = c("T1", "T2"),
    abstract = c("A1\n\nmore", "A2\nx"),
    stringsAsFactors = FALSE
  )
  out <- create_fine_tune_data(
    data = dat, prompt = "Q?",
    studyid = studyid, title = title, abstract = abstract
  )

  expect_s3_class(out, "fine_tune_data")
  expect_named(out, c("studyid", "title", "abstract", "question"))
  expect_equal(levels(out$studyid), c("a", "b"))
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
  dat <- data.frame(
    title = c("T1", "T2"),
    abstract = c("A1", "A2"),
    stringsAsFactors = FALSE
  )
  out <- create_fine_tune_data(
    data = dat, prompt = "Q?", title = title, abstract = abstract
  )

  expect_equal(levels(out$studyid), c("1", "2"))
  expect_match(out$question[2], "Study 2:", fixed = TRUE)
})

test_that("save_fine_tune_data() writes jsonl", {
  dat <- ft_dat()
  dat$true_answer <- ifelse(dat$human == 1, "Include", "Exclude")
  file <- withr::local_tempfile(fileext = ".jsonl")
  save_fine_tune_data(dat, role_and_subject = "You screen studies.", file = file)

  lines <- readLines(file)
  expect_length(lines, 10)
  first <- jsonlite::fromJSON(lines[1])
  expect_equal(first$messages$role, c("system", "user", "assistant"))
  expect_equal(first$messages$content[1], "You screen studies.")
  expect_equal(first$messages$content[3], "Include")
})

test_that("save_fine_tune_data() takes the answer from a named column", {
  dat <- ft_dat()
  dat$human <- ifelse(dat$human == 1, "Include", "Exclude")
  file <- withr::local_tempfile(fileext = ".jsonl")
  save_fine_tune_data(dat, role_and_subject = "r", file = file, true_answer = human)
  expect_equal(jsonlite::fromJSON(readLines(file)[2])$messages$content[3], "Exclude")
})

test_that("save_fine_tune_data() writes a default file name", {
  withr::local_dir(withr::local_tempdir())
  dat <- ft_dat()
  dat$true_answer <- "Include"
  save_fine_tune_data(dat, role_and_subject = "r")
  expect_true(file.exists("fine_tune_data.jsonl"))
})

test_that("save_fine_tune_data() validates its input", {
  dat <- ft_dat()
  dat$true_answer <- "Include"
  expect_error(save_fine_tune_data(as.data.frame(dat), "r"), "fine_tune_data")
  expect_error(save_fine_tune_data(dat[1:3, ], "r"), "at least 10 rows")
  expect_error(save_fine_tune_data(ft_dat(), "r"), "true_answer")
  expect_error(save_fine_tune_data(dat, "r", file = "x.csv"), "jsonl")
})
