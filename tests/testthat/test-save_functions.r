test_that("read_ris_to_dataframe parses RIS and preserves field order", {
  ris <- c(
    "TY  - JOUR",
    "AU  - Author, One",
    "AU  - Author, Two",
    "TI  - An example title",
    "PY  - 2020",
    "ER  - ",
    "",
    "TY  - CHAP",
    "TI  - Another title",
    "AU  - Author, Three",
    "ER  - "
  )
  tmp <- tempfile(fileext = ".ris")
  writeLines(ris, tmp, useBytes = TRUE)

  df <- read_ris_to_dataframe(tmp)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_equal(colnames(df), c("TY", "AU", "TI", "PY"))

  expect_equal(df$TY, c("JOUR", "CHAP"))
  expect_equal(df$AU[1], "Author, One; Author, Two")
  expect_equal(df$TI, c("An example title", "Another title"))
  expect_equal(df$PY, c("2020", ""))
})

test_that("save_dataframe_to_ris writes valid RIS and splits semicolons", {
  df <- data.frame(
    TY = c("JOUR", "CHAP"),
    AU = c("Author, One; Author, Two", "Author, Three"),
    TI = c("An example title", "Another title"),
    PY = c("2020", ""),
    stringsAsFactors = FALSE
  )

  tmp <- tempfile(fileext = ".ris")
  save_dataframe_to_ris(df, tmp)

  lines <- readLines(tmp, encoding = "UTF-8")

  # First record: TY first, AU split into multiple lines
  expect_equal(lines[1], "TY  - JOUR")
  expect_true(any(lines == "AU  - Author, One"))
  expect_true(any(lines == "AU  - Author, Two"))
  expect_true(any(lines == "TI  - An example title"))
  expect_true(any(lines == "PY  - 2020"))
  expect_true(any(lines == "ER  - "))

  # Second record presence and content
  idx2 <- which(lines == "TY  - CHAP")
  expect_length(idx2, 1L)
  expect_true(any(lines[(idx2 + 1):length(lines)] == "AU  - Author, Three"))
  expect_true(any(lines[(idx2 + 1):length(lines)] == "TI  - Another title"))
})

test_that("read after write round-trips data (normalized)", {
  df <- data.frame(
    TY = c("JOUR", "CHAP"),
    AU = c("Author, One; Author, Two", "Author, Three"),
    TI = c("An example title", "Another title"),
    PY = c("2020", ""),
    stringsAsFactors = FALSE
  )

  tmp <- tempfile(fileext = ".ris")
  save_dataframe_to_ris(df, tmp)
  df2 <- read_ris_to_dataframe(tmp)

  # Column order by first appearance in the file
  expect_equal(colnames(df2), c("TY", "AU", "TI", "PY"))
  # Values should match expected normalized representation
  expect_equal(df2$AU, c("Author, One; Author, Two", "Author, Three"))
  expect_equal(df2$TY, df$TY)
  expect_equal(df2$TI, df$TI)
  expect_equal(df2$PY, df$PY)
})