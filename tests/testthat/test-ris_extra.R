write_ris <- function(lines, bytes = NULL) {
  path <- withr::local_tempfile(fileext = ".ris", .local_envir = parent.frame())
  if (is.null(bytes)) writeLines(lines, path, useBytes = TRUE) else writeBin(bytes, path)
  path
}

test_that("read_ris_to_dataframe() handles continuation lines, repeated tags and a missing ER", {
  path <- write_ris(c(
    "TY  - JOUR",
    "TI  - A long title",
    "      that continues",
    "AU  - One, A",
    "AU  - Two, B",
    "      Jr.",
    "AB  - Abstract text",
    "KW  - alpha",
    "KW  - beta",
    "ER  - ",
    "TY  - BOOK",
    "TI  - No end marker"
  ))
  df <- read_ris_to_dataframe(path)
  expect_equal(nrow(df), 2)
  expect_equal(df$title, c("A long title that continues", "No end marker"))
  expect_equal(df$author[1], "One, A; Two, B Jr.")
})

test_that("read_ris_to_dataframe() returns an empty data frame for empty files", {
  expect_equal(nrow(read_ris_to_dataframe(write_ris(""))), 0)
})

test_that("read_ris_to_dataframe() repairs invalid UTF-8 bytes", {
  bytes <- c(
    charToRaw("TY  - JOUR\nTI  - Caf"), as.raw(0xE9),
    charToRaw(" culture\nER  - \n")
  )
  df <- read_ris_to_dataframe(write_ris(bytes = bytes))
  expect_equal(df$title, "Caf\u00e9 culture")

  # Undefined Windows-1252 bytes vary across iconv implementations.
  bytes <- c(
    charToRaw("TY  - JOUR\nTI  - Price "),
    as.raw(0x80),
    charToRaw(" 10\nER  - \n")
  )
  expect_equal(read_ris_to_dataframe(write_ris(bytes = bytes))$title, "Price \u20ac 10")
})

test_that("read_ris_to_dataframe() warns about bytes that cannot be recovered", {
  local_mocked_bindings(
    .repair_invalid_utf8_line = \(line, ...) line
  )
  bytes <- c(charToRaw("TY  - JOUR\nTI  - Bad "), as.raw(0xFF), charToRaw(" byte\nER  - \n"))
  expect_snapshot(df <- read_ris_to_dataframe(write_ris(bytes = bytes)))
  expect_equal(nrow(df), 1)
})

test_that(".repair_invalid_utf8_line() only touches invalid bytes", {
  line <- rawToChar(c(charToRaw("na"), as.raw(0xEF), charToRaw("ve \u00fc")))
  expect_equal(.repair_invalid_utf8_line(line), "na\u00efve \u00fc")
  expect_equal(.repair_invalid_utf8_line(rawToChar(c(charToRaw("a"), as.raw(0xC3)))), "a\u00c3")
  expect_equal(.repair_invalid_utf8_line("plain"), "plain")
  expect_equal(.repair_invalid_utf8_line(rawToChar(as.raw(c(0x61, 0xFF))), candidate_encodings = character()), "a")
  expect_equal(.utf8_lead_len(as.raw(0xF0)), 4L)
  expect_equal(.utf8_lead_len(as.raw(0xE2)), 3L)
  expect_equal(.utf8_lead_len(as.raw(0x80)), 0L)
})

test_that("save_dataframe_to_ris() preserves the original file when unchanged", {
  path <- write_ris(c(
    "TY  - JOUR", "AU  - One, A", "AU  - Two, B", "TI  - Title one", "PY  - 2020", "ER  - ",
    "", "TY  - JOUR", "TI  - Title two", "ER  - "
  ))
  df <- read_ris_to_dataframe(path)
  out <- withr::local_tempfile(fileext = ".ris")
  expect_output(save_dataframe_to_ris(df, out), "RIS file saved")
  expect_equal(read_ris_to_dataframe(out)$title, df$title)

  lines <- readLines(out)
  expect_equal(sum(lines == "AU  - One, A"), 1)
  expect_equal(sum(lines == "ER  - "), 2)
})

test_that("save_dataframe_to_ris() falls back to current values after edits", {
  path <- write_ris(c(
    "TY  - JOUR", "AU  - One, A", "AU  - Two, B", "TI  - Title one", "AB  - Abstract; with semicolon", "ER  - ",
    "", "TY  - JOUR", "TI  - Title two", "ER  - "
  ))
  df <- read_ris_to_dataframe(path)
  df$title[1] <- "Edited title"
  df$extra <- c("x", NA)
  out <- withr::local_tempfile(fileext = ".ris")
  expect_output(save_dataframe_to_ris(df, out), "RIS file saved")

  lines <- readLines(out)
  expect_true("TI  - Edited title" %in% lines)
  expect_true("AU  - Two, B" %in% lines)
  expect_true("AB  - Abstract; with semicolon" %in% lines)
  expect_true("extra  - x" %in% lines)
})

test_that("save_dataframe_to_ris() writes plain data frames with repeated and list columns", {
  df <- data.frame(
    type = c("JOUR", NA),
    author = c("A, One; B, Two", "C, Three"),
    author2 = c("D, Four", ""),
    keywords = c("k1; k2", "k3"),
    stringsAsFactors = FALSE
  )
  df$sources <- list(c("s1", "s2"), "s3")
  df$fac <- factor(c("f1", "f2"))
  out <- withr::local_tempfile(fileext = ".ris")
  expect_output(save_dataframe_to_ris(df, out), "RIS file saved")
  lines <- readLines(out)
  expect_true(all(c("AU  - A, One", "AU  - B, Two", "AU  - D, Four") %in% lines))
  expect_true("fac  - f2" %in% lines)
})

test_that("RIS tag helpers resolve and reverse-map columns", {
  rev_map <- .get_reverse_ris_tag_map()
  expect_equal(.resolve_ris_tag("TI", 1), "TI")
  expect_equal(.resolve_ris_tag("title", 1, reverse_tag_map = rev_map), "TI")
  expect_equal(.resolve_ris_tag("author3", 1, reverse_tag_map = rev_map), "AU")
  expect_equal(.resolve_ris_tag("unknown_col", 1, reverse_tag_map = rev_map), "unknown_col")
  expect_equal(.resolve_ris_tag("x", 2, tag_meta = list(x = c("AB", "N1"))), "N1")
  expect_equal(.resolve_ris_tag("x", 2, tag_meta = list(x = c("AB", NA))), "x")

  df <- data.frame(title = "T", author = "A", author2 = "B", other = "O", stringsAsFactors = FALSE)
  rev <- .reverse_map_ris_tags(df)
  expect_equal(rev$AU, "A; B")
  expect_equal(rev$TI, "T")
  expect_equal(rev$other, "O")

  df2 <- data.frame(author = c("A", ""), author2 = c("", "B"), stringsAsFactors = FALSE)
  expect_equal(.reverse_map_ris_tags(df2)$AU, c("A", "B"))
})

test_that(".map_ris_tags() collapses repeated tags into numbered columns", {
  df <- data.frame(
    AB = c("a1", ""), N2 = c("a2", "b2"), TI = c("", "t"), T1 = c("tfb", ""),
    stringsAsFactors = FALSE
  )
  mapped <- .map_ris_tags(df)
  expect_equal(mapped$abstract, c("a1", "b2"))
  expect_equal(mapped$abstract2, c("a2", ""))
  expect_equal(mapped$title, c("tfb", "t"))
  expect_equal(attr(mapped, "ris_tag_used")$abstract, c("AB", "N2"))
})

test_that(".map_ris_tags() labels U1 columns, fills years and falls back for titles", {
  numeric_u1 <- .map_ris_tags(data.frame(U1 = c("1", "22"), stringsAsFactors = FALSE))
  expect_named(numeric_u1, "eppi_id")
  text_u1 <- .map_ris_tags(data.frame(U1 = c("x", "22"), stringsAsFactors = FALSE))
  expect_named(text_u1, "custom_info")
  expect_named(.map_ris_tags(data.frame(U1 = c(NA, ""), stringsAsFactors = FALSE)), "custom_info")

  years <- .map_ris_tags(data.frame(Y1 = c("2019/05/01", "2020"), stringsAsFactors = FALSE))
  expect_equal(years$year, c(2019L, 2020L))

  both <- .map_ris_tags(data.frame(PY = c("", "2021"), Y1 = c("2018///", "2020"), stringsAsFactors = FALSE))
  expect_equal(both$year, c(2018L, 2021L))

  titles <- .map_ris_tags(data.frame(TI = c("", "Main"), TT = c("Translit", "Other"), stringsAsFactors = FALSE))
  expect_equal(titles$title, c("Translit", "Main"))
  only_fallback <- .map_ris_tags(data.frame(FT = "Foreign", stringsAsFactors = FALSE))
  expect_equal(only_fallback$title, "Foreign")
})

test_that("read_ris_to_dataframe() merges repeated tags that appear in multiple lines of one record", {
  path <- write_ris(c(
    "TY  - JOUR", "AB  - first", "N2  - second", "KW  - k1", "KW  - k2", "KW  - k3",
    "  continued keyword", "ER  - "
  ))
  df <- read_ris_to_dataframe(path)
  expect_equal(df$abstract, "first")
  expect_equal(df$abstract2, "second")
  expect_equal(df$keywords, "k1; k2; k3 continued keyword")
})

test_that("save_dataframe_to_ris() skips raw tags that are empty or unknown", {
  path <- write_ris(c("TY  - JOUR", "TI  - T1", "AU  - A", "ER  - ", "", "TY  - JOUR", "TI  - T2", "ER  - "))
  df <- read_ris_to_dataframe(path)
  attr(df, "ris_record_order")[[1]] <- c("TY", "ZZ", "TI", "AU", "ER")
  out <- withr::local_tempfile(fileext = ".ris")
  expect_output(save_dataframe_to_ris(df, out), "saved")
  lines <- readLines(out)
  expect_false(any(grepl("^ZZ", lines)))
  expect_equal(sum(lines == "ER  - "), 2)
})

