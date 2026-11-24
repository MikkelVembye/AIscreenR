report_dat <- function() {
  tibble::tibble(
    studyid = c(1, 2, 3, 4),
    title = c("T <1>", "T 2", "T 3", "T 4"),
    abstract = c("A 'one'", "A 2", "A 3", "A 4"),
    author = c("Au \"1\"", "Au 2", "Au 3", "Au 4"),
    year = c(2001, 2002, 2003, 2004),
    journal = c("J1", "J2", "J3", "J4"),
    submodel = "gpt-mock",
    run_date = "2026-01-01",
    longest_answer = c("Because 1", "Because 2", "Because 3", "Because 4"),
    mean_confidence = c(90, 80, 70, 60),
    human_code = c(1, 0, 1, 0),
    final_decision_gpt_num = c(0, 1, 1, 0)
  )
}

local_render <- function(env = parent.frame()) {
  local_mocked_bindings(quarto_render = \(...) invisible(NULL), .package = "quarto", .env = env)
}

make_report <- function(data = report_dat(), ..., dir = ".") {
  report(
    data = data, studyid = studyid, title = title, abstract = abstract,
    human_code = human_code, final_decision_gpt_num = final_decision_gpt_num,
    file = "rep", format = "html", document_title = "Doc", open = FALSE, directory = dir, ...
  )
}

test_that("report() writes a qmd with sections for disagreements and agreements", {
  skip_if_not_installed("quarto")
  local_render()
  withr::local_dir(withr::local_tempdir())

  out <- suppressMessages(make_report(gpt_answer = longest_answer, gpt_confidence = mean_confidence))
  expect_equal(basename(out), "rep.html")

  qmd <- paste(readLines("rep.qmd", warn = FALSE), collapse = "\n")
  expect_match(qmd, "Disagreements", fixed = TRUE)
  expect_match(qmd, "False exclusion", fixed = TRUE)
  expect_match(qmd, "False inclusion", fixed = TRUE)
  expect_match(qmd, "Agreements", fixed = TRUE)
  expect_match(qmd, "Because 1", fixed = TRUE)
  expect_match(qmd, "Confidence (GPT)**: 90", fixed = TRUE)
  expect_match(qmd, "&lt;1&gt;", fixed = TRUE)
  expect_match(qmd, "**Journal**: J1", fixed = TRUE)
  expect_match(qmd, "**Run date**: 2026-01-01", fixed = TRUE)
})

test_that("report() warns when no gpt_answer is given", {
  skip_if_not_installed("quarto")
  local_render()
  withr::local_dir(withr::local_tempdir())
  expect_snapshot(suppressMessages(make_report()))
})

test_that("report() picks a subtitle from the type of disagreement", {
  skip_if_not_installed("quarto")
  local_render()
  withr::local_dir(withr::local_tempdir())
  subtitle <- function(dat) {
    suppressWarnings(suppressMessages(make_report(dat)))
    grep("^subtitle", readLines("rep.qmd", warn = FALSE), value = TRUE)
  }
  d <- report_dat()

  d$human_code <- 1; d$final_decision_gpt_num <- 0
  expect_match(subtitle(d), "Included by humans, excluded by GPT")

  d$human_code <- 0; d$final_decision_gpt_num <- 1
  expect_match(subtitle(d), "Excluded by humans, included by GPT")

  d$human_code <- c(1, 0, 1, 0); d$final_decision_gpt_num <- c(0, 1, 0, 1)
  expect_match(subtitle(d), "Disagreement between humans and GPT")

  d$human_code <- c(1, 0, 1, 0); d$final_decision_gpt_num <- c(1, 0, 1, 0)
  expect_match(subtitle(d), "Full report")

  suppressWarnings(suppressMessages(make_report(d, document_subtitle = "Custom")))
  expect_match(grep("^subtitle", readLines("rep.qmd", warn = FALSE), value = TRUE), "Custom")
})

test_that("report() handles only-disagreement and only-agreement data, and sparse columns", {
  skip_if_not_installed("quarto")
  local_render()
  withr::local_dir(withr::local_tempdir())

  d <- report_dat()
  d$human_code <- 1; d$final_decision_gpt_num <- 0
  suppressWarnings(suppressMessages(make_report(d)))
  expect_match(paste(readLines("rep.qmd"), collapse = "\n"), "False exclusion", fixed = TRUE)

  d$human_code <- c(1, 1, 0, 0); d$final_decision_gpt_num <- c(1, 1, 0, 0)
  suppressWarnings(suppressMessages(make_report(d)))
  expect_false(any(grepl("Disagreements", readLines("rep.qmd"), fixed = TRUE)))

  d <- report_dat()[, c("studyid", "title", "abstract", "human_code", "final_decision_gpt_num")]
  d$human_code <- c(1, 0, 1, 0)
  d$final_decision_gpt_num <- c(NA, 1, 0, 0)
  suppressWarnings(suppressMessages(make_report(d)))
  expect_true(file.exists("rep.qmd"))
})

test_that("report() accepts full paths and appends the qmd extension", {
  skip_if_not_installed("quarto")
  local_render()
  tmp <- withr::local_tempdir()
  withr::local_dir(withr::local_tempdir())
  out <- suppressWarnings(suppressMessages(report(
    data = report_dat(), studyid = studyid, title = title, abstract = abstract,
    human_code = human_code, final_decision_gpt_num = final_decision_gpt_num,
    file = file.path(tmp, "full.qmd"), document_title = "Doc", open = FALSE, format = "docx"
  )))
  expect_equal(basename(out), "full.docx")
  expect_equal(normalizePath(dirname(out)), normalizePath(tmp))
  expect_true(file.exists(file.path(tmp, "full.qmd")))

  expect_snapshot(
    suppressMessages(report(
      data = report_dat(), studyid = studyid, title = title, abstract = abstract,
      human_code = human_code, final_decision_gpt_num = final_decision_gpt_num,
      file = file.path(tmp, "full.qmd"), format = "html", document_title = "Doc", open = FALSE,
      directory = withr::local_tempdir()
    )),
    transform = \(x) gsub("\\\\", "/", x)
  )
})

test_that("report() validates its input", {
  d <- report_dat()
  args <- list(
    data = d, studyid = quote(studyid), title = quote(title), abstract = quote(abstract),
    human_code = quote(human_code), final_decision_gpt_num = quote(final_decision_gpt_num),
    file = "rep", format = "html", document_title = "Doc"
  )
  call_report <- function(drop = NULL, ...) {
    a <- utils::modifyList(args[setdiff(names(args), drop)], list(...))
    do.call(report, a)
  }
  expect_error(call_report("data"), "'data' is missing")
  expect_error(call_report("studyid"), "'studyid' is missing")
  expect_error(call_report("title"), "'title' is missing")
  expect_error(call_report("abstract"), "'abstract' is missing")
  expect_error(call_report("file"), "'file' is missing")
  expect_error(call_report("document_title"), "'document_title' is missing")
  expect_error(call_report("human_code"), "'human_code' is missing")
  expect_error(call_report("final_decision_gpt_num"), "'final_decision_gpt_num' is missing")
  expect_error(call_report("format"), "'format' is missing")
  expect_error(call_report(format = "txt"), "Invalid format")
  expect_error(call_report(format = c("html", "pdf")), "Invalid format")
  expect_error(call_report(format = NA_character_), "Invalid format")
  expect_error(call_report(directory = file.path(tempdir(), "missing-dir")), "does not exist")
  expect_error(call_report(file = file.path(tempdir(), "missing-dir", "x")), "parsed from argument 'file'")

  big <- d
  big$abstract[1] <- strrep("a", 60000)
  expect_error(suppressWarnings(call_report(data = big, directory = withr::local_tempdir())), "too large")
})

test_that("report() refuses to overwrite an open report and can open the result", {
  skip_if_not_installed("quarto")
  local_render()
  withr::local_dir(withr::local_tempdir())
  suppressWarnings(suppressMessages(make_report()))
  file.create("rep.html")

  real_file <- base::file
  local_mocked_bindings(
    file = \(description, open = "", ...) if (identical(open, "r+")) stop("locked") else real_file(description, open, ...),
    .package = "base"
  )
  expect_error(
    suppressWarnings(suppressMessages(report(
      data = report_dat(), studyid = studyid, title = title, abstract = abstract,
      human_code = human_code, final_decision_gpt_num = final_decision_gpt_num,
      file = "rep", format = "html", document_title = "Doc", open = TRUE, directory = "."
    ))),
    "already open"
  )
})

test_that("report() opens the rendered file when asked to", {
  skip_if_not_installed("quarto")
  local_render()
  withr::local_dir(withr::local_tempdir())
  opened <- NULL
  if (.Platform$OS.type == "windows") {
    local_mocked_bindings(shell.exec = \(file) opened <<- file, .package = "base")
  } else {
    withr::local_options(browser = \(url) opened <<- url)
  }
  out <- suppressWarnings(suppressMessages(report(
    data = report_dat(), studyid = studyid, title = title, abstract = abstract,
    human_code = human_code, final_decision_gpt_num = final_decision_gpt_num,
    file = "rep", format = "html", document_title = "Doc", open = TRUE, directory = "."
  )))
  expect_equal(opened, out)
})

