# Tests for comparing with the AI and exporting in the screening app

# 12 records: the reviewers disagree on record 12, which is settled as include
local_finished_project <- function(env = parent.frame()) {
  path <- local_screening_project(12, env = env)
  human <- rep(c(1, 0, 0), 4)
  screen_all(path, human1 = human, human2 = replace(human, 12, 1))
  .update_screening_project(path, function(p) {
    p <- .record_final(p, 12, 1)
    .add_ai_answers(p, fake_ai_answers(replace(human, c(1, 2), c(0, 1))))
  })
  path
}

test_that(".screening_results() has one row per record with every decision", {
  p <- .load_screening_project(local_finished_project())
  res <- .screening_results(p)

  expect_equal(nrow(res), 12)
  expect_equal(res$human1[12], 0L)
  expect_equal(res$human2[12], 1L)
  expect_equal(res$final[12], 1L)
  expect_equal(res$ai[1:2], c(0L, 1L))
  expect_equal(.ai_disagreements(p)$studyid, c(1, 2, 12))
})

test_that(".ai_performance() compares the AI with the final decision", {
  p <- .load_screening_project(local_finished_project())
  performance <- .ai_performance(p)

  expect_equal(performance$n_screened, 12)
  # Final includes: records 1, 4, 7, 10 and 12. The AI missed 1 and 12.
  expect_equal(performance$recall, 3 / 5)

  p$final <- p$final[0, ]
  expect_null(.ai_performance(p)) # A disagreement is not settled
})

test_that("the Excel file has the answer_data and the decisions", {
  skip_if_no_excel()
  p <- .load_screening_project(local_finished_project())
  p$ai <- p$ai[rev(seq_len(nrow(p$ai))), ] # The sheet is sorted by studyid
  file <- withr::local_tempfile(fileext = ".xlsx")

  .write_screening_xlsx(p, file)

  expect_equal(readxl::excel_sheets(file), c("answer_data", "decisions"))
  answers <- readxl::read_excel(file, "answer_data")
  expect_named(answers, names(p$ai))
  expect_equal(answers$studyid, 1:12)
  expect_equal(answers$decision_binary, .ai_decisions(p))
  decisions <- readxl::read_excel(file, "decisions")
  expect_named(decisions, c("studyid", "title", "human1", "human1_note", "human2",
                            "human2_note", "ai", "ai_reason", "final"))
  expect_equal(decisions$final, .final_decisions(p))
})

test_that("the Excel file keeps special letters, line breaks and very long text", {
  skip_if_no_excel()
  path <- local_screening_project(2)
  p <- .record_decision(.load_screening_project(path), "human1", 1, 1, "æøå\nsecond line")
  p <- .record_decision(p, "human1", 2, 0, strrep("x", 40000))
  file <- withr::local_tempfile(fileext = ".xlsx")

  .write_screening_xlsx(p, file)

  expect_equal(readxl::excel_sheets(file), "decisions") # The AI has not run
  res <- readxl::read_excel(file, "decisions")
  expect_equal(res$human1_note[1], "æøå\nsecond line")
  expect_equal(nchar(res$human1_note[2]), 32767) # Excel's limit for a cell
})

test_that("the RIS export keeps the original fields and adds one note per record", {
  path <- file.path(withr::local_tempdir(), "review.rds")
  refs <- read_ris_to_dataframe(system.file("extdata", "incl_tutorial.ris", package = "AIscreenR"))
  p <- .create_screening_project(refs, "incl_tutorial.ris", path)
  p <- .record_decision(p, "human1", 1, 1, "æøå; two\nlines")
  p <- .record_decision(p, "human2", 1, 0)
  file <- withr::local_tempfile(fileext = ".ris")

  .write_screening_ris(p, file)
  back <- read_ris_to_dataframe(file)

  expect_equal(nrow(back), nrow(refs))
  for (column in names(refs)) expect_identical(back[[column]], refs[[column]])
  expect_equal(
    back$notes1[1],
    paste0("AIscreenR screening: human1=Include; human2=Exclude; ai=No answer; ",
           "final=Not settled; human1 note: æøå; two lines")
  )
  expect_match(back$notes1[2], "human1=No answer")
})

test_that("an existing note in the RIS file is kept", {
  ris <- withr::local_tempfile(fileext = ".ris")
  writeLines(c("TY  - JOUR", "TI  - A", "N1  - original note", "ER  - "), ris)
  path <- file.path(withr::local_tempdir(), "review.rds")
  p <- .create_screening_project(read_ris_to_dataframe(ris), "a.ris", path)
  file <- withr::local_tempfile(fileext = ".ris")

  .write_screening_ris(p, file)
  back <- read_ris_to_dataframe(file)

  expect_match(back$notes1, "original note")
  expect_match(back$notes1, "AIscreenR screening")
})

test_that("the report is only available when everything is decided and the AI has answered", {
  path <- local_screening_project(3)
  p <- .load_screening_project(path)
  expect_null(.report_info(p))

  p <- screen_all(path, human1 = c(1, 0, 1), human2 = c(1, 0, 0))
  expect_null(.report_info(.add_ai_answers(p, fake_ai_answers(c(0, 0, 1))))) # Not settled

  p <- .record_final(p, 3, 1)
  expect_null(.report_info(p)) # No AI answers

  info <- .report_info(.add_ai_answers(p, fake_ai_answers(c(0, 0)))) # 1 record missing
  expect_equal(info, list(disagreements = 1L, without_ai = 1L))
})

test_that(".report_data() holds the records where the AI differs from the final decision", {
  p <- .load_screening_project(local_finished_project())
  p$refs$author <- paste("Author", 1:12)
  p$ai$detailed_description <- replace(rep(NA_character_, 12), 1, "Not about the topic")

  data <- .report_data(p)

  expect_equal(data$studyid, c(1, 2, 12))
  expect_equal(data$human_code, c(1L, 0L, 1L))
  expect_equal(data$final_decision_gpt_num, c(0L, 1L, 0L))
  expect_equal(data$decision_gpt, c("Not about the topic", "Include", "Exclude"))
  expect_equal(data$abstract, paste("Abstract", c(1, 2, 12)))
  expect_equal(data$author, paste("Author", c(1, 2, 12)))
  expect_equal(data$model, rep("gpt-6-luna", 3))
})

test_that("the report is written as one self-contained HTML file and as Word", {
  skip_on_cran()
  skip_if_not(.quarto_available(), "Quarto is not installed")
  p <- .load_screening_project(local_finished_project())
  html <- withr::local_tempfile(fileext = ".html")
  docx <- withr::local_tempfile(fileext = ".docx")

  .write_screening_report(p, html, "html")
  .write_screening_report(p, docx, "docx")

  text <- paste(readLines(html, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_match(text, "AI screening disagreements")
  expect_match(text, "False exclusion")
  expect_match(text, "False inclusion")
  expect_match(text, "Title 12")
  expect_no_match(text, "AI_disagreements_files/", fixed = TRUE) # Needs no other files
  expect_equal(readBin(docx, "raw", 2), charToRaw("PK"))
})

test_that("the report buttons appear on three tabs only when the report may be shown", {
  skip_if_no_app()
  path <- local_screening_project(2)
  screen_all(path, human1 = c(1, 1), human2 = c(1, 0))
  .update_screening_project(path, function(p) {
    .add_ai_answers(.set_screening_setup(p, "q?", "gpt-6-luna", FALSE), fake_ai_answers(c(0, 0)))
  })
  testthat::local_mocked_bindings(
    .quarto_available = function() TRUE,
    .write_screening_report = function(project, file, format) writeLines(format, file)
  )
  buttons <- function(html, tab) grepl(paste0("report_html_", tab), html, fixed = TRUE)

  shiny::testServer(app_dir(), {
    # One disagreement between the reviewers is not settled yet
    expect_false(buttons(output$ai_panel$html, "ai"))
    expect_false(buttons(output$reconcile_panel$html, "reconcile"))
    expect_false(buttons(output$export_panel$html, "export"))
    expect_error(output$report_html_export)

    click(session, "include", 2, input = "reconcile_action")

    expect_true(buttons(output$ai_panel$html, "ai"))
    expect_true(buttons(output$reconcile_panel$html, "reconcile"))
    expect_true(buttons(output$export_panel$html, "export"))
    expect_match(output$ai_panel$html, "differs from the final decision: 2")
    expect_equal(readLines(output$report_html_ai), "html")
    expect_equal(readLines(output$report_docx_export), "docx")

    ai_running(TRUE)
    session$flushReact()
    expect_false(buttons(output$export_panel$html, "export"))
  })
})

test_that("the app says why there are no report buttons", {
  skip_if_no_app()
  path <- local_screening_project(2)
  screen_all(path, human1 = c(1, 0), human2 = c(1, 0))
  .update_screening_project(path, function(p) .add_ai_answers(p, fake_ai_answers(c(1, 0))))

  shiny::testServer(app_dir(), {
    expect_match(output$export_panel$html, "nothing to report")
  })

  .update_screening_project(path, function(p) .add_ai_answers(p, fake_ai_answers(c(0, 0))))
  testthat::local_mocked_bindings(.quarto_available = function() FALSE)

  shiny::testServer(app_dir(), {
    expect_match(output$export_panel$html, "Install Quarto")
    expect_no_match(output$export_panel$html, "report_html")
  })
})

test_that("the Compare tab shows performance and saves the analysis", {
  skip_if_no_app()
  path <- local_finished_project()
  local_sequential_plan()
  withr::local_options(AIscreenR.screening_B = 5)

  shiny::testServer(app_dir(), {
    expect_match(output$compare_panel$html, "answered 12 of 12")
    expect_match(output$performance, "recall")
    expect_match(output$analysis_panel$html, "Run analysis")

    suppressWarnings(suppressMessages({
      session$setInputs(run_analysis = 1)
      session$elapse(500)
    }))

    expect_false(analysis_running())
    expect_null(analysis_message())
    expect_equal(project()$analysis$B, 5)
    expect_match(output$analysis_panel$html, "Solving probabilities")
    expect_match(output$ratios, "ai / human1")
  })

  # The saved result is dropped when the AI answers change
  p <- .add_ai_answers(.load_screening_project(path), fake_ai_answers(1))
  expect_null(p$analysis)
})

test_that("the Export tab downloads both files and warns about missing parts", {
  skip_if_no_app()
  skip_if_no_excel()
  path <- local_screening_project(2)
  screen_all(path, human1 = c(1, 0), human2 = c(0, 0))

  shiny::testServer(app_dir(), {
    expect_match(output$export_panel$html, "1 disagreements are not settled")
    expect_match(output$export_panel$html, "2 records have no AI answer")

    expect_equal(nrow(readxl::read_excel(output$download_xlsx, "decisions")), 2)
    expect_equal(nrow(read_ris_to_dataframe(output$download_ris)), 2)
  })
})

test_that("nothing can be downloaded before both reviewers have finished", {
  skip_if_no_app()
  path <- local_screening_project(2)

  shiny::testServer(app_dir(), {
    expect_error(output$download_xlsx)
  })
})

test_that("a Next button appears when a step is done and opens the next tab", {
  skip_if_no_app()
  path <- local_screening_project(2)
  opened <- NULL
  testthat::local_mocked_bindings(
    updateNavbarPage = function(session, inputId, selected) opened <<- c(opened, selected),
    .package = "shiny"
  )
  has_next <- function(html, tab) grepl(paste0("go_", tab), html, fixed = TRUE)

  shiny::testServer(app_dir(), {
    expect_true(has_next(output$summary$html, "Setup"))
    expect_false(has_next(output$setup_panel$html, "Screen")) # No question yet

    session$setInputs(question = "q?", model = "gpt-6-luna", reasons = FALSE, save_setup = 1)
    expect_true(has_next(output$setup_panel$html, "Screen"))

    change_project(function(p) {
      for (i in 1:2) p <- .record_decision(p, "human1", i, 1)
      for (i in 1:2) p <- .record_decision(p, "human2", i, c(1, 0)[i])
      .finish_screening(.finish_screening(p, "human1"), "human2")
    })
    session$flushReact()
    expect_true(has_next(output$screen_panel$html, "AI"))
    expect_false(has_next(output$ai_panel$html, "Reconcile")) # AI not run
    expect_false(has_next(output$reconcile_panel$html, "Compare")) # Not settled
    expect_true(has_next(output$analysis_panel$html, "Export"))

    change_project(function(p) .add_ai_answers(.record_final(p, 2, 1), fake_ai_answers(c(1, 1))))
    session$flushReact()
    expect_true(has_next(output$ai_panel$html, "Reconcile"))
    expect_true(has_next(output$reconcile_panel$html, "Compare"))

    session$setInputs(go_Setup = 1)
    session$setInputs(go_Export = 1)
  })

  expect_equal(opened, c("Setup", "Export"))
})
