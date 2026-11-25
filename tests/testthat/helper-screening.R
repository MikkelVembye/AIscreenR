# Helpers for the screening app tests

# A project with `n` records in a temporary folder
local_screening_project <- function(n = 4, env = parent.frame()) {
  path <- file.path(withr::local_tempdir(.local_envir = env), "review.rds")
  refs <- data.frame(title = paste("Title", seq_len(n)), abstract = paste("Abstract", seq_len(n)))
  .create_screening_project(refs, "test.ris", path)
  withr::local_options(AIscreenR.screening_project = path, .local_envir = env)
  path
}

# Let both reviewers screen everything and finish
screen_all <- function(path, human1, human2) {
  .update_screening_project(path, function(p) {
    for (i in seq_along(human1)) p <- .record_decision(p, "human1", i, human1[i])
    for (i in seq_along(human2)) p <- .record_decision(p, "human2", i, human2[i])
    .finish_screening(.finish_screening(p, "human1"), "human2")
  })
}

# AI answers shaped like the answer_data from tabscreen_gpt()
fake_ai_answers <- function(decisions) {
  answers <- tibble::tibble(
    studyid = seq_along(decisions), promptid = 1L, model = "gpt-6-luna",
    iterations = 1, decision_gpt = as.character(decisions), decision_binary = decisions
  )
  class(answers) <- c("gpt_tbl", class(answers))
  answers
}

# Run background jobs in this process, so the mocks apply to them
local_sequential_plan <- function(env = parent.frame()) {
  old <- future::plan(future::sequential)
  withr::defer(future::plan(old), envir = env)
}

# What the buttons on the Screen and Reconcile tabs send. `nonce` makes two equal
# clicks count as two clicks, as in the browser.
click <- local({
  nonce <- 0
  function(session, action, key, note = NULL, input = "screen_action") {
    nonce <<- nonce + 1
    value <- list(action = action, key = as.character(key), note = note, nonce = nonce)
    do.call(session$setInputs, stats::setNames(list(value), input))
  }
})

app_dir <- function() system.file("app", package = "AIscreenR")

skip_if_no_app <- function() {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("future")
}

skip_if_no_excel <- function() {
  testthat::skip_if_not_installed("writexl")
  testthat::skip_if_not_installed("readxl")
}
