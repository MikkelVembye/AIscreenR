# Tests for the screening app: R/run_screening_app.R,
# R/misc.func.hidden.screening_app.R and inst/app/app.R

ris_file <- function(lines, env = parent.frame()) {
  file <- withr::local_tempfile(fileext = ".ris", .local_envir = env)
  writeLines(lines, file)
  file
}

two_records <- c(
  "TY  - JOUR", "TI  - First title", "AB  - First abstract", "ER  - ", "",
  "TY  - JOUR", "TI  - Second title", "ER  - "
)

test_that(".read_screening_ris() reads a good file", {
  res <- .read_screening_ris(ris_file(two_records))

  expect_equal(nrow(res$refs), 2)
  expect_length(res$warnings, 0)
})

test_that(".read_screening_ris() gives a clear error for unusable files", {
  expect_error(.read_screening_ris(ris_file(character(0))), "No records found")
  expect_error(.read_screening_ris(ris_file(c("just", "text"))), "No records found")
  expect_error(
    .read_screening_ris(ris_file(c("TY  - JOUR", "AB  - text", "ER  - "))),
    "No titles found"
  )
})

test_that(".screening_records() numbers the records and fills missing text with \"\"", {
  records <- .screening_records(.read_screening_ris(ris_file(two_records))$refs)

  expect_equal(records$studyid, 1:2)
  expect_equal(records$title, c("First title", "Second title"))
  expect_equal(records$abstract, c("First abstract", ""))

  no_abstracts <- .screening_records(data.frame(title = c("A", NA)))
  expect_equal(no_abstracts$title, c("A", ""))
  expect_equal(no_abstracts$abstract, c("", ""))
})

test_that(".screening_preview() shortens long abstracts only", {
  preview <- .screening_preview(
    data.frame(title = c("A", "B"), abstract = c("short", strrep("x", 20))),
    max_chars = 10
  )

  expect_equal(preview$abstract, c("short", paste0(strrep("x", 10), "...")))
})

test_that("a project survives saving and loading, including the RIS information", {
  path <- file.path(withr::local_tempdir(), "review.rds")
  refs <- .read_screening_ris(system.file("extdata", "incl_tutorial.ris", package = "AIscreenR"))$refs

  .create_screening_project(refs, "incl_tutorial.ris", path)
  loaded <- .load_screening_project(path)

  # identical() also compares the attributes that RIS export depends on
  expect_identical(loaded$refs, refs)
  expect_equal(loaded$ris_name, "incl_tutorial.ris")
  expect_equal(list.files(dirname(path)), "review.rds") # No temporary file is left behind
})

test_that("saving again replaces the project file", {
  path <- file.path(withr::local_tempdir(), "review.rds")
  project <- .create_screening_project(data.frame(title = "A"), "a.ris", path)

  project$ris_name <- "changed.ris"
  .save_screening_project(project, path)

  expect_equal(.load_screening_project(path)$ris_name, "changed.ris")
})

test_that("an existing project is never replaced by a new one", {
  path <- file.path(withr::local_tempdir(), "review.rds")
  .create_screening_project(data.frame(title = "A"), "a.ris", path)

  expect_error(.create_screening_project(data.frame(title = "B"), "b.ris", path), "already exists")
  expect_equal(.load_screening_project(path)$ris_name, "a.ris")
})

test_that(".load_screening_project() rejects other files", {
  dir <- withr::local_tempdir()
  other_rds <- file.path(dir, "other.rds")
  saveRDS(mtcars, other_rds)
  text <- file.path(dir, "text.rds")
  writeLines("not an rds file", text)

  expect_error(.load_screening_project(other_rds), "not a screening project")
  expect_error(.load_screening_project(text), "not a screening project")
})

test_that("run_screening_app() checks the project path before starting", {
  skip_if_no_app()
  dir <- withr::local_tempdir()
  other <- file.path(dir, "other.rds")
  saveRDS(mtcars, other)
  testthat::local_mocked_bindings(runApp = function(...) stop("the app started"), .package = "shiny")

  expect_error(run_screening_app(), "ending in")
  expect_error(run_screening_app("review.ris"), "ending in")
  expect_error(run_screening_app(file.path(dir, "missing", "review.rds")), "does not exist")
  expect_error(run_screening_app(other), "not a screening project")
  expect_equal(nrow(readRDS(other)), nrow(mtcars)) # The other file is left alone
})

test_that("run_screening_app() passes the full path to the app and restores the options", {
  skip_if_no_app()
  local_sequential_plan() # Restored after the test
  withr::local_dir(withr::local_tempdir())
  withr::local_options(AIscreenR.screening_project = NULL, shiny.maxRequestSize = NULL)
  seen <- NULL
  testthat::local_mocked_bindings(
    runApp = function(appDir, ...) {
      seen <<- list(
        app = file.exists(file.path(appDir, "app.R")),
        project = getOption("AIscreenR.screening_project"),
        max_size = getOption("shiny.maxRequestSize"),
        background = inherits(future::plan(), "multisession")
      )
    },
    .package = "shiny"
  )

  run_screening_app("review.rds")

  expect_true(seen$app)
  expect_equal(seen$project, file.path(normalizePath(getwd(), winslash = "/"), "review.rds"))
  expect_equal(seen$max_size, 50 * 1024^2)
  expect_true(seen$background)
  expect_true(inherits(future::plan(), "sequential"))
  expect_null(getOption("AIscreenR.screening_project"))
  expect_null(getOption("shiny.maxRequestSize"))
})

test_that("run_screening_app() lets background jobs use `workers` processes", {
  skip_if_no_app()
  local_sequential_plan() # Restored after the test
  withr::local_dir(withr::local_tempdir())
  inside <- NULL
  testthat::local_mocked_bindings(
    # Ask a real background job how many workers it can use
    runApp = function(...) {
      # Suppresses "package 'future' was built under R version ..." from the job
      inside <<- suppressWarnings(future::value(future::future(future::nbrOfWorkers())))
    },
    .package = "shiny"
  )

  run_screening_app("review.rds", workers = 2)
  expect_equal(inside, 2, ignore_attr = TRUE)

  run_screening_app("review.rds", workers = 1)
  expect_equal(inside, 1, ignore_attr = TRUE)

  # The default is the available cores minus one
  testthat::local_mocked_bindings(availableCores = function(...) 4L, .package = "future")
  run_screening_app("review.rds")
  expect_equal(inside, 3, ignore_attr = TRUE)

  expect_length(future::plan("list"), 1) # The earlier plan is back
})

test_that("run_screening_app() rejects an unusable number of workers", {
  skip_if_no_app()
  testthat::local_mocked_bindings(runApp = function(...) stop("the app started"), .package = "shiny")
  path <- file.path(withr::local_tempdir(), "review.rds")

  for (bad in list(0, 1.5, NA_real_, "4", c(2, 3))) {
    expect_error(run_screening_app(path, workers = bad), "whole number")
  }
})

test_that("the app creates a project from an uploaded RIS file", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  path <- file.path(withr::local_tempdir(), "review.rds")
  withr::local_options(AIscreenR.screening_project = path)
  upload <- ris_file(two_records)

  shiny::testServer(system.file("app", package = "AIscreenR"), {
    session$setInputs(ris_file = list(datapath = upload, name = "mine.ris"))

    expect_match(output$summary$html, "2 records in mine.ris")
    expect_match(output$summary$html, "Records without an abstract: 1")
    expect_match(output$summary$html, "Create project")
    expect_false(file.exists(path)) # Nothing is saved before the button is clicked

    session$setInputs(create = 1)

    expect_true(file.exists(path))
    expect_equal(nrow(project()$refs), 2)
    expect_match(output$summary$html, "Saved in")
    expect_no_match(output$summary$html, "Create project")
  })
})

test_that("the app shows an error for an unusable file and saves nothing", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  path <- file.path(withr::local_tempdir(), "review.rds")
  withr::local_options(AIscreenR.screening_project = path)
  upload <- ris_file(c("just", "text"))

  shiny::testServer(system.file("app", package = "AIscreenR"), {
    session$setInputs(ris_file = list(datapath = upload, name = "bad.ris"))

    expect_match(output$summary$html, "No records found")
    expect_no_match(output$summary$html, "Create project")
    expect_false(file.exists(path))
  })
})

test_that("the app continues an existing project", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  path <- file.path(withr::local_tempdir(), "review.rds")
  withr::local_options(AIscreenR.screening_project = path)
  .create_screening_project(.read_screening_ris(ris_file(two_records))$refs, "old.ris", path)

  shiny::testServer(system.file("app", package = "AIscreenR"), {
    expect_match(output$summary$html, "2 records in old.ris")
    expect_no_match(output$summary$html, "Create project")
  })
})
