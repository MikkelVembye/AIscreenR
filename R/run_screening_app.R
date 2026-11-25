#' @title Double screen titles and abstracts in a Shiny app
#'
#' @description
#' `r lifecycle::badge("experimental")`<br>
#' <br>
#' Opens a local Shiny app for double screening the titles and abstracts of a RIS
#' file. Two human reviewers (`human1` and `human2`) and an OpenAI GPT model
#' screen every record.
#'
#' @param project Character string with the path to the project file, ending in
#'   `.rds`. If the file exists, the app continues that project. Otherwise, the
#'   file is created when a RIS file is uploaded in the app. An existing file is
#'   never replaced by an upload.
#' @param workers Number of R processes that work at the same time during AI
#'   screening (requests sent at once) and during the [solve_or_guess()]
#'   analysis. Default is the number of available cores minus one. Each worker uses
#'   memory, and more requests at once can reach the rate limit of your OpenAI
#'   account (see [rate_limits_per_minute()]). Use a lower number if that
#'   happens, or `1` to send one request at a time.
#' @param ... Further arguments passed to `shiny::runApp()`.
#'
#' @details The tabs of the app are used in this order:
#' \enumerate{
#'   \item **Upload** a RIS file and create the project.
#'   \item **Setup**: write the screening question and choose the model.
#'   \item **Screen**: the two reviewers take turns on the same computer. Use
#'     "Hand over" before the other reviewer takes over, and "I am finished" after
#'     the last record. Answers are locked when both have finished.
#'   \item **AI**: screen the records with [tabscreen_gpt()]. This costs money and
#'     can run while the reviewers screen. Uncertain AI answers count as include.
#'   \item **Reconcile**: the reviewers settle the records they disagree on.
#'   \item **Compare**: the AI against the final human decision, using
#'     [screen_analyzer()], and the three original answers, using [solve_or_guess()].
#'   \item **Export** an Excel file, or a RIS file with one note line per record.
#'     The Excel file has the sheets `answer_data` (the AI answers as returned by
#'     [tabscreen_gpt()]) and `decisions` (human, AI and final decisions).
#' }
#' When both reviewers have finished, all disagreements are settled and the AI has
#' answered, the AI, Reconcile and Export tabs offer a [report()] of the records
#' where the AI differs from the final decision, as HTML or Word. This needs
#' Quarto and the package `quarto`.
#'
#' The total price of the AI screening is shown on the AI tab and printed in the
#' R console. It is calculated from the tokens used and the package's price list,
#' and may differ from the `price_dollar` of [tabscreen_gpt()], which rounds each
#' request.
#' The reviewers do not see each other's answers or the AI answers in the app
#' while screening. The project file itself is not encrypted. The API key is not
#' saved in the project file.
#'
#' The app requires the packages `shiny`, `DT`, `future` and `writexl`.
#'
#' @return No return value, called to start the app.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_screening_app("my_review.rds")
#'
#' # Send at most 4 requests at once
#' run_screening_app("my_review.rds", workers = 4)
#' }
run_screening_app <- function(project, workers = max(1, future::availableCores() - 1), ...) {

  if (missing(project) || !is.character(project) || length(project) != 1 ||
      !grepl("\\.rds$", project, ignore.case = TRUE)) {
    stop("`project` must be a file path ending in \".rds\", e.g. \"my_review.rds\".", call. = FALSE)
  }

  if (!dir.exists(dirname(project))) {
    stop("The folder \"", dirname(project), "\" does not exist.", call. = FALSE)
  }

  rlang::check_installed(c("shiny", "DT", "future", "writexl"), reason = "to run the screening app.")

  if (!is.numeric(workers) || length(workers) != 1 || is.na(workers) ||
      workers < 1 || workers != round(workers)) {
    stop("`workers` must be a whole number of at least 1.", call. = FALSE)
  }

  # Use the full path, because Shiny changes the working directory while the app runs
  project <- file.path(normalizePath(dirname(project), winslash = "/"), basename(project))

  # Fail before the app opens if the file is not a screening project
  if (file.exists(project)) .load_screening_project(project)

  old_options <- options(
    AIscreenR.screening_project = project,
    shiny.maxRequestSize = 50 * 1024^2 # Shiny's default upload limit is 5 MB
  )
  on.exit(options(old_options), add = TRUE)

  # Two levels of background R sessions. The first keeps the app responsive while
  # the AI screening and the analysis run. The second is what tabscreen_gpt() and
  # solve_or_guess() use to work in parallel inside such a background job.
  # I() tells future to use the number even though it is more than it would pick
  # inside a background session.
  parallel_level <- if (workers > 1) {
    future::tweak(future::multisession, workers = I(workers))
  } else {
    future::sequential
  }
  old_plan <- future::plan(list(
    future::tweak(future::multisession, workers = 2),
    parallel_level
  ))
  on.exit(future::plan(old_plan), add = TRUE)

  shiny::runApp(system.file("app", package = "AIscreenR"), ...)
}
