#' @title Generic function to re-screen failed title and abstract requests.
#'
#' @description
#'  This is a generic function to re-screen failed title and abstract requests.
#'  It reuses the arguments captured during the original screening and only re-submits
#'  the rows stored in `object$error_data` to the appropriate backend.
#'
#' @param object An object of either class `'gpt'` or `'groq'`, as returned by
#'  [tabscreen_gpt()] or [tabscreen_groq()]. Objects of class `'ollama'` are not supported.
#' @param api_key Optional API key. If omitted, the selected backend uses its own default (e.g., `get_api_key()` for GPT and `get_api_key_groq()` for Groq).
#' @param max_tries,max_seconds 'Cap the maximum number of attempts with
#'  `max_tries` or the total elapsed time from the first request with
#'  `max_seconds`. If neither option is supplied (the default), [httr2::req_perform()]
#'  will not retry' (Wickham, 2023). If missing, the values from the original screening
#'  (stored in `attr(object, "arg_list")`) will be reused.
#' @param is_transient 'A predicate function that takes a single argument
#'  (the response) and returns `TRUE` or `FALSE` specifying whether or not
#'  the response represents a transient error' (Wickham, 2023). If missing, the `is_transient`
#'  function from the original screening will be used.
#' @param backoff 'A function that takes a single argument (the number of failed
#'   attempts so far) and returns the number of seconds to wait' (Wickham, 2023).
#'   If missing, the `backoff` value from the original screening will be used.
#' @param after 'A function that takes a single argument (the response) and
#'   returns either a number of seconds to wait or `NULL`, which indicates
#'   that a precise wait time is not available and that the `backoff` strategy
#'   should be used instead' (Wickham, 2023). If missing, the `after` value
#'   from the original screening will be used.
#' @param studyid Optional column (unquoted) for study id. Defaults to 'studyid'.
#' @param title Optional column (unquoted) for title. If omitted, inferred (e.g., title, ti, t1).
#' @param abstract Optional column (unquoted) for abstract. If omitted, inferred (e.g., abstract, ab, abs).
#' @param ... Further arguments forwarded to the underlying backend function
#'  ([tabscreen_gpt()] or [tabscreen_groq()]). If arguments were supplied in the
#'  original screening and should differ for re-screening, pass them again here.
#'
#' @details
#' The backend is derived from `class(object)` and mapped to either
#'  [tabscreen_gpt()] or [tabscreen_groq()]. Only rows in `object$error_data`
#'  are re-submitted. To avoid name collisions during unnesting in the backend,
#'  columns that will be regenerated (currently `decision_binary`, `decision_description`,
#'  `error_message`, `res`) are dropped from `error_data` before the call.
#'  The original arguments from the first screening are taken from `attr(object, "arg_list")`
#'  and are combined with any non-`NULL` overrides provided here.
#'
#' @return An object of class `'gpt'` or `'groq'` similar to the object returned by
#'  the original screening function, with:
#'  - `answer_data` updated to include newly successful rows,
#'  - `error_data` updated to include only remaining failures.
#'  Other fields (e.g., `price_data`, `price_dollar`, and `arg_list`) are preserved or updated by the backend.
#'
#' @seealso [tabscreen_gpt()], [tabscreen_groq()]
#'
#' @examples
#' \dontrun{
#'
#' # Example with openai
#' set_api_key()
#' prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"
#'
#' obj_with_error <-
#'   tabscreen_gpt(
#'     data = filges2015_dat[1:2,],
#'     prompt = prompt,
#'     studyid = studyid,
#'     title = title,
#'     abstract = abstract,
#'     model = "gpt-4o-mini"
#'     )
#'
#' obj_rescreened <-
#'   obj_with_error |>
#'   screen_errors()
#'
#'
#' # Example with groq
#' prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"
#'
#' obj_with_error <-
#'   tabscreen_groq(
#'     data = filges2015_dat[1:2,],
#'     prompt = prompt,
#'     studyid = studyid,
#'     title = title,
#'     abstract = abstract,
#'     model = "llama-3.3-70b-versatile"
#'   )
#'
#' obj_rescreened <-
#'   obj_with_error |>
#'   screen_errors()
#'}
#' @export

screen_errors <- function(
  object,
  api_key = NULL,
  max_tries = NULL,
  max_seconds = NULL,
  is_transient = NULL,
  backoff = NULL,
  after = NULL,
  studyid = NULL,
  title = NULL,
  abstract = NULL,
  ...
) {
  # Detect backend class
  if (inherits(object, "ollama")) {
    stop("Rescreening not supported for 'ollama' class. Errors obtained from tabscreen_ollama() will most likely be due to local resource constraints or invalid input.")
  }
  backend_class <- intersect(class(object), c("gpt", "groq"))
  if (length(backend_class) == 0) stop("Unknown object class for screen_errors().")

  backend_fun <- switch(
    backend_class,
    gpt = tabscreen_gpt,
    groq = tabscreen_groq,
    stop("No tabscreen function for this class.")
  )

  error_data <- object$error_data
  if (is.null(error_data) || nrow(error_data) == 0) stop("No errors to rescreen.")

  # Drop columns regenerated by tabscreen_* to avoid collisions
  drop_cols <- c("decision_binary", "decision_description", "error_message", "res")
  error_data <- dplyr::select(error_data, -dplyr::any_of(drop_cols))

  # Recover original args if present (but don't require them)
  arg_list <- attr(object, "arg_list")
  if (is.null(arg_list)) arg_list <- list()

  # Helper for case-insensitive alias lookup
  names_lower <- tolower(names(error_data))
  find_first <- function(aliases) {
    aliases <- tolower(aliases)
    idx <- match(aliases, names_lower)
    idx <- idx[!is.na(idx)]
    if (length(idx)) names(error_data)[idx[1]] else NULL
  }

  # Resolve column mappings
  studyid_col <- NULL
  title_col <- NULL
  abstract_col <- NULL

  if (!missing(studyid))  studyid_col  <- rlang::as_name(rlang::ensym(studyid))
  if (!missing(title))    title_col    <- rlang::as_name(rlang::ensym(title))
  if (!missing(abstract)) abstract_col <- rlang::as_name(rlang::ensym(abstract))

  if (is.null(studyid_col))  studyid_col  <- arg_list$studyid_col
  if (is.null(title_col))    title_col    <- arg_list$title_col
  if (is.null(abstract_col)) abstract_col <- arg_list$abstract_col

  # Prefer canonical names, else infer from aliases
  if (is.null(studyid_col))  studyid_col  <- if ("studyid" %in% names(error_data)) "studyid" else find_first(c("id","record_id","study_id"))
  if (is.null(title_col))    title_col    <- if ("title"   %in% names(error_data)) "title"   else find_first(c("ti","t1","t","title_text"))
  if (is.null(abstract_col)) abstract_col <- if ("abstract"%in% names(error_data)) "abstract" else find_first(c("ab","abs","a","abstract_text"))

  if (is.null(studyid_col) || !(studyid_col %in% names(error_data))) {
    stop("screen_errors() needs a 'studyid' column from the initial screening.")
  }
  if (is.null(title_col) || is.null(abstract_col)) {
    stop("screen_errors() could not infer title/abstract columns. Pass them explicitly, e.g.: screen_errors(results, title = TI, abstract = AB).")
  }

  # Determine the original prompt (prefer successful answers)
  prompt_val <- NULL
  if (!is.null(object$answer_data) && "prompt" %in% names(object$answer_data)) {
    u <- unique(stats::na.omit(object$answer_data$prompt))
    if (length(u) == 1) prompt_val <- u
  }
  if (is.null(prompt_val) && "prompt" %in% names(error_data)) {
    u <- unique(stats::na.omit(error_data$prompt))
    if (length(u) == 1) prompt_val <- u
  }
  if (is.null(prompt_val) && !is.null(arg_list$prompt)) prompt_val <- arg_list$prompt
  if (is.null(prompt_val)) stop("screen_errors() could not find the original prompt.")

  # Build minimal input to avoid name collisions
  keep_cols <- unique(stats::na.omit(c(studyid_col, title_col, abstract_col)))
  rescreen_data <-
    error_data |>
    dplyr::select(dplyr::all_of(keep_cols)) |>
    dplyr::distinct()

  # Ensure plain tibble
  rescreen_data <- tibble::as_tibble(as.data.frame(rescreen_data), .name_repair = "minimal")

  # Prepare overrides (only include non-NULL to avoid clobbering originals)
  overrides <- list(
    data      = rescreen_data,
    prompt    = prompt_val,
    # map columns explicitly (NSE)
    studyid   = rlang::sym(studyid_col),
    title     = rlang::sym(title_col),
    abstract  = rlang::sym(abstract_col),
    # retry controls (user overrides)
    max_tries   = max_tries,
    max_seconds = max_seconds,
    is_transient= is_transient,
    backoff     = backoff,
    after       = after,
    ...
  )

  # Forward api key if provided; otherwise backend default is used
  if (!is.null(api_key)) {
    overrides$api_key <- api_key
  }

  # If model wasn't captured previously, try infer a single model from error_data
  if (is.null(arg_list$model) && "model" %in% names(error_data)) {
    mod <- unique(stats::na.omit(error_data$model))
    if (length(mod) == 1) overrides$model <- mod
  }

  overrides <- overrides[!vapply(overrides, is.null, logical(1))]

  # Merge with original args (originals provide defaults; overrides win)
  call_args <- utils::modifyList(arg_list, overrides)
  call_args <- call_args[!vapply(call_args, is.null, logical(1))]

  # Call the identified function
  rescreened <- rlang::call2(backend_fun, !!!call_args) |>
    rlang::eval_tidy()

  # Combine results
  orig_success <- if (!is.null(object$answer_data)) dplyr::filter(object$answer_data, !is.na(decision_binary)) else dplyr::tibble()
  new_success  <- if (!is.null(rescreened$answer_data)) dplyr::filter(rescreened$answer_data, !is.na(decision_binary)) else dplyr::tibble()
  object$answer_data <- dplyr::bind_rows(orig_success, new_success)
  object$error_data  <- rescreened$error_data
  object
}