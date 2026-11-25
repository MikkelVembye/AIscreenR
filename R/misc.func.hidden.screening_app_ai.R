# Internal functions for the screening app: setup and AI screening

# OpenAI models with a known price
.screening_models <- function() {
  unique(grep("^gpt-", model_prizes$model, value = TRUE))
}

.set_screening_setup <- function(project, question, model, reasons) {
  if (!is.null(project$ai)) {
    stop("The AI has started. Delete the AI answers to change the setup.", call. = FALSE)
  }
  question <- trimws(question)
  if (!nzchar(question)) stop("Write a screening question.", call. = FALSE)
  stopifnot(model %in% .screening_models())

  project$question <- question
  project$model <- model
  project$reasons <- isTRUE(reasons)
  project
}

.delete_ai_answers <- function(project) {
  project["ai"] <- list(NULL)
  project["analysis"] <- list(NULL)
  project$ai_price <- 0
  project
}

# Add the price of one chunk to the total price of the AI screening
.add_ai_price <- function(project, price) {
  project$ai_price <- sum(project$ai_price, price, na.rm = TRUE)
  project
}

# Price (USD) of the answers, from the tokens used and the package's price list.
# The price_dollar of tabscreen_gpt() is not used, because it rounds the input
# price of every request to 4 decimals, which distorts the total for cheap models.
.token_price <- function(answers) {
  prices <- .model_prices(answers)
  sum(
    prices$prompt_tokens * prices$price_in_per_token +
      prices$completion_tokens * prices$price_out_per_token,
    na.rm = TRUE
  )
}

.format_price <- function(price) {
  paste0("$", if (price >= 0.01) sprintf("%.2f", price) else format(signif(price, 2), scientific = FALSE))
}

# Study IDs without an AI answer
.ai_missing <- function(project) {
  setdiff(seq_len(nrow(project$refs)), project$ai$studyid)
}

.ai_decisions <- function(project) {
  out <- rep(NA_integer_, nrow(project$refs))
  out[project$ai$studyid] <- as.integer(project$ai$decision_binary)
  out
}

# Approximate price of screening the records that have no AI answer yet
.ai_cost <- function(project) {
  ids <- .ai_missing(project)
  if (length(ids) == 0 || !nzchar(project$question)) return(0)

  approximate_price_gpt(
    data = .screening_records(project$refs)[ids, ],
    prompt = project$question,
    studyid = studyid,
    title = title,
    abstract = abstract,
    model = project$model
  )$price_dollar
}

# Screen some records in the background. The API key is passed on, never saved.
.start_ai_chunk <- function(chunk, question, model, reasons, api_key) {
  future::future(
    AIscreenR::tabscreen_gpt(
      data = chunk, prompt = question, studyid = studyid, title = title, abstract = abstract,
      model = model, api_key = api_key, decision_description = reasons,
      messages = FALSE, progress = FALSE
    ),
    globals = list(
      chunk = chunk, question = question, model = model, reasons = reasons, api_key = api_key
    ),
    packages = "AIscreenR",
    seed = TRUE
  )
}

# Split the result of a finished chunk into good answers and failed requests
.ai_chunk_result <- function(job) {
  res <- tryCatch(suppressWarnings(future::value(job)), error = function(e) e)
  if (inherits(res, "error")) {
    return(list(answers = NULL, n_failed = NA_integer_, error = conditionMessage(res)))
  }

  answers <- res$answer_data
  answers$studyid <- as.integer(as.character(answers$studyid)) # tabscreen_gpt() returns a factor
  ok <- !is.na(answers$decision_binary)

  list(
    answers = answers[ok, ],
    n_failed = sum(!ok),
    error = if (any(!ok)) as.character(answers$decision_gpt[!ok][1]),
    price = .token_price(res$answer_data) # Includes failed requests that used tokens
  )
}

.add_ai_answers <- function(project, answers) {
  if (is.null(answers) || nrow(answers) == 0) return(project)
  old <- if (!is.null(project$ai)) project$ai[!project$ai$studyid %in% answers$studyid, ]
  project$ai <- dplyr::bind_rows(old, answers)
  project["analysis"] <- list(NULL) # Saved results no longer match the answers
  project
}
