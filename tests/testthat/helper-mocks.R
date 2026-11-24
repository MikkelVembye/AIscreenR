# Helpers for mocking HTTP calls to the LLM APIs.
#
# `local_mock_api()` replaces httr2's transport with `handler(req)`, makes
# curl::has_internet() return `internet`, and keeps httr2::last_response()
# in sync (httr2 does not set it for mocked responses), which the package
# relies on via status_code() and error_message().

local_mock_api <- function(handler, internet = TRUE, .env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$last <- NULL
  state$n <- 0L
  state$reqs <- list()

  wrapped <- function(req) {
    state$n <- state$n + 1L
    state$reqs[[state$n]] <- req
    resp <- handler(req)
    state$last <- resp
    resp
  }

  httr2::local_mocked_responses(wrapped, env = .env)
  testthat::local_mocked_bindings(
    last_response = function() state$last,
    .package = "httr2",
    .env = .env
  )
  testthat::local_mocked_bindings(
    has_internet = function() internet,
    .package = "curl",
    .env = .env
  )
  invisible(state)
}

json_resp <- function(body, status = 200L) {
  httr2::response_json(status_code = status, body = body)
}

text_resp <- function(text, status = 500L, type = "text/html") {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = type),
    body = charToRaw(text)
  )
}

# OpenAI responses endpoint
mock_responses_body <- function(args = list(decision_gpt = "1"), usage = TRUE) {
  list(
    model = "gpt-mock",
    output = list(
      list(type = "reasoning"),
      list(type = "function_call", arguments = jsonlite::toJSON(args, auto_unbox = TRUE))
    ),
    usage = if (usage) list(input_tokens = 11, output_tokens = 7)
  )
}

# OpenAI chat completions style
mock_chat_body <- function(args = list(decision_gpt = "1")) {
  list(
    model = "chat-mock",
    choices = list(list(message = list(tool_calls = list(
      list(`function` = list(arguments = jsonlite::toJSON(args, auto_unbox = TRUE)))
    )))),
    usage = list(prompt_tokens = 11, completion_tokens = 7)
  )
}

mock_dat <- function(n = 2) {
  tibble::tibble(
    studyid = seq_len(n),
    title = paste("Title", seq_len(n)),
    abstract = paste("Abstract", seq_len(n))
  )
}
