
#' @title Find updated rate limits for API models
#'
#' @description
#' `r lifecycle::badge("stable")`<br>
#' <br>
#' `rate_limits_per_minute` reports the rate limits for a given API model.
#' The function returns the available requests per minute (RPM) as well as tokens per minute (TPM).
#' Find general information at
#' \url{https://developers.openai.com/api/docs/models/model-endpoint-compatibility}.
#'
#' @param AI_tool Character string specifying the AI tool from which the API is
#' issued. Currently supports `"OpenAI"` (default) and `"Groq"`.
#' @param model Character string with the name of the completion model.
#' Default is `"gpt-4o-mini"`. Can take multiple values.
#' For OpenAI models, find available models at
#' \url{https://developers.openai.com/api/docs/models/model-endpoint-compatibility}.
#' For Groq models, find available models at
#' \url{https://console.groq.com/docs/models}.
#' @param api_key Character string with the API key. For OpenAI, use [get_api_key()].
#' For Groq, use [get_api_key_groq()].
#'
#' @return A \code{tibble} including variables with information about the model used,
#' the number of requests and tokens per minute.
#' @export
#'
#' @examples
#' \dontrun{
#' set_api_key()
#'
#' rate_limits_per_minute(
#'   model = "gpt-4o-mini",
#'   AI_tool = "OpenAI",
#'   api_key = get_api_key()
#' )
#'
#' # Groq example
#' rate_limits_per_minute(
#'   model = "llama3-70b-8192",
#'   AI_tool = "Groq",
#'   api_key = get_api_key_groq()
#' )
#' }

rate_limits_per_minute <- function(
    model = "gpt-4o-mini",
    AI_tool = "OpenAI",
    api_key = NULL
) {
  if (is.null(api_key)) {
    if (AI_tool == "OpenAI") {
      api_key <- get_api_key()
    } else if (AI_tool == "Groq") {
      api_key <- get_api_key_groq()
    } else {
      stop("AI_tool must be 'OpenAI' or 'Groq'.")
    }
  }
  furrr::future_map_dfr(model, ~ .rate_limits_per_minute_engine(model = .x, AI_tool = AI_tool, api_key = api_key))

}

# Hidden function
.rate_limits_per_minute_engine <- function(
    model, AI_tool, api_key
    ){

  if ("Groq" %in% AI_tool && any(!is.element(model, groq_model_prizes$model)))
    stop("Unknown model(s) used. Available models are: ",
         paste(groq_model_prizes$model, collapse = ", "))
  
  if ("OpenAI" %in% AI_tool && any(!is.element(model, model_prizes$model)))
    stop("Unknown model(s) used. Available models are: ",
         paste(model_prizes$model, collapse = ", "))

  # Set endpoint URL and transient handler based on AI_tool
  if ("OpenAI" %in% AI_tool) {

    api_url <- "https://api.openai.com/v1/chat/completions"
    is_trans <- gpt_is_transient

  } else if ("Groq" %in% AI_tool) {

    api_url <- "https://api.groq.com/openai/v1/chat/completions"
    is_trans <- .groq_is_transient

  } else {
    stop("AI_tool must be 'OpenAI' or 'Groq'.")
  }

  body <- list(
    model = model,
    messages = list(list(
      role = "user",
      content = "1+1"
    ))
  )

  req <-
    httr2::request(api_url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_key)
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_retry(
      is_transient = is_trans,
      max_tries = 2 # Setting retries to 2 to avoid message from httr2
    ) |>
    httr2::req_user_agent("AIscreenR (http://mikkelvembye.github.io/AIscreenR/)")


  if (curl::has_internet()){

    resp <- try(
      suppressMessages(req |> httr2::req_perform()),
      silent = TRUE
    )

    if (status_code() == 200){

      rmp <- resp |>
        httr2::resp_header("x-ratelimit-limit-requests") |>
        as.numeric()

      tmp <- resp |>
        httr2::resp_header("x-ratelimit-limit-tokens") |>
        as.numeric()

      res <- tibble::tibble(
        model = model,
        requests_per_minute = rmp,
        tokens_per_minute = tmp
      )

    } else {

      res <- tibble::tibble(
        model = error_message(),
        requests_per_minute = NA_real_,
        tokens_per_minute = NA_real_
      )

    }

  } else {

    res <- tibble::tibble(
      model = "Error: Could not reach host [check internet connection]",
      requests_per_minute = NA_real_,
      tokens_per_minute = NA_real_
    )

  }

  res

}
