#' Get Groq API Key
#'
#' @description Get the Groq API key from environment variables
#' @param env_var Environment variable name containing the API key
#' @return API key string
#' @export
#'
#' @examples
#' \dontrun{
#' # Set your API key first
#' Sys.setenv(GROQ_KEY = "your-api-key")
#' 
#' # Get the API key
#' key <- get_api_key()
#' }
get_api_key <- function(env_var = "GROQ_KEY") {
  api_key <- Sys.getenv(env_var)
  if (api_key == "") {
    stop("GROQ_KEY environment variable not set. Please set it using Sys.setenv(GROQ_KEY = 'your-api-key')")
  }
  return(api_key)
}

#' Check if error is transient for Groq API
#'
#' @description Predicate function to determine if an HTTP error should trigger a retry
#' @param resp HTTP response object from httr2
#' @return Logical indicating if error is transient and should be retried
#' @export
#'
#' @examples
#' \dontrun{
#' # Used internally in tabscreen_groq for retry logic
#' groq_is_transient(response)
#' }
groq_is_transient <- function(resp) {
  status_code() %in% c(429, 500:503)
}

#' Test if object is groq_tbl
#'
#' @description Check if an object inherits from the groq_tbl class
#' @param x An object to test
#' @return TRUE if object inherits from groq_tbl class, FALSE otherwise
#' @export
#'
#' @examples
#' \dontrun{
#' result <- tabscreen_groq(data, prompt, studyid, title, abstract)
#' is_groq_tbl(result$answer_data)  # TRUE
#' }
is_groq_tbl <- function(x) {
  inherits(x, "groq_tbl")
}

#' Test if object is groq_agg_tbl  
#'
#' @description Check if an object inherits from the groq_agg_tbl class
#' @param x An object to test
#' @return TRUE if object inherits from groq_agg_tbl class, FALSE otherwise
#' @export
#'
#' @examples
#' \dontrun{
#' result <- tabscreen_groq(data, prompt, studyid, title, abstract, reps = 3)
#' is_groq_agg_tbl(result$answer_data_aggregated)  # TRUE
#' }
is_groq_agg_tbl <- function(x) {
  inherits(x, "groq_agg_tbl")
}

#' Get HTTP status code from last response
#'
#' @description Internal function to get the HTTP status code from the most recent API call
#' @return Integer HTTP status code, or 999 if no response available
#' @keywords internal
status_code <- function() {
  resp <- httr2::last_response()
  if (!is.null(resp)) {
    code <- resp |> httr2::resp_status()
  } else {
    code <- 999
  }
  code
}

#' Get formatted text for HTTP status code errors
#'
#' @description Internal function to provide user-friendly error messages for HTTP status codes
#' @return Character string with formatted error message
#' @keywords internal
status_code_text <- function() {
  resp_last <- httr2::last_response()
  check_string <- "[possibly overload on server]"
  
  if (!is.null(resp_last)) {
    code <- resp_last |> httr2::resp_status()
    text <- paste("Error", code)
    
    if (code == 400) text <- paste("Error", code, "Bad request [check/clean body parameters]")
    if (code == 401) text <- paste("Error", code, "Unauthorized [invalid api]")
    if (code == 404) text <- paste("Error", code, "[check model]")
    if (code == 429 | code == 500) text <- paste("Error", code, check_string)
    if (code == 503) text <- paste("Error", code, "Service Unavailable")
    
    s_code <- text
  } else {
    s_code <- "Error [could not reach host]"
  }
  s_code
}

#' Get input price per token for a Groq model
#'
#' @description Internal function to retrieve input token pricing for Groq models
#' @param model Character string with Groq model name
#' @return Numeric price per input token, or NA if model not found
#' @keywords internal
.input_price_groq <- function(model) {
  result <- groq_model_prices |> 
    dplyr::filter(model == !!model) |> 
    dplyr::pull(price_in_per_token)
  
  if(length(result) == 0) NA_real_ else result
}

#' Get output price per token for a Groq model
#'
#' @description Internal function to retrieve output token pricing for Groq models
#' @param model Character string with Groq model name
#' @return Numeric price per output token, or NA if model not found
#' @keywords internal
.output_price_groq <- function(model) {
  result <- groq_model_prices |> 
    dplyr::filter(model == !!model) |> 
    dplyr::pull(price_out_per_token)
  
  if(length(result) == 0) NA_real_ else result
}

#' Calculate price for Groq API usage
#'
#' @description Internal function to calculate total costs for Groq API screening
#' @param data Dataset with token usage information and model details
#' @return Data frame with pricing breakdown by model
#' @keywords internal
.price_groq <- function(data) {
  price_data <- data |>
    dplyr::mutate(
      prompt_tokens = if("prompt_tokens" %in% names(data)) prompt_tokens 
                      else round(stringr::str_count(question, '\\w+') * 1.6),
      completion_tokens = if("completion_tokens" %in% names(data)) completion_tokens else 7.05
    ) |>
    dplyr::filter(!is.na(prompt_tokens) | !is.na(completion_tokens)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      input_price = prompt_tokens * .input_price_groq(model) * iterations,
      output_price = completion_tokens * .output_price_groq(model) * iterations
    ) |>
    dplyr::ungroup() |>
    dplyr::reframe(
      prompt = if("promptid" %in% names(data)) first(promptid) else NA_integer_,
      iterations = unique(iterations),
      input_price_dollar = sum(input_price, na.rm = TRUE),
      output_price_dollar = sum(output_price, na.rm = TRUE),
      total_price_dollar = round(input_price_dollar + output_price_dollar, 4),
      .by = c(model, iterations)
    )
  
  return(price_data)
}
