#' Groq model pricing data
#'
#' @description Data frame containing current pricing information for Groq models
#' @format A data frame with pricing information:
#' \describe{
#'   \item{model}{Character vector of model names}
#'   \item{price_in_per_token}{Numeric vector of input token prices (USD per token)}
#'   \item{price_out_per_token}{Numeric vector of output token prices (USD per token)}
#' }
#' @source \url{https://console.groq.com/docs/pricing}
#' @export
groq_model_prices <- data.frame(
  model = c(
    "meta-llama/llama-4-scout-17b-16e-instruct",
    "meta-llama/llama-4-maverick-17b-128e-instruct", 
    "deepseek-r1-distill-llama-70b",
    "qwen-qwq-32b",
    "mistral-saba-24b",
    "llama-3.3-70b-versatile",
    "llama-3.1-8b-instant",
    "llama3-70b-8192",
    "llama3-8b-8192", 
    "gemma2-9b-it",
    "llama-guard-3-8b"
  ),
  price_in_per_token = c(
    0.11/1000000, 0.20/1000000, 0.75/1000000,
    0.29/1000000, 0.79/1000000, 0.59/1000000,
    0.05/1000000, 0.59/1000000, 0.05/1000000,
    0.20/1000000, 0.20/1000000
  ),
  price_out_per_token = c(
    0.34/1000000, 0.60/1000000, 0.99/1000000,
    0.39/1000000, 0.79/1000000, 0.79/1000000,
    0.08/1000000, 0.79/1000000, 0.08/1000000,
    0.20/1000000, 0.20/1000000
  )
)

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
#' @return Data frame with pricing breakdown by model and iterations
#' @keywords internal
.price_groq <- function(data) {
  price_data <- data |>
    dplyr::mutate(
      # Estimate tokens if not available
      prompt_tokens = if("prompt_tokens" %in% names(data)) prompt_tokens 
                      else round(stringr::str_count(question, '\\w+') * 1.6),
      completion_tokens = if("completion_tokens" %in% names(data)) completion_tokens else 7.05
    ) |>
    dplyr::filter(!is.na(prompt_tokens) | !is.na(completion_tokens)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # Calculate costs per row
      input_price = prompt_tokens * .input_price_groq(model) * iterations,
      output_price = completion_tokens * .output_price_groq(model) * iterations
    ) |>
    dplyr::ungroup() |>
    dplyr::reframe(
      # Aggregate by model and iterations
      prompt = if("promptid" %in% names(data)) dplyr::first(promptid) else NA_integer_,
      iterations = unique(iterations),
      input_price_dollar = sum(input_price, na.rm = TRUE),
      output_price_dollar = sum(output_price, na.rm = TRUE),
      total_price_dollar = round(input_price_dollar + output_price_dollar, 4),
      .by = c(model, iterations)
    )
  
  return(price_data)
}

#' Get current Groq pricing for a specific model
#'
#' @description Helper function to get pricing information for a specific Groq model
#' @param model Character string with Groq model name
#' @return Named list with input and output prices per token
#' @export
#' @examples
#' \dontrun{
#' get_groq_pricing("llama3-70b-8192")
#' }
get_groq_pricing <- function(model) {
  if (!model %in% groq_model_prices$model) {
    stop("Model '", model, "' not found in pricing data.")
  }
  
  pricing <- groq_model_prices |>
    dplyr::filter(model == !!model) |>
    dplyr::select(price_in_per_token, price_out_per_token)
  
  list(
    model = model,
    input_price_per_token = pricing$price_in_per_token,
    output_price_per_token = pricing$price_out_per_token,
    currency = "USD"
  )
}

#' Calculate estimated cost for Groq screening
#'
#' @description Estimate the cost of screening based on number of studies and tokens
#' @param n_studies Number of studies to screen
#' @param avg_tokens_per_study Average number of tokens per study (input)
#' @param avg_completion_tokens Average completion tokens per response
#' @param model Groq model name
#' @param reps Number of repetitions per study
#' @return Estimated cost in USD
#' @export
#' @examples
#' \dontrun{
#' estimate_groq_cost(
#'   n_studies = 100,
#'   avg_tokens_per_study = 300,
#'   model = "llama3-70b-8192"
#' )
#' }
estimate_groq_cost <- function(
  n_studies,
  avg_tokens_per_study = 300,
  avg_completion_tokens = 10,
  model = "llama3-70b-8192",
  reps = 1
) {
  
  if (!model %in% groq_model_prices$model) {
    stop("Model '", model, "' not found in pricing data.")
  }
  
  pricing <- get_groq_pricing(model)
  
  total_input_tokens <- n_studies * avg_tokens_per_study * reps
  total_output_tokens <- n_studies * avg_completion_tokens * reps
  
  input_cost <- total_input_tokens * pricing$input_price_per_token
  output_cost <- total_output_tokens * pricing$output_price_per_token
  total_cost <- input_cost + output_cost
  
  list(
    model = model,
    n_studies = n_studies,
    reps = reps,
    total_input_tokens = total_input_tokens,
    total_output_tokens = total_output_tokens,
    input_cost_usd = round(input_cost, 4),
    output_cost_usd = round(output_cost, 4),
    total_cost_usd = round(total_cost, 4)
  )
}