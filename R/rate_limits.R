
#' @title Find updated rate limits for API models
#'
#' @description
#' `r lifecycle::badge("stable")`<br>
#' <br>
#' `rate_limits_per_minute` reports the rate limits for a given api.
#' includes the allowed requests per minute (RPM) as well as tokens per minute (TPM).
#' Find general information at
#' \url{https://platform.openai.com/docs/guides/rate-limits/overview}.
#'
#' @param AI_tool Character string specified the AI tool from which the api is
#' issued. Default is `"CHATGPT"`
#' @param model Character string with the name of the completion model.
#' Default = `"gpt-3.5-turbo-0613"`.
#' Find available model at
#' \url{https://platform.openai.com/docs/models/model-endpoint-compatibility}.
#' @param api_key Numerical value with your personal API key. Find at
#'  \url{https://platform.openai.com/account/api-keys}. Use
#'  [secret_make_key()], [secret_encrypt()], and
#'  [secret_decrypt()] to scramble and decrypt the api key and
#'  use [set_api_key()] to securely automate the use of the
#'  api key by setting the api key as a locale environment variable.
#'
#' @return A tibble including variables with information about the model used,
#' the number of requests and tokens per minute.
#' @export
#'
#' @examples
#' \dontrun{
#' set_api_key()
#'
#' rate_limits_per_minute()
#' }

rate_limits_per_minute <- function(
    AI_tool = "CHATGPT",
    model = "gpt-3.5-turbo-0613",
    api_key = get_api_key()
    ){


  if ("CHATGPT" %in% AI_tool){

    body <- list(
      model = model,
      messages = list(list(
        role = "user",
        content = ""
      ))
    )

    req <-
      httr2::request("https://api.openai.com/v1/chat/completions") |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", api_key)
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_retry(
        is_transient = gpt_is_transient,
      ) |>
      httr2::req_user_agent("AIscreenR (http://mikkelvembye.github.io/AIscreenR/)")

    rmp <- req |>
      httr2::req_perform() |>
      httr2::resp_header("x-ratelimit-limit-requests") |>
      as.numeric()

    tmp <- req |>
      httr2::req_perform() |>
      httr2::resp_header("x-ratelimit-limit-tokens") |>
      as.numeric()

    res <- tibble::tibble(
      model = model,
      requests_per_minute = rmp,
      tokens_per_minute = tmp
      )

  }

  res

}
