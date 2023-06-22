#' @param ... Further argument to pass to the request body.
#'  See \url{https://platform.openai.com/docs/api-reference/chat/create}.
#' @param time_info Logical indicating whether the run time of each
#'  request/question should be included in the data. Default = `TRUE`.
#' @param token_info Logical indicating whether the total number of tokens
#' per request should be included in the output data. Default = `TRUE`.
#' @param model Character string with the name of the completion model.
#'  Default = `"gpt-3.5-turbo-0301"`. Find available model at
#' \url{https://platform.openai.com/docs/models/model-endpoint-compatibility}.
#' @param role Character string indicate the role of the user. Default is `"user"`.
#' @param api_key Numerical value with your personal API key. Find at
#'  \url{https://platform.openai.com/account/api-keys}. Use
#'  [secret_make_key()], [secret_encrypt()], and
#'  [secret_decrypt()] to scramble and decrypt the api key and
#'  use [set_api_key()] to securely automate the use of the
#'  api key by setting the api key as a locale environment variable.
#' @param max_tries,max_seconds 'Cap the maximum number of attempts with
#'  `max_tries` or the total elapsed time from the first request with
#'  `max_seconds`. If neither option is supplied (the default), [req_perform()]
#'  will not retry' (Wickham, 2023).
#' @param is_transient 'A predicate function that takes a single argument
#'  (the response) and returns `TRUE` or `FALSE` specifying whether or not
#'  the response represents a transient error' (Wickham, 2023).
#' @param backoff 'A function that takes a single argument (the number of failed
#'   attempts so far) and returns the number of seconds to wait' (Wickham, 2023).
#' @param after 'A function that takes a single argument (the response) and
#'   returns either a number of seconds to wait or `NULL`, which indicates
#'   that a precise wait time is not available that the `backoff` strategy
#'   should be used instead' (Wickham, 2023).
#' @param rpm Numerical value indicating the number of requests per minute (rpm)
#'  available for the specified api key. Find more information at
#'  \url{https://platform.openai.com/docs/guides/rate-limits/what-are-the-rate-limits-for-our-api}.
#'  Alternatively, use [rate_limits_per_minute()].
#' @param reps Numerical value indicating the number of times the same
#'  question should be sent to ChatGPT. This can be useful to test consistency
#'  between answers. Default is `1`.
#' @param seed Numerical value for a seed to ensure that proper,
#'  parallel-safe random numbers are produced.
