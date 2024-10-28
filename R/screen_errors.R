#' @title Generic function to re-screen failed title and abstract requests.
#'
#' @description
#' `r lifecycle::badge("experimental")`<br>
#' <br>
#'
#'  This is a generic function to re-screen of failed title and abstract requests.
#'
#' @param object An object of either class `'gpt'` or `'chatgpt'`.
#' @param api_key Numerical value with your personal API key.
#' @param max_tries,max_seconds 'Cap the maximum number of attempts with
#'  `max_tries` or the total elapsed time from the first request with
#'  `max_seconds`. If neither option is supplied (the default), [httr2::req_perform()]
#'  will not retry' (Wickham, 2023). Default `max_tries` is 16. If missing, the value of `max_seconds`
#'  from the original screening conducted with [tabscreen_gpt()] will be used.
#' @param is_transient 'A predicate function that takes a single argument
#'  (the response) and returns `TRUE` or `FALSE` specifying whether or not
#'  the response represents a transient error' (Wickham, 2023). If missing, the `is_transient`
#'  function from the original screening conducted with [tabscreen_gpt()] will be used.
#' @param backoff 'A function that takes a single argument (the number of failed
#'   attempts so far) and returns the number of seconds to wait' (Wickham, 2023).
#'   If missing, the `backoff`value from the original screening conducted with [tabscreen_gpt()] will be used.
#' @param after 'A function that takes a single argument (the response) and
#'   returns either a number of seconds to wait or `NULL`, which indicates
#'   that a precise wait time is not available that the `backoff` strategy
#'   should be used instead' (Wickham, 2023). If missing, the `after` value
#'   from the original screening conducted with [tabscreen_gpt()] will be used.
#' @param ... Further argument to pass to the request body. See \url{https://platform.openai.com/docs/api-reference/chat/create}.
#'  If used in the original screening in [tabscreen_gpt()], the argument(s)
#'  must be specified again here.
#'
#' @return An object of class `'gpt'` or `'chatgpt'` similar to the object returned by [tabscreen_gpt()].
#' See documentation for [tabscreen_gpt()].
#'
#' @seealso [screen_errors.gpt()], [screen_errors.chatgpt()]
#'
#' @examples
#'
#' \dontrun{
#' prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"
#'
#' obj_with_error <-
#'   tabscreen_gpt(
#'     data = filges2015_dat[1:10,],
#'     prompt = prompt,
#'     studyid = studyid,
#'     title = title,
#'     abstract = abstract,
#'     model = "gpt-4o-mini"
#'     )
#'
#' obj_rescreened <-
#'   obj_with_error |>
#'   screen_error()
#'
#'}
#'
#' @export

screen_errors <- function(
    object,
    api_key = get_api_key(),
    max_tries = 4,
    max_seconds,
    is_transient,
    backoff,
    after,
    ...
) UseMethod("screen_errors")

#' @export

screen_errors.default <- function(
    object,
    api_key = get_api_key(),
    max_tries = 4,
    max_seconds,
    is_transient,
    backoff,
    after,
    ...
) {

  stop(paste0("screen_errors does not know how to handle object of class ", class(data),
              ". It can only be used on objects of class 'screen_errors.chatgpt' and 'screen_errors.gpt'."))

}
