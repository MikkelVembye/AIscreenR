#' @title Generic function to re-screen failed title and abstract requests.
#'
#' @description
#' `r lifecycle::badge("experimental")`<br>
#' <br>
#'
#'  This is a generic function to re-screen of failed title and abstract requests.
#'
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
