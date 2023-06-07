
#' Creating a temporary R environment API key variable
#'
#' @param key Character string with an (ideally encrypt) API key. If not provided it
#' returns a password box in which the true API key can be entered.
#' @param env_var Character string indicating the name of the temporary R environment variables
#' and the used AI model. Currently, the argument only takes \code{env_var = "CHATGPT_KEY"}.
#'
#' @return A temporary environment variable with the name from \code{env_var}.
#' If \code{key} is missing, it returns a password box in which the true API key can be entered.
#' @export
#'
#' @examples
#' \dontrun{
#' set_api_key()
#' }
#'

# Inspired by codes from https://httr2.r-lib.org/articles/wrapping-apis.html

set_api_key <- function(key, env_var = "CHATGPT_KEY") {
  if (missing(key)) {
    key <- askpass::askpass("Please enter your API key")
  }

  if ("CHATGPT_KEY" %in% env_var) Sys.setenv("CHATGPT_KEY" = key)
}


get_api_key <- function(env_var = "CHATGPT_KEY") {

  if ("CHATGPT_KEY" %in% env_var) key <- Sys.getenv("CHATGPT_KEY")

  if (identical(key, "")){

    if (is_testing()) {

      if ("CHATGPT_KEY" %in% env_var) key <- testing_key_chatgpt()

    } else {

      stop("No API key found. Use set_api_key()")

    }

  }

  key

}


# Helpers

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

