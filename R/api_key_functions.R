
#' @title Creating a temporary R environment API key variable
#'
#' @description This function automatically sets/creates an interim R environment variable with
#' the API key to call a given AI model (e.g. ChatGPT). Thereby users avoid exposing their API keys.
#' If the API key is set in the console, it will/can be revealed via the .Rhistory.
#' Find more information about this issue at https://httr2.r-lib.org/articles/wrapping-apis.html
#'
#' @details When set_api_key() has successfully been executed, \code{get_api_key()} automatically
#' retrieves the API key from the R environment and the users do not need to specify the API when running
#' functions from the package that call an API.
#'
#' @param key Character string with an (ideally encrypt) API key. If not provided it
#' returns a password box in which the true API key can be entered secretly.
#' @param env_var Character string indicating the name of the temporary R environment variables
#' and the used AI model. Currently, the argument only takes \code{env_var = "CHATGPT_KEY"}.
#'
#' @return A temporary environment variable with the name from \code{env_var}.
#' If \code{key} is missing, it returns a password box in which the true API key can be entered.
#' @export
#'
#' @seealso \code{\link{get_api_key}}
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


#' @title Get API key from temporary R environment variable
#'
#' @param env_var Character string indicating the name of the R environment variable
#' with the API key. See \code{set_api_key()} to set/create this variable.
#'
#' @return The specified API key (NOTE: Avoid exposing this in the console).
#' @export
#'
#' @seealso \code{\link{set_api_key}}
#'
#' @examples
#' \dontrun{
#' get_api_key()
#' }
#'


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

