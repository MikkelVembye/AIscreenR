#----------------------------------------------------------------
#
#  Inherits
#
#----------------------------------------------------------------

#Deprecated


#' @title Test if the object is a `'chatgpt'` object
#'
#' @description
#' `r lifecycle::badge("deprecated")`<br>
#' <br>
#' This function returns `TRUE` for `chatgpt` objects,
#' and `FALSE` for all other objects.
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the `chatgpt` class.
#' @export

is_chatgpt <- function(x){
  inherits(x, "chatgpt")
}

#' @title Test if the object is a `'chatgpt_tbl'` object
#'
#' @description
#' `r lifecycle::badge("deprecated")`<br>
#' <br>
#' This function returns `TRUE` for `chatgpt_tbl` objects,
#' and `FALSE` for all other objects.
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the `chatgpt_tbl` class.
#' @export

is_chatgpt_tbl <- function(x){
  inherits(x, "chatgpt_tbl")
}

# New

#' Test if the object is a `'gpt'` object
#'
#' This function returns `TRUE` for `gpt` objects,
#' and `FALSE` for all other objects.
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the `gpt` class.
#' @export

is_gpt <- function(x){
  inherits(x, "gpt")
}

#' Test if the object is a `'gpt_tbl'` object
#'
#' This function returns `TRUE` for `gpt_tbl` objects,
#' and `FALSE` for all other objects.
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the `gpt_tbl` class.
#' @export

is_gpt_tbl <- function(x){
  inherits(x, "gpt_tbl")
}
