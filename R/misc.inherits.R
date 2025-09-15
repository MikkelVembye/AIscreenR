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

#' Test if the object is a `'gpt_agg_tbl'` object
#'
#' This function returns `TRUE` for `gpt_agg_tbl` objects,
#' and `FALSE` for all other objects.
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the `gpt_agg_tbl` class.
#' @export

is_gpt_agg_tbl <- function(x){
  inherits(x, "gpt_agg_tbl")
}

#-----------------------------------------------------------------------
# GROQ functions
#-----------------------------------------------------------------------
#' Test if the object is a `'groq'` object
#' 
#' This function returns `TRUE` for `groq` objects,
#' and `FALSE` for all other objects.
#' 
#' @param x An object
#' @return `TRUE` if the object inherits from the `groq` class.
#' @export

is_groq_tbl <- function(x) {
  inherits(x, "groq_tbl")
}

#' Test if the object is a `'groq_agg_tbl'` object
#' This function returns `TRUE` for `groq_agg_tbl` objects,
#' and `FALSE` for all other objects.
#' @param x An object
#' @return `TRUE` if the object inherits from the `groq_agg_tbl
#' @export

is_groq_agg_tbl <- function(x) {
  inherits(x, "groq_agg_tbl")
}

#----------------------------------------------------------------
# Ollama functions
#----------------------------------------------------------------
#' Test if the object is a `'ollama'` object
#' 
#' This function returns `TRUE` for `ollama` objects,
#' and `FALSE` for all other objects.
#' 
#' @param x An object
#' @return `TRUE` if the object inherits from the `ollama` class.
#' @export
is_ollama_tbl <- function(x) {
  inherits(x, "ollama_tbl")
}

#' Test if the object is a `'ollama_agg_tbl'` object
#' This function returns `TRUE` for `ollama_agg_tbl` objects,
#' and `FALSE` for all other objects.
#' @param x An object
#' @return `TRUE` if the object inherits from the `ollama_agg_tbl
#' @export
is_ollama_agg_tbl <- function(x) {
  inherits(x, "ollama_agg_tbl")
}