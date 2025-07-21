#' Print method for groq objects
#'
#' @param x A groq object from [tabscreen_groq()].
#' @param ... Additional arguments passed to `print`.
#'
#' @return Invisibly returns the input object `x`. Provides the user with information on how to access the results.
#' @export
#'
#' @examples
#' \dontrun{
#' # res is an object created by tabscreen_groq()
#' print(res)
#' }
print.groq <- function(x, ...) {

  if ("answer_data_aggregated" %in% names(x) && !is.null(x$answer_data_aggregated)) {
    cat("\nFind the final, aggregated result dataset via `x$answer_data_aggregated`")
    cat("\nFind the dataset with all individual answers via `x$answer_data`")
  } else {
    cat("\nFind the final result dataset via `x$answer_data`")
  }

  if ("price_dollar" %in% names(x)) {
    cat(paste0("\n\nTotal screening cost: $", round(x$price_dollar, 4)))
    cat("\nFind detailed price information via `x$price_data`")
  }
  
  if ("error_data" %in% names(x) && !is.null(x$error_data)) {
    n_errors <- nrow(x$error_data)
    cat(paste0("\n\nNOTE: The screening resulted in ", n_errors, " error(s)."))
    cat("\nFind references with errors via `x$error_data`")
  }
  
  cat("\n")
  
  invisible(x)
}