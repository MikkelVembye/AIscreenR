#' Print methods for `'gpt'` objects
#'
#' @param x an object of class `'gpt'`.
#' @param ... other print arguments.
#'
#' @return Information about how to find answer data sets and pricing information.
#' @export
#'
#' @examples
#' \dontrun{
#' print(x)
#' }


print.gpt <- function(x, ...){

  if ("answer_data_aggregated" %in% names(x)){
    print <- cat(paste0("\nFind the final result dataset via result_object$answer_data_aggregated"))
  } else {
    print <- cat(paste0("\nFind the final result dataset via result_object$answer_data"))
  }

  print

}
