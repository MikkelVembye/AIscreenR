#' Print methods for `'gpt_price'` objects
#'
#' @param x an object of class `"gpt_price"`.
#' @param ... other print arguments.
#'
#' @return The total price of the screening across all gpt-models expected to be used for the screening.
#' @export
#'
#' @examples
#' \dontrun{
#' print(x)
#' }

print.gpt_price <- function(x, ...){
  price <- paste0("$", x$price_dollar, ".")
  cat("The approximate price of the (simple) screening will be around", price, ...)
}
