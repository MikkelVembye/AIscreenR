#' Print methods for 'chatgpt' objects
#'
#' @param x an object of class "chatgpt".
#' @param ... other print arguments.
#'
#' @return Information about how to find answer datasets and pricing information.
#' @export
#'
#' @examples
#' \dontrun{
#' print(x)
#' }


print.chatgpt <- function(x, ...){

  cat("Find data with all answers by executing\n object_name$answer_data_all",
      "\n\nFind data with the result aggregated across multiple answers by executing\n",
      "object_name$answer_data_sum\n\nFind total price for the screening by executing\n",
      "object_name$price_dollor", ...)

}
