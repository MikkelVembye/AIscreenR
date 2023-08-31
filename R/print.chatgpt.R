#' Print methods for 'chatgpt' objects
#'
#' @param x an object of class "chatgpt".
#' @param ... other print arguments.
#'
#' @return An object with the head of the summarized and final answer data
#' ideally deduced from multiple answers from ChatGPT.
#' @export
#'
#' @examples
#' \dontrun{
#' print(x)
#' }


print.chatgpt <- function(x, ...){

  print(x$answer_data_sum, ...)

}
