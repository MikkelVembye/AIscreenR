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

  dat1 <- substitute(x$answer_data_all)
  dat2 <- substitute(x$answer_data_sum)

  cat("Find data with all answers by executing\n\n", deparse(dat1),
      "\n\nFind data with the result aggregated across multiple answers by executing\n\n",
      deparse(dat2), ...)

}
