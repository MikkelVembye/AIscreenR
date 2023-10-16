#' Print methods for `'chatgpt'` objects
#'
#' @param x an object of class `'chatgpt'`.
#' @param ... other print arguments.
#'
#' @return Information about how to find answer data sets and pricing information.
#' @export
#'
#' @examples
#' \dontrun{
#' print(x)
#' }


print.chatgpt <- function(x, ...){

  obj_name <- substitute(x)

  all_data <- paste0(obj_name, "$answer_data_all\n\n")
  sum_data <- paste0(obj_name, "$answer_data_sum\n\n")
  sum_data2 <- paste0(obj_name, "$answer_data_sum\n")
  price1 <- paste0(obj_name, "$price_dollor\n\n")
  price2 <- paste0(obj_name, "$price_dollor\n")

  if ("error_data" %in% names(x)) err_data <- paste0(obj_name, "$error_data\n")


  if ("error_data" %in% names(x) && "prompt_tokens" %in% names(x$answer_data_all)){

    cat("Find data with all answers by executing\n ", all_data,
        "Find data with the result aggregated across multiple answers by executing\n ", sum_data,
        "Find total price for the screening by executing\n ", price1,
        "Find error data by executing\n ", err_data, sep = "", ...)

  } else if (!"error_data" %in% names(x) && "prompt_tokens" %in% names(x$answer_data_all)){

    cat("Find data with all answers by executing\n ", all_data,
        "Find data with the result aggregated across multiple answers by executing\n ", sum_data,
        "Find total price for the screening by executing\n ", price2, sep = "", ...)

  } else if ("error_data" %in% names(x) && !"prompt_tokens" %in% names(x$answer_data_all)){

    cat("Find data with all answers by executing\n ", all_data,
        "Find data with the result aggregated across multiple answers by executing\n ", sum_data,
        "Find error data by executing\n ", err_data, sep = "", ...)

  } else if (!"error_data" %in% names(x) && !"prompt_tokens" %in% names(x$answer_data_all)){

    cat("Find data with all answers by executing\n ", all_data,
        "Find data with the result aggregated across multiple answers by executing\n ", sum_data2,
        sep = "", ...)

  }


}

