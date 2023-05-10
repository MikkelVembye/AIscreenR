
#' Abstract screening with ChatGPT function
#'
#' @param question Insert
#' @param api_key Insert
#' @param model Insert
#' @param sleep_time Insert
#' @param add_name_to_vars Insert
#'
#' @return A tibble with answer and its running time.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' q <- "What is a carrot?"
#' ask_chatgpt(q, api_key = 123456789, sleep_time = 0)
#' }
#'
ask_chatgpt <- function(
    question,
    api_key,
    model = "gpt-3.5-turbo",
    sleep_time = 20,
    add_name_to_vars = NULL
){

  Sys.sleep(sleep_time)

  tictoc::tic()

  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = model,
      messages = list(list(
        role = "user",
        content = question
      ))
    )
  )

  time <- tictoc::toc(quiet = TRUE)


  res <- tibble::tibble(

    time = as.numeric(time$toc - time$tic),
    answer_raw = stringr::str_trim(httr::content(response)$choices[[1]]$message$content),
    answer = stringr::str_remove_all(answer_raw, "\n")

  ) |>
    dplyr::select(-answer_raw)

  if (!is.null(add_name_to_vars)){

    if (!is.character(add_name_to_vars)){
      stop("add_name_to_vars only takes a character string as input")
    }

    res <- res |> dplyr::rename_with(~ paste0(.x, add_name_to_vars))

  }

  res

}
