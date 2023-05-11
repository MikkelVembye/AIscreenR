
#' Abstract screening with ChatGPT function
#'
#' @param question Insert
#' @param api_key Insert
#' @param model Insert
#' @param sleep_time Insert
#' @param time_info Logical indicating if time for answer should be returned
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
#'
#'
#'


ask_chatgpt <- function(
    question,
    api_key,
    model = "gpt-3.5-turbo",
    sleep_time = 20,
    time_info = FALSE
 ){

  run_ask_chatgpt <- function(
    question,
    api_key,
    model,
    sleep_time,
    time_info
    ){

    tictoc::tic()

    # Insert homepage

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
    run_time <- round(as.numeric(time$toc - time$tic), 2)

    if (sleep_time > run_time) Sys.sleep(sleep_time - round(run_time + 1))


    answer <- stringr::str_trim(httr::content(response)$choices[[1]]$message$content) |>
      stringr::str_remove_all("\n")

    if(rlang::is_empty(answer)){
      answer <- "API limit reached or invalid API"
      run_time <- NA_real_
    }
    res <- tibble::tibble(answer = answer)

    if(time_info){
      res <- res |> dplyr::mutate(run_time = run_time)
    }

    res

  }

  if (time_info){

    run_ask_chatgpt <-
      purrr::possibly(
        run_ask_chatgpt,
        otherwise = tibble::tibble(
          answer = paste0(
          "ERROR (Possible because the token limit is reached. ",
          "Try to reduce the number of characters [including the answer] below 3500)"
          ),
          run_time = NA_real_
        )
      )

  } else {

  run_ask_chatgpt <-
    purrr::possibly(
      run_ask_chatgpt,
      otherwise = tibble::tibble(
        answer = paste0(
        "ERROR (Possible because the token limit is reached. ",
        "Try to reduce the number of characters [including the answer] below 3500)"
     )
    )
   )

  }

  run_ask_chatgpt(
    question = question,
    api_key = api_key,
    model = model,
    sleep_time = sleep_time,
    time_info = time_info
  )


}



















