
#' Asking a single question to ChatGPT
#'
#' @param question Character with the question you want ChatGPT to answer.
#' @param api_key Numerical value with your personal API key. Find at \url{https://platform.openai.com/account/api-keys}
#' @param model Character indicating the ChatGPT model to be use. Default is "gpt-3.5-turbo".
#' @param sleep_time Numerical value indicating in seconds the sleeping time in between questions. This
#' is especially helpful when using the function together with \code{purrr::map_*} functions. With a free subscription at ChatGPT
#' you can ask 3 questions per minute. With a plus subscription you have access to 60 questions per minute, and can reduce the
#' default sleeping time. Default is 20 seconds.
#' @param time_info Logical indicating if the time of the answer should be returned.
#' @param reps Numerical indicating the number of times the same questions should be sent to ChatGPT.
#' This can be useful when investigating the consistency of the yield answer. Default is 1.
#'
#'
#' @return A tibble with the ChatGPT answer to your question.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Find your api key at https://platform.openai.com/account/api-keys
#' api_key <- 123456789
#'
#' q <- "What is a carrot?"
#' ask_chatgpt(q, api_key = api_key, sleep_time = 0)
#'
#' }
#'


ask_chatgpt <- function(
    question,
    api_key,
    model = "gpt-3.5-turbo",
    sleep_time = 0,
    time_info = FALSE,
    reps = 1
 ){

  run_ask_chatgpt <- function(
    question,
    api_key,
    model,
    sleep_time,
    time_info
    ){

    Sys.sleep(sleep_time)

    #tictoc::tic()

    # Code reproduced from https://www.r-bloggers.com/2023/03/call-chatgpt-or-really-any-other-api-from-r/

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

    #time <- tictoc::toc(quiet = TRUE)
    #run_time <- round(as.numeric(time$toc - time$tic), 2)
    run_time <- as.numeric(response$times[[6]])

    answer <- stringr::str_trim(httr::content(response)$choices[[1]]$message$content) |>
      stringr::str_replace_all("\n", " ")

    if(rlang::is_empty(answer)){
      answer <- paste0(
        "Error ", as.numeric(response$status_code),
        ". Check https://platform.openai.com/docs/guides/error-codes")

      #run_time <- NA_real_
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
          answer = "ERROR (Have you loaded your API?)",
          run_time = NA_real_
        )
      )

  } else {

  run_ask_chatgpt <-
    purrr::possibly(
      run_ask_chatgpt,
      otherwise = tibble::tibble(
        answer = "ERROR (Have you loaded your API?)"
    )
   )

  }


  if (reps == 1){

    final_res <- run_ask_chatgpt(
      question = question,
      api_key = api_key,
      model = model,
      sleep_time = sleep_time,
      time_info = time_info
    )

  } else if (reps > 1){

    final_res <-
      furrr::future_map_dfr(
        1:reps, \(i) run_ask_chatgpt(
          question = question,
          api_key = api_key,
          model = model,
          sleep_time = sleep_time,
          time_info = time_info
      )
    ) |>
      dplyr::mutate(
        n = 1:reps
      )


  }

  final_res


}



















