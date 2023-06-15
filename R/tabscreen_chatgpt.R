
#' Title and abstract screening with ChatGPT
#'
#' @description This function supports the conduct of title and abstract screening with ChatGPT in R.
#' The function allow to run title and abstract screening across multiple prompts and with
#' repeated questions to check for consistency across answers.
#'
#' @template common-arg
#' @param api_key Numerical value with your personal API key. Find at \url{https://platform.openai.com/account/api-keys}
#' @param arrange_var Function indicating the variables determing the arrangement of the data. Default is \code{studyid}.
#' @param model Character indicating the ChatGPT model to be use. Default is "gpt-3.5-turbo".
#' @param sleep_time Numerical value indicating in seconds the sleeping time in between questions. This
#' is especially helpful when not a pay-as-you-go user. For more information see
#' \url{https://platform.openai.com/docs/guides/rate-limits/what-are-the-rate-limits-for-our-api}
#' @param time_info Logical indicating if the time of the answer should be returned.
#' @param reps Numerical indicating the number of times the same question should be sent to ChatGPT.
#' This can be useful to test consistency between answers. Default is 1.
#' @param seed Numerical value for a seed to ensure that proper,
#' parallel-safe random numbers are produced.
#' @param ... Further time functions to be added to RETRY.
#' See \url{https://httr.r-lib.org/reference/RETRY.html}
#'
#' @return A \code{tibble} with answer and run_time if \code{time_info = TRUE}.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(future)
#'
#' # Find your api key at https://platform.openai.com/account/api-keys
#' api_key <- 123456789
#'
#' data <- load("data.RData")
#' prompts <- paste("Prompt", 1:3)
#'
#' plan(multisession, workers = 7)
#'
#' system.time(
#'  test_dat <-
#'   tabscreen_chatgpt(
#'     data = data,
#'     prompt = prompts,
#'     studyid = studyid,
#'     title = Title,
#'     abstract = Abstract
#'  )
#' )
#'
#' }
#'

tabscreen_chatgpt <-
  function(
    data,
    prompt,
    studyid,
    title,
    abstract,
    api_key = get_api_key(),
    arrange_var = studyid,
    model = "gpt-3.5-turbo",
    sleep_time = 0,
    time_info = FALSE,
    reps = 1,
    seed = NULL,
    ...
  ) {


  if (missing(studyid)){

    dat <-
      data |>
      dplyr::mutate(
       studyid = 1:nrow(data)
      ) |>
      dplyr::relocate(studyid, .before = {{ title }})


  } else {

    dat <-
      data |>
      dplyr::mutate(
        studyid = {{ studyid }}
      ) |>
      dplyr::relocate(studyid, .before = {{ title }})

  }

  dat <-
    dat |>
    dplyr::mutate(
      dplyr::across(c({{ title }}, {{ abstract }}), ~ dplyr::if_else(is.na(.x) | .x == "" | .x == " ", "No information", .x))
    ) |>
    dplyr::slice(rep(1:nrow(dat), each = length(prompt))) |>
    dplyr::mutate(
      prompt = rep(prompt, nrow(dat)),
      question_raw = paste0(
        prompt,
        ". Now, please evaluate the following titles and abstracts for",
        " Study ", studyid, ":",
        " -Title: ", {{ title }},
        " -Abstract: ", {{ abstract }}),
      question = stringr::str_replace_all(question_raw, "\n\n", " "),
      question = stringr::str_remove_all(question, "\n")
    ) |>
    dplyr::select(-question_raw) |>
    dplyr::relocate(prompt, .after = studyid) |>
    dplyr::arrange(prompt, {{ arrange_var }})


  furrr_seed <- if (is.null(seed)) TRUE else NULL

  if (reps == 1){

    answer_dat <-
      dat |>
      dplyr::mutate(
        furrr::future_map_dfr(
          question, ~ ask_chatgpt(
            question = .x,
            api_key = api_key,
            model = model,
            sleep_time = sleep_time,
            time_info = time_info,
            seed = seed
          ),
          ...,
          .options = furrr::furrr_options(seed = furrr_seed)
        )
      )

  } else if (reps > 1) {

    answer_dat <-
      dat |>
      dplyr::mutate(
        res = furrr::future_map(
          question, ~ ask_chatgpt(
            question = .x,
            api_key = api_key,
            model = model,
            sleep_time = sleep_time,
            time_info = time_info,
            reps = reps,
            seed = seed
          ),
          ...,
          .options = furrr::furrr_options(seed = furrr_seed)
        )
      ) |>
      tidyr::unnest(res)

  }

  n_error <- answer_dat |> dplyr::filter(stringr::str_detect(answer, "Error|error")) |> nrow()

  if (n_error > 0){

    message("Request falied for some title and abstracts. Retrying screening for those studies")

    succes_dat <-
      answer_dat |>
      dplyr::filter(!stringr::str_detect(answer, "Error|error"))

    error_dat <-
      answer_dat |>
      dplyr::filter(stringr::str_detect(answer, "Error|error")) |>
      dplyr::select(-answer)

    if (reps == 1){

      answer_dat_retry <-
        error_dat |>
        dplyr::mutate(
          furrr::future_map_dfr(
            question, ~ ask_chatgpt(
              question = .x,
              api_key = api_key,
              model = model,
              sleep_time = sleep_time,
              time_info = time_info,
              seed = seed
            ),
            ...,
            .options = furrr::furrr_options(seed = furrr_seed)
          )
        )

    } else if (reps > 1) {

      answer_dat_retry <-
        error_dat |>
        dplyr::mutate(
          res = furrr::future_map(
            question, ~ ask_chatgpt(
              question = .x,
              api_key = api_key,
              model = model,
              sleep_time = sleep_time,
              time_info = time_info,
              reps = reps,
              seed = seed
            ),
            ...,
            .options = furrr::furrr_options(seed = furrr_seed)
          )
        ) |>
        tidyr::unnest(res)

    }

    answer_dat <-
      dplyr::bind_rows(
        succes_dat,
        answer_dat_retry
      ) |>
      dplyr::arrange(prompt, {{ arrange_var }})


    still_error <- answer_dat |> dplyr::filter(stringr::str_detect(answer, "Error|error")) |> nrow()
    if (still_error > 0) message("NOTE: Request falied for some title and abstracts.")

  }

  tibble::new_tibble(answer_dat, class = "chatgpt")

}



#' Asking a single question to ChatGPT
#'
#' @param question Character with the question you want ChatGPT to answer.
#' @param api_key Numerical value with your personal API key. Find at \url{https://platform.openai.com/account/api-keys}
#' @param model Character indicating the ChatGPT model to be use. Default is "gpt-3.5-turbo".
#' @param sleep_time Numerical value indicating in seconds the sleeping time in between questions. This
#' is especially helpful when not a pay-as-you-go user. For more information see
#' \url{https://platform.openai.com/docs/guides/rate-limits/what-are-the-rate-limits-for-our-api}
#' @param time_info Logical indicating if the time of the answer should be returned.
#' @param reps Numerical indicating the number of times the same question should be sent to ChatGPT.
#' This can be useful to test consistency between answers. Default is 1.
#' @param seed Numerical value for a seed to ensure that proper,
#' parallel-safe random numbers are produced.
#' @param ... Further time functions to be added to RETRY.
#' See \url{https://httr.r-lib.org/reference/RETRY.html}
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
    api_key = get_api_key(),
    model = "gpt-3.5-turbo",
    sleep_time = 0,
    time_info = FALSE,
    reps = 1,
    seed = NULL,
    ...
 ){

  run_ask_chatgpt <- function(
    question,
    api_key,
    model,
    sleep_time,
    time_info,
    ...
    ){

    Sys.sleep(sleep_time)

    tictoc::tic()

    # Raw code reproduced from https://www.r-bloggers.com/2023/03/call-chatgpt-or-really-any-other-api-from-r/

    response <-
      suppressMessages(
      httr::RETRY(
      verb = "POST",
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(Authorization = paste("Bearer", api_key)),
      httr::content_type_json(),
      body = list(
        model = model,
        messages = list(list(
          role = "user",
          content = question
        )
       )
      ),
      encode = "json",
      ...
     )
    )

    time <- tictoc::toc(quiet = TRUE)
    run_time <- round(as.numeric(time$toc - time$tic), 2)
    #run_time <- as.numeric(response$times[[6]])

    answer <- stringr::str_trim(httr::content(response)$choices[[1]]$message$content) |>
      stringr::str_replace_all("\n", " ") |>
      stringr::str_replace_all("\"", "'")

    if(rlang::is_empty(answer)){
      answer <- paste0(
        httr::http_status(response)$message,
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
      suppressWarnings(
      purrr::possibly(
        run_ask_chatgpt,
        otherwise = tibble::tibble(
          answer = "Error (Have you loaded your API?)",
          run_time = NA_real_
        )
       )
      )

  } else {

  run_ask_chatgpt <-
    suppressWarnings(
    purrr::possibly(
      run_ask_chatgpt,
      otherwise = tibble::tibble(
        answer = "Error (Have you loaded your API?)"
      )
     )
    )

  }


  if (reps == 1){

    final_res <- run_ask_chatgpt(
      question = question,
      api_key = api_key,
      model = model,
      sleep_time = sleep_time,
      time_info = time_info,
      ...
    )

  } else if (reps > 1){

    furrr_seed <- if (is.null(seed)) TRUE else NULL

    suppressPackageStartupMessages(
      suppressWarnings(
      final_res <-
        furrr::future_map_dfr(
          1:reps, \(i) run_ask_chatgpt(
            question = question,
            api_key = api_key,
            model = model,
            sleep_time = sleep_time,
            time_info = time_info,
            ...
        ), .options = furrr::furrr_options(seed = furrr_seed)
      ) |>
        dplyr::mutate(
          n = 1:reps
      )
     )
    )

  }

  final_res


}


#' @title  Asking a single question to ChatGPT
#'
#' @description The function makes it possible to ask a question to ChatGPT. If
#' `reps > 1` the same question is asked reps times. This can be useful to test consistency
#'  between answers.
#'
#' @references Wickham H (2023).
#' \emph{httr2: Perform HTTP Requests and Process the Responses}.
#' https://httr2.r-lib.org, https://github.com/r-lib/httr2.
#'
#' @template askgpt-arg
#'
#' @return A tibble including the following variables, the ChatGPT answer,
#' the running time (in seconds) of each request, and the total amount of tokens used.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find your api key at https://platform.openai.com/account/api-keys
#' set_api_key()
#'
#' q <- "What is a banana?"
#' ask_gpt(q)
#' }



ask_gpt <- function(
    question,
    ...,
    time_info = TRUE,
    token_info = TRUE,
    model = "gpt-3.5-turbo-16k-0613",
    role = "user",
    api_key = get_api_key(),
    max_tries = 8,
    max_seconds = NULL,
    is_transient = gpt_is_transient,
    backoff = NULL,
    after = NULL,
    rpm = 3500,
    reps = 1,
    seed = NULL
){

  if (!is.character(question)) stop("The question must be a character string")

  ask_gpt_engine <- function(
    question,
    ...,
    time_info,
    token_info,
    model,
    role,
    api_key,
    max_tries,
    max_seconds,
    is_transient,
    backoff,
    after,
    rpm
  ){

    tictoc::tic()

    body <- list(
      model = model,
      messages = list(list(
        role = role,
        content = question
      )),
      ...
    )


    req <-
      httr2::request("https://api.openai.com/v1/chat/completions") |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", api_key)
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_retry(
        max_tries = max_tries,
        max_seconds = max_seconds,
        is_transient = is_transient,
        backoff = backoff,
        after = after
      ) |>
      httr2::req_throttle(rpm/60) |>
      httr2::req_user_agent("AIscreenR (http://mikkelvembye.github.io/AIscreenR/)")



    if (curl::has_internet()){

      resp <- try(
        suppressMessages(req |> httr2::req_perform()),
        silent = TRUE
      )

      if (status_code() == 200){

        resp <- resp |> httr2::resp_body_json()

        answer <- stringr::str_trim(resp$choices[[1]]$message$content)
        tokens_tot <- resp$usage$total_tokens

      } else {

        answer <- status_code_text()
        tokens_tot <- NA_real_

      }

    } else {

      answer <- "Error: Could not reach host [check internet connection]"
      tokens_tot <- NA_real_

    }

    time <- tictoc::toc(quiet = TRUE)

    run_time <- round(as.numeric(time$toc - time$tic), 1)

    res <- tibble::tibble(
      answer = answer,
      run_time = run_time,
      tokens = tokens_tot
    )

    if (!time_info) res <- res |> dplyr::select(-run_time)
    if (!token_info) res <- res |> dplyr::select(-tokens)


    res


  }


  if (reps == 1) {

    final_res <- ask_gpt_engine(
      question = question,
      ...,
      time_info = time_info,
      token_info = token_info,
      model = model,
      role = role,
      api_key = api_key,
      max_tries = max_tries,
      max_seconds = max_seconds,
      is_transient = is_transient,
      backoff = backoff,
      after = after,
      rpm = rpm
    )

  } else if (reps > 1){

    furrr_seed <- if (is.null(seed)) TRUE else NULL

    suppressPackageStartupMessages(
      suppressWarnings(
        final_res <-
          furrr::future_map_dfr(
            1:reps, \(i) ask_gpt_engine(
              question = question,
              ...,
              time_info = time_info,
              token_info = token_info,
              model = model,
              role = role,
              api_key = api_key,
              max_tries = max_tries,
              max_seconds = max_seconds,
              is_transient = is_transient,
              backoff = backoff,
              after = after,
              rpm = rpm
            ),
            .options = furrr::furrr_options(seed = furrr_seed)
          ) |>
          dplyr::mutate(
            n = 1:reps
          )
      )
    )

  }

  final_res

}


# Helpers
status_code <- function(){

  resp <- httr2::last_response()

  if (!is.null(resp)){

    code <- resp |> httr2::resp_status()

  } else {

    code <- 999

  }

  code

}

status_code_text <- function(){

  resp_last <- httr2::last_response()

  check_string <- "[check https://platform.openai.com/docs/guides/error-codes]"

  if (!is.null(resp_last)){

    code <- resp_last |> httr2::resp_status()

    text <- paste("Error", code)

    if (code == 400) text <- paste("Error", code, "Bad request [check body parameters]")
    if (code == 401) text <- paste("Error", code, "Unauthorized [invalid api]")
    if (code == 404) text <- paste("Error", code, "[check gpt model]")
    if (code == 429 | code == 500) text <- paste("Error", code, check_string)
    if (code == 503) text <- paste("Error", code, "Service Unavailable")

    s_code <- text

  } else {

    s_code <- "Error [could not reach host]"

  }

  s_code

}


gpt_is_transient <- function(resp){
  status_code() == 400 ||  status_code() == 429 || status_code() == 500
}


# Function with encrypt code string

testing_key_chatgpt <- function() {
  httr2::secret_decrypt(
    "4UAcFSIHVz8Z4zED1WEj3k65xFBWlJ8dzavRDGG4dz0pBxEOXtvSkLwK6_fZaZqCr94oVtKBD6DQo82vwa2gljJMTw",
    "AISCREENR_KEY"
    )
}













