
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
#' @return A \code{tibble} with answer
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
#'
#' plan(multisession, workers = 7)
#'
#' system.time(
#'  test_dat <-
#'   tabscreen_chatgpt_engine(
#'     data = data,
#'     prompt = prompts,
#'     studyid = studyid,
#'     title = Title,
#'     abstract = Abstract,
#'     api_key = api_key,
#'     reps = 2
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
    api_key,
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


    still_errror <- answer_dat |> dplyr::filter(stringr::str_detect(answer, "Error|error")) |> nrow()
    if (still_errror > 0) message("NOTE: Request falied for some title and abstracts.")

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
    api_key,
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
      stringr::str_replace_all("\n", " ")

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
      purrr::possibly(
        run_ask_chatgpt,
        otherwise = tibble::tibble(
          answer = "Error (Have you loaded your API?)",
          run_time = NA_real_
        )
      )

  } else {

  run_ask_chatgpt <-
    purrr::possibly(
      run_ask_chatgpt,
      otherwise = tibble::tibble(
        answer = "Error (Have you loaded your API?)"
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

  }

  final_res


}



















