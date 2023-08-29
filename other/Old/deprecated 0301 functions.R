################################################################################
# HTTR2 functions (0301 models)
################################################################################

#' @title Title and abstract screening with ChatGPT (0301/0314 models)
#'
#' @description
#' `r lifecycle::badge("deprecated")`<br>
#' <br>
#' `tabscreen_gpt_0301` will deprecate September 13 when OPEN-AI removes the
#' 0301 and 0314 models. Instead use INSERT FUNCTION WHEN READY.
#' Read more at \url{https://platform.openai.com/docs/deprecations/2023-06-13-updated-chat-models}.
#' <br>
#' <br>
#' This function supports the conduct of title and abstract screening with ChatGPT in R.
#' The function allow to run title and abstract screening across multiple prompts and with
#' repeated questions to check for consistency across answers
#'
#' @references Wickham H (2023).
#' \emph{httr2: Perform HTTP Requests and Process the Responses}.
#' https://httr2.r-lib.org, https://github.com/r-lib/httr2.
#'
#' @template common-arg
#' @param arrange_var Function indicating the variables determing the arrangement of the data. Default is \code{studyid}.
#' @template askgpt-arg
#'
#' @return A \code{tibble} with answer and run_time if \code{time_info = TRUE}.
#'
#' @importFrom stats df
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' library(future)
#'
#' # Find your api key at https://platform.openai.com/account/api-keys
#' set_api_key()
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
#' }

tabscreen_gpt_0301 <- function(
    data,
    prompt,
    studyid,
    title,
    abstract,
    ...,
    arrange_var = studyid,
    time_info = TRUE,
    token_info = TRUE,
    model = "gpt-3.5-turbo-0301",
    role = "user",
    api_key = get_api_key(),
    max_tries = 2,
    max_seconds = NULL,
    is_transient = gpt_is_transient,
    backoff = NULL,
    after = NULL,
    rpm = 3500,
    reps = 1,
    seed = NULL
){

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

  question_dat <-
    dat |>
    dplyr::mutate(
      dplyr::across(c({{ title }}, {{ abstract }}), ~ dplyr::if_else(
        is.na(.x) | .x == "" | .x == " ", "No information", .x)
      )
    ) |>
    dplyr::slice(rep(1:nrow(dat), length(prompt))) |>
    dplyr::mutate(
      prompt = rep(prompt, each = dplyr::n_distinct(studyid))
    ) |>
    dplyr::slice(rep(1:dplyr::n(), each = length(model))) |>
    dplyr::mutate(
      model = rep(model, dplyr::n_distinct(studyid)*dplyr::n_distinct(prompt)),
      question_raw = paste0(
        prompt,
        " Now, please evaluate the following titles and abstracts for",
        " Study ", studyid, ":",
        " -Title: ", {{ title }},
        " -Abstract: ", {{ abstract }}),
      question = stringr::str_replace_all(question_raw, "\n\n", " "),
      question = stringr::str_remove_all(question, "\n")
    ) |>
    dplyr::select(-question_raw) |>
    dplyr::arrange(model, prompt, {{ arrange_var }})

  furrr_seed <- if (is.null(seed)) TRUE else NULL

  answer_dat <-
    question_dat |>
    dplyr::mutate(
      res = furrr::future_map2(
        .x = question, .y = model, ~ ask_gpt_0301(
          question = .x,
          model = .y,
          time_info = time_info,
          token_info = token_info,
          role = role,
          api_key = api_key,
          max_tries = max_tries,
          max_seconds = max_seconds,
          is_transient = is_transient,
          backoff = backoff,
          after = after,
          rpm = rpm,
          reps = reps,
          seed = seed
        ),
        #...,
        .options = furrr::furrr_options(seed = furrr_seed))
    ) |>
    tidyr::unnest(res)

  n_error <- answer_dat |> dplyr::filter(stringr::str_detect(answer, "Error|error")) |> nrow()

  if (n_error > 0){

    succes_dat <- answer_dat |>
      filter(!stringr::str_detect(answer, "Error|error"))


    error_dat <- answer_dat |>
      filter(stringr::str_detect(answer, "Error|error")) |>
      dplyr::mutate(
        furrr::future_map_dfr(
          .x = question, .y = model, ~ ask_gpt_0301(
            question = .x,
            model = .y,
            time_info = time_info,
            token_info = token_info,
            role = role,
            api_key = api_key,
            max_tries = max_tries,
            max_seconds = max_seconds,
            is_transient = is_transient,
            backoff = backoff,
            after = after,
            rpm = rpm,
            reps = reps,
            seed = seed
          ),
          .options = furrr::furrr_options(seed = TRUE))
      )

    answer_dat <-
      dplyr::bind_rows(
        succes_dat,
        error_dat
      ) |>
      dplyr::arrange(model, prompt, {{ arrange_var }})

    still_error <- answer_dat |> dplyr::filter(stringr::str_detect(answer, "Error|error")) |> nrow()
    if (still_error > 0) message("NOTE: Request falied for some title and abstracts.")

  }

  tibble::new_tibble(answer_dat, class = "chatgpt")

}


#' @title  Asking a single question to ChatGPT (0301/0314 models)
#'
#' @description
#' `r lifecycle::badge("deprecated")`<br>
#' <br>
#' `tabscreen_gpt_0301` will deprecate September 13 when OPEN-AI removes the
#' 0301 and 0314 models. Instead use INSERT FUNCTION WHEN READY.
#' Read more at \url{https://platform.openai.com/docs/deprecations/2023-06-13-updated-chat-models}.
#' <br>
#' <br>
#' The function makes it possible to ask a question to ChatGPT. If
#' `reps > 1` the same question is asked reps times. This can be useful to test consistency
#' between answers.
#'
#' @references Wickham H (2023).
#' \emph{httr2: Perform HTTP Requests and Process the Responses}.
#' https://httr2.r-lib.org, https://github.com/r-lib/httr2.
#'
#' @param question Character string with the question you want ChatGPT to answer.
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
#' ask_gpt_0301(q)
#' }



ask_gpt_0301 <- function(
    question,
    ...,
    time_info = TRUE,
    token_info = TRUE,
    model = "gpt-3.5-turbo-0301",
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

  ask_gpt_engine_0301 <- function(
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

    final_res <- ask_gpt_engine_0301(
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
            1:reps, \(i) ask_gpt_engine_0301(
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
