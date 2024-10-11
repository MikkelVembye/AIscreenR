
#' @title Title and abstract screening with GPT API models using function calls.
#'
#' @description
#' `r lifecycle::badge("stable")`<br>
#' <br>
#' This function supports the conduct of title and abstract screening with GPT API models in R.
#' The function allows to run title and abstract screening across multiple prompts and with
#' repeated questions to check for consistency across answers. This function draws
#' on the newly developed function calling to better steer the output of the responses.
#'
#' @references Vembye, M. H., Christensen, J., Mølgaard, A. B., & Schytt, F. L. W. (Under preparation)
#'   \emph{GPT API Models Can Function as Highly Reliable Second Screeners of Titles and Abstracts in Systematic Reviews:
#'   A Proof of Concept and Common Guidelines}
#'
#' Wickham H (2023).
#' \emph{httr2: Perform HTTP Requests and Process the Responses}.
#' https://httr2.r-lib.org, https://github.com/r-lib/httr2.
#'
#' @template common-arg
#' @param ... Further argument to pass to the request body.
#'   See \url{https://platform.openai.com/docs/api-reference/chat/create}.
#' @param model Character string with the name of the completion model. Can take
#'   multiple models, including gpt-4 models. Default = `"gpt-4"` (i.e., gpt-4-0613). This model has
#'   been shown to outperform the gpt-3.5-turbo models in terms of its ability to detect
#'   relevant studies (Vembye et al., Under preparation).
#'   Find available model at
#' \url{https://platform.openai.com/docs/models/model-endpoint-compatibility}.
#' @param role Character string indicate the role of the user. Default is `"user"`.
#' @param functions Function to steer output. Default is `incl_function_simple`.
#'   To get detailed responses use the `AIscreenR:::incl_function`. Also see 'Examples below.
#'   Find further documentation for function calling at
#'   \url{https://openai.com/blog/function-calling-and-other-api-updates}.
#' @param function_call_name Functions to call.
#'   Default is `list(name = "inclusion_decision_simple")`. To get detailed responses
#'   use `list(name = "inclusion_decision")`. Also see 'Examples below.
#' @param top_p 'An alternative to sampling with temperature, called nucleus sampling,
#'   where the model considers the results of the tokens with top_p probability mass.
#'   So 0.1 means only the tokens comprising the top 10% probability mass are considered.
#'   We generally recommend altering this or temperature but not both.' (OPEN-AI). Default is 1.
#'   Find documentation at
#' \url{https://platform.openai.com/docs/api-reference/chat/create#chat/create-top_p}.
#' @param time_info Logical indicating whether the run time of each
#'   request/question should be included in the data. Default = `TRUE`.
#' @param token_info Logical indicating whether the number of prompt and completion tokens
#'   per request should be included in the output data. Default = `TRUE`. When `TRUE`,
#'   the output object will include price information of the conducted screening.
#' @param api_key Numerical value with your personal API key. Find at
#'  \url{https://platform.openai.com/account/api-keys}. Use
#'  [secret_make_key()], [secret_encrypt()], and
#'  [secret_decrypt()] to scramble and decrypt the api key and
#'  use [set_api_key()] to securely automate the use of the
#'  api key by setting the api key as a locale environment variable.
#' @param max_tries,max_seconds 'Cap the maximum number of attempts with
#'  `max_tries` or the total elapsed time from the first request with
#'  `max_seconds`. If neither option is supplied (the default), [req_perform()]
#'  will not retry' (Wickham, 2023).
#' @param is_transient 'A predicate function that takes a single argument
#'  (the response) and returns `TRUE` or `FALSE` specifying whether or not
#'  the response represents a transient error' (Wickham, 2023).
#' @param backoff 'A function that takes a single argument (the number of failed
#'   attempts so far) and returns the number of seconds to wait' (Wickham, 2023).
#' @param after 'A function that takes a single argument (the response) and
#'   returns either a number of seconds to wait or `NULL`, which indicates
#'   that a precise wait time is not available that the `backoff` strategy
#'   should be used instead' (Wickham, 2023).
#' @param rpm Numerical value indicating the number of requests per minute (rpm)
#'   available for the specified api key. Find more information at
#'   \url{https://platform.openai.com/docs/guides/rate-limits/what-are-the-rate-limits-for-our-api}.
#'   Alternatively, use [rate_limits_per_minute()].
#' @param reps Numerical value indicating the number of times the same
#'   question should be sent to OpenAI's GPT API models. This can be useful to test consistency
#'   between answers. Default is `1` but when using 3.5 models, we recommend setting this
#'   value to `10`.
#' @param seed Numerical value for a seed to ensure that proper,
#'   parallel-safe random numbers are produced.
#' @param progress Logical indicating whether a progress line should be shown when running
#'   the title and abstract screening in parallel. Default is `TRUE`.
#' @param messages Logical indicating whether to print messages embedded in the function.
#'   Defualt is `TRUE`.
#' @param incl_cutoff_upper Numerical value indicating the probability threshold
#'   for which a studies should be included. Default is 0.5, which indicates that
#'   titles and abstracts that OpenAI's GPT API model has included more than 50 percent of the times
#'   should be included.
#' @param incl_cutoff_lower Numerical value indicating the probability threshold
#'   above which studies should be check by a human. Default is 0.4, which means
#'   that if you ask OpenAI's GPT API model the same questions 10 times and it includes the
#'   title and abstract 4 times, we suggest that the study should be check by a human.
#' @param force Logical argument indicating whether to force the function to use more than
#'   10 iterations for gpt-3.5 models and more than 1 iteration for gpt-4 models.
#'   This argument is developed to avoid the conduct of wrong and extreme sized screening.
#'   Default is `FALSE`.
#'
#'
#' @return An object of class \code{"chatgpt"}. The object is a list containing the following
#' components:
#' \item{answer_data_sum}{dataset with the summarized, probabilistic inclusion decision
#' for each title and abstract across multiple repeated questions.}
#' \item{answer_data_all}{dataset with all individual answers.}
#' \item{price}{numerical value indicating the total price (in USD) of the screening.}
#' \item{price_data}{dataset with prices across all gpt models used for screening.}
#'
#' @note The \code{answer_data_sum} data contains the following mandatory variables:
#' \tabular{lll}{
#'  \bold{studyid} \tab \code{integer} \tab indicating the study ID of the reference. \cr
#'  \bold{title} \tab \code{character} \tab indicating the title of the reference. \cr
#'  \bold{abstract} \tab \code{character}   \tab indicating the abstract of the reference. \cr
#'  \bold{promptid} \tab \code{integer} \tab indicating the prompt ID. \cr
#'  \bold{prompt} \tab \code{character} \tab indicating the prompt. \cr
#'  \bold{model} \tab \code{character}   \tab indicating the specific gpt-model used. \cr
#'  \bold{question} \tab \code{character} \tab indicating the final question sent to OpenAI's GPT API models. \cr
#'  \bold{top_p} \tab \code{numeric}  \tab indicating the applied top_p. \cr
#'  \bold{incl_p} \tab \code{numeric}  \tab indicating the probability of inclusion calculated across multiple repeated responses on the same title and abstract. \cr
#'  \bold{final_decision_gpt} \tab \code{character} \tab indicating the final decision reached by gpt - either 'Include', 'Exclude', or 'Check'. \cr
#'  \bold{final_decision_gpt_num}  \tab \code{integer}  \tab indicating the final numeric decision reached by gpt - either 1 or 0. \cr
#'  \bold{longest_answer}  \tab \code{character} \tab indicating the longest gpt response obtained
#'  across multiple repeated responses on the same title and abstract. Only included if the detailed function calling
#'  function is used. See 'Examples' below for how to use this function. \cr
#'  \bold{reps}  \tab \code{integer}  \tab indicating the number of times the same question has been sent to OpenAI's GPT API models. \cr
#'  \bold{n_mis_answers} \tab \code{integer} \tab indicating the number of missing responses. \cr
#' }
#' <br>
#' The \code{answer_data_all} data contains the following mandatory variables:
#' \tabular{lll}{
#'  \bold{studyid} \tab \code{integer} \tab indicating the study ID of the reference. \cr
#'  \bold{title} \tab \code{character} \tab indicating the title of the reference. \cr
#'  \bold{abstract} \tab \code{character}   \tab indicating the abstract of the reference. \cr
#'  \bold{promptid} \tab \code{integer} \tab indicating the prompt ID. \cr
#'  \bold{prompt} \tab \code{character} \tab indicating the prompt. \cr
#'  \bold{model} \tab \code{character}   \tab indicating the specific gpt-model used. \cr
#'  \bold{iterations} \tab \code{numeric} \tab indicating the number of times the same question has been sent to OpenAI's GPT API models. \cr
#'  \bold{question} \tab \code{character} \tab indicating the final question sent to OpenAI's GPT API models. \cr
#'  \bold{top_p}  \tab \code{numeric} \tab indicating the applied top_p. \cr
#'  \bold{decision_gpt}  \tab \code{character} \tab indicating the raw gpt decision - either \code{"1", "0", "1.1"} for inclusion, exclusion, or uncertainty, respectively. \cr
#'  \bold{detailed_description}  \tab \code{character} \tab indicating detailed description of the given decision made by OpenAI's GPT API models.
#'  Only included if the detailed function calling function is used. See 'Examples' below for how to use this function. \cr
#'  \bold{decision_binary}  \tab \code{integer} \tab indicating the binary gpt decision,
#'  that is 1 for inclusion and 0 for exclusion. 1.1 decision are coded equal to 1 in this case. \cr
#'  \bold{prompt_tokens}  \tab \code{integer} \tab indicating the number of prompt tokens sent to the server for the given request. \cr
#'  \bold{completion_tokens}  \tab \code{integer} \tab indicating the number of completion tokens sent to the server for the given request. \cr
#'  \bold{run_time}  \tab \code{numeric} \tab indicating the time it took to obtain a response from the server for the given request. \cr
#'  \bold{n} \tab \code{integer} \tab indicating request ID.  \cr
#' }
#' <br>
#' If any requests failed to reach the server, the `chatgpt` object contains an
#' error data set (`error_data`) having the same variables as `answer_data_all`
#' but with failed request references only.
#'
#' <br>
#'
#' The \code{price_data} data contains the following variables:
#' \tabular{lll}{
#'  \bold{model} \tab \code{character} \tab gpt model. \cr
#'  \bold{input_price_dollar} \tab \code{integer} \tab price for all prompt/input tokens for the correspondent gpt-model. \cr
#'  \bold{output_price_dollar}  \tab \code{integer} \tab price for all completion/output tokens for the correspondent gpt-model. \cr
#'  \bold{price_total_dollar} \tab \code{integer} \tab total price for all tokens for the correspondent gpt-model. \cr
#' }
#'
#' Find current token pricing at \url{https://openai.com/pricing}.
#'
#' @importFrom stats df
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#'
#' set_api_key()
#'
#' prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"
#'
#' tabscreen_gpt(
#'   data = filges2015_dat[1:2,],
#'   prompt = prompt,
#'   studyid = studyid,
#'   title = title,
#'   abstract = abstract,
#'   max_tries = 2,
#'   )
#'
#'  # Get detailed descriptions of the gpt decisions by using the
#'  # embedded function calling functions from the package. See example below.
#'  tabscreen_gpt(
#'    data = filges2015_dat[1:2,],
#'    prompt = prompt,
#'    studyid = studyid,
#'    title = title,
#'    abstract = abstract,
#'    functions = AIscreenR:::incl_function,
#'    function_call_name = list(name = "inclusion_decision"),
#'    max_tries = 2
#'  )
#'}


tabscreen_gpt <- function(
  data,
  prompt,
  studyid,
  title,
  abstract,
  ...,
  #arrange_var = studyid,
  model = "gpt-4",
  role = "user",
  functions = incl_function_simple,
  function_call_name = list(name = "inclusion_decision_simple"),
  top_p = 1,
  time_info = TRUE,
  token_info = TRUE,
  api_key = get_api_key(),
  max_tries = 16,
  max_seconds = NULL,
  is_transient = gpt_is_transient,
  backoff = NULL,
  after = NULL,
  rpm = 10000,
  reps = 1,
  seed = NULL,
  progress = TRUE,
  messages = TRUE,
  incl_cutoff_upper = 0.5,
  incl_cutoff_lower = incl_cutoff_upper - 0.1,
  force = FALSE
  ){


  if (as.Date(Sys.time()) > as.Date("2024-09-13") && model == "gpt-3.5-turbo-0613"){
    stop("The gpt-3.5-turbo-0613 model has deprecated and can no longer be used.")
  } else{
    message("Note that the gpt-3.5-turbo-0613 deprecates Septemper 13 2024 and can no long be used there after.")
  }


  # Stop warnings
  # Ensuring that users do not conduct wrong screening
  if (max(reps) > 10 && !force){
    max_reps_mes <- paste("* Are you sure you want to use", max(reps), "iterations? If so, set force = TRUE")
    stop(max_reps_mes)
  }


  if (any(stringr::str_detect(model, "gpt-4")) && max(reps) > 1 && !force){

    gpt4_reps <- tibble::tibble(model, reps) |> filter(stringr::str_detect(model, "gpt-4")) |> pull(reps) |> max()

    if (gpt4_reps > 1){

      max_reps_mes_gpt4 <-
        paste("* Are you sure you want to use", gpt4_reps, "iterations with gpt-4?",
              "If so, set force = TRUE")
      stop(max_reps_mes_gpt4)

    }

  }

  if (length(rpm) > 1 && length(model) != length(rpm)){
    stop("model and rpm must be of the same length.")
  }

  if (length(reps) > 1 && length(model) != length(reps)){
    stop("model and reps must be of the same length.")
  }


  if (any(!is.element(model, c(
    "gpt-3.5-turbo", "gpt-3.5-turbo-0613", "gpt-3.5-turbo-1106",
    "gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613",
    "gpt-4", "gpt-4-0613",
    "gpt-4-32k", "gpt-4-32k-0613", "gpt-4-1106-preview", "gpt-4-vision-preview"
  )))) stop("Unknown gpt model(s) used - check model name(s).")


  if(incl_cutoff_upper < incl_cutoff_lower){
    stop("incl_cutoff_lower must not exceed incl_cutoff_upper")
  }

  if (!missing(prompt)){
    if (n_distinct(prompt) != length(prompt)) stop("Do not add the same prompt twice.")
  }

  if (n_distinct(reps) == 1 && n_distinct(model) != length(model)){
    model <- unique(model)
  }

  # Collecting all arguments to be used in the screen_errors function
  arg_list <-
    list(
      role = role,
      functions = functions,
      function_call_name = function_call_name,
      time_info = time_info,
      token_info = token_info,
      max_tries = max_tries,
      max_seconds = max_seconds,
      is_transient = is_transient,
      backoff = backoff,
      after = after,
      seed = seed,
      progress = progress,
      messages = messages,
      incl_cutoff_upper = incl_cutoff_upper,
      incl_cutoff_lower = incl_cutoff_lower,
      ...
    )

  if ("max_tokens" %in% names(arg_list)){
    if (arg_list$max_tokens < 11) stop("Cannot retrieve results from server with tokens below 11.")
  }


  if (is_chatgpt_tbl(data) && !"topp" %in% names(data)) data <- data |> tibble::as_tibble()

  ###############################################################
  # Function to send a single request to OpenAI's GPT API models
  ###############################################################

  ask_gpt_engine <- function(
    body,
    RPM,
    timeinf = time_info,
    tokeninf = token_info,
    key = api_key,
    max_t = max_tries,
    max_s = max_seconds,
    is_trans = is_transient,
    back = backoff,
    aft = after
  ){

    detailed <- body$function_call$name == "inclusion_decision"

    if (RPM > 10000) RPM <- 10000
    #if (RPM > 200 && stringr::str_detect(body$model, "gpt-4")) RPM <- 200

    if (max_t == 0) max_t <- is_trans <- NULL

    tictoc::tic()

    url <- "https://api.openai.com/v1/chat/completions"

    req <-
      httr2::request(url) |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", key)
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_retry(
        max_tries = max_t,
        max_seconds = max_s,
        is_transient = is_trans,
        backoff = back,
        after = aft
      ) |>
      httr2::req_throttle(RPM/60) |>
      httr2::req_user_agent("AIscreenR (http://mikkelvembye.github.io/AIscreenR/)")


    if (curl::has_internet()){

      resp <- try(
        suppressMessages(req |> httr2::req_perform()),
        silent = TRUE
      )

      if (status_code() == 200){

        resp <- resp |> httr2::resp_body_json()

        resp_text <- resp$choices[[1]]$message$function_call$arguments

        if (!stringr::str_detect(resp_text, '\"\n\\}|\" \n\\}|\"  \n\\}|\n\\}')){

          resp_text <- paste0(resp_text, '\"\n}')

        }

        if (!stringr::str_detect(resp_text, '\\}') && stringr::str_detect(resp_text, '\"\n|\" \n|\"  \n') || stringr::str_ends(resp_text, '\n')) {

          resp_text <- paste0(resp_text, '}')

        }

        if (detailed){

          res <-
            tibble::as_tibble(jsonlite::fromJSON(resp_text)) |>
            dplyr::mutate(

              decision_binary = as.numeric(
                dplyr::if_else(stringr::str_detect(decision_gpt, "1"), 1, 0, missing = NA_real_)
              ),

              detailed_description = dplyr::if_else(
                is.na(decision_binary), "Error: Something went wrong [Try again]", detailed_description,
                missing = "Error: Something went wrong [Try again]"
              ),

              prompt_tokens = resp$usage$prompt_tokens,
              completion_tokens = resp$usage$completion_tokens

            )

        } else {

          res <-
            tibble::as_tibble(jsonlite::fromJSON(resp_text)) |>
            dplyr::mutate(

              decision_binary = as.numeric(
                dplyr::if_else(stringr::str_detect(decision_gpt, "1"), 1, 0, missing = NA_real_)
              ),

              prompt_tokens = resp$usage$prompt_tokens,
              completion_tokens = resp$usage$completion_tokens

            )

        }


      } else {

        # The following code must be made more generic in order to take in further
        # functions

        detail_desc <- if(detailed) NA_character_ else NULL

        res <- tibble::tibble(
          decision_gpt = status_code_text(),
          detailed_description = detail_desc,
          decision_binary = NA_real_,
          prompt_tokens = NA_real_,
          completion_tokens = NA_real_
        )

      }

    } else {

      detail_desc <- if(detailed) NA_character_ else NULL

      res <- tibble::tibble(
        decision_gpt = "Error: Could not reach host [check internet connection]",
        detailed_description = detail_desc,
        decision_binary = NA_real_,
        prompt_tokens = NA_real_,
        completion_tokens = NA_real_
      )

    }


    time <- tictoc::toc(quiet = TRUE)

    run_time <- round(as.numeric(time$toc - time$tic), 1)

    res <- res |> dplyr::mutate(run_time = run_time)

    if (!timeinf) res <- res |> dplyr::select(-run_time)
    if (!tokeninf) res <- res |> dplyr::select(-c(prompt_tokens, completion_tokens))


    res


  }

  t_info <- if (time_info) NA_real_ else NULL
  p_tokens <- c_tokens <- if (token_info) NA_real_ else NULL

  # error handling
  if (function_call_name$name == "inclusion_decision_simple"){

    ask_gpt_engine <-
      suppressWarnings(
        purrr::possibly(
          ask_gpt_engine,
          otherwise = tibble::tibble(
            decision_gpt = "Error [possibly a JSON error]",
            decision_binary = NA_real_,
            prompt_tokens = p_tokens,
            completion_tokens = c_tokens,
            run_time = t_info
          )
        )
      )

  } else {

    ask_gpt_engine <-
      suppressWarnings(
        purrr::possibly(
          ask_gpt_engine,
          otherwise = tibble::tibble(
            decision_gpt = "Error [possibly a JSON error]",
            detailed_description = NA_character_,
            decision_binary = NA_real_,
            prompt_tokens = p_tokens,
            completion_tokens = c_tokens,
            run_time = t_info
          )
        )
      )

  }

  ################################################################
  # Function to send repeated requests to OpenAI's GPT API models
  ################################################################

  ask_gpt <- function(
    question,
    model_gpt,
    topp,
    iterations,
    req_per_min,
    ...,
    role_gpt = role,
    funcs = functions,
    func_call_name = function_call_name,
    seeds = seed
  ){


    body <- list(
      model = model_gpt,
      messages = list(
        list(
          role = role_gpt,
          content = question
        )
      ),
      functions = funcs,
      function_call = func_call_name,
      top_p = topp,
      ...
    )


    if(iterations > 1) iterations <- 1:iterations

    furrr_seed <- if (is.null(seeds)) TRUE else NULL

    final_res <-
      furrr::future_map_dfr(
        iterations, \(i) ask_gpt_engine(body = body, RPM = req_per_min),
        .options = furrr::furrr_options(seed = furrr_seed)
      ) |>
      dplyr::mutate(n = iterations)


    final_res

  }

#  t_info <- if (time_info) NA_real_ else NULL
#  p_tokens <- c_tokens <- if (token_info) NA_real_ else NULL
#
#  # error handling
#  if (function_call_name$name == "inclusion_decision_simple"){
#
#    ask_gpt <-
#      suppressWarnings(
#        purrr::possibly(
#          ask_gpt,
#          otherwise = tibble::tibble(
#            decision_gpt = "JSON error",
#            decision_binary = NA_real_,
#            prompt_tokens = p_tokens,
#            completion_tokens = c_tokens,
#            run_time = t_info,
#            n = NA_integer_
#          )
#        )
#      )
#
#  } else {
#
#    ask_gpt <-
#      suppressWarnings(
#        purrr::possibly(
#          ask_gpt,
#          otherwise = tibble::tibble(
#            decision_gpt = "JSON error",
#            detailed_description = NA_character_,
#            decision_binary = NA_real_,
#            prompt_tokens = p_tokens,
#            completion_tokens = c_tokens,
#            run_time = t_info,
#            n = NA_integer_
#          )
#        )
#      )
#
#  }


  ###############################################
  # Data manipulation
  ###############################################


  if (!is_chatgpt_tbl(data)){

    if (missing(studyid)){

      dat <-
        data |>
        dplyr::mutate(
          studyid = 1:nrow(data)
        ) |>
        dplyr::relocate(studyid, .before = {{ title }}) |>
        dplyr::relocate({{ abstract }}, .after = {{ title }}) |>
        dplyr::relocate(c(studyid, {{ title }}, {{ abstract }}), .after = last_col())


    } else {

      dat <-
        data |>
        dplyr::mutate(
          studyid = {{ studyid }}
        ) |>
        dplyr::relocate(studyid, .before = {{ title }}) |>
        dplyr::relocate({{ abstract }}, .after = {{ title }}) |>
        dplyr::relocate(c(studyid, {{ title }}, {{ abstract }}), .after = last_col())

    }


    mp_reps <- if (length(reps) > 1) 1 else length(model)
    mp_rpm <- if (length(rpm) > 1) 1 else length(model)

    model_length <- length(model)
    prompt_length <- length(prompt)
    studyid_length <- dplyr::n_distinct(dat$studyid)

    question_dat <-
      dat |>
      dplyr::mutate(
        dplyr::across(c({{ title }}, {{ abstract }}), ~ dplyr::if_else(
          is.na(.x) | .x == "" | .x == " " | .x == "NA", "No information", .x, missing = "No information")
        )
      ) |>
      dplyr::slice(rep(1:nrow(dat), prompt_length)) |>
      dplyr::mutate(
        promptid = rep(1:prompt_length, each = studyid_length),
        prompt = rep(prompt, each = studyid_length)
      ) |>
      dplyr::slice(rep(1:dplyr::n(), each = model_length)) |>
      dplyr::mutate(
        model = rep(model, studyid_length*prompt_length),
        iterations = rep(reps, studyid_length*prompt_length*mp_reps),
        req_per_min = rep(rpm, studyid_length*prompt_length*mp_rpm),
        question_raw = paste0(
          prompt,
          " Now, evaluate the following title and abstract for",
          " Study ", studyid, ":",
          " -Title: ", {{ title }},
          " -Abstract: ", {{ abstract }}
        ),
        question = stringr::str_replace_all(question_raw, "\n\n", " "),
        question = stringr::str_remove_all(question, "\n")
      ) |>
      dplyr::select(-question_raw) |>
      dplyr::slice(rep(1:dplyr::n(), each = length(top_p))) |>
      mutate(
        topp = rep(top_p, studyid_length*prompt_length*model_length)
      ) |>
      dplyr::arrange(promptid, model, topp, iterations, studyid)

    # For checks of whether multiple reps are used with gpt4 models
    if (any(stringr::str_detect(model, "gpt-4"))){
      max_reps_gpt4 <-
        question_dat |>
        filter(stringr::str_detect(model, "gpt-4")) |>
        summarise(
          max_reps = max(iterations, na.rm = TRUE)
        ) |>
        pull(max_reps)
    }

    # Approximate prize
    app_price_dat <-
      question_dat |>
      mutate(
        prompt_tokens = round(stringr::str_count(question, '\\w+') * 1.6),
        completion_tokens = 11 # Average number of completion tokens for the inclusion_decision_simple function
      ) |>
      filter(!is.na(prompt_tokens) | !is.na(completion_tokens)) |>
      dplyr::rowwise() |>
      mutate(

        input_price = case_when(
          any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ round(prompt_tokens * (0.0015/1000) * iterations, 4),
          any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ round(prompt_tokens * (0.003/1000) * iterations, 4),
          any(c("gpt-4", "gpt-4-0613") %in% model) ~ round(prompt_tokens * (0.03/1000) * iterations, 4),
          any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ round(prompt_tokens * (0.06/1000) * iterations, 4),
          TRUE ~ NA_real_
        ),

        output_price = case_when(
          any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ completion_tokens * (0.002/1000) * iterations,
          any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ completion_tokens * (0.004/1000) * iterations,
          any(c("gpt-4", "gpt-4-0613") %in% model) ~ completion_tokens * (0.06/1000) * iterations,
          any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ completion_tokens * (0.12/1000) * iterations,
          TRUE ~ NA_real_
        )

      ) |>
      ungroup() |>
      summarise(

        iterations = unique(iterations),
        input_price_dollar = sum(input_price, na.rm = TRUE),
        output_price_dollar = sum(output_price, na.rm = TRUE),
        total_price_dollar = round(input_price_dollar + output_price_dollar, 4),

        .by = c(model, iterations)

      )

    app_price <- sum(app_price_dat$total_price_dollar, na.rm = TRUE)


    if (messages){

      message(paste0("* The approximate price of the current (simple) screening will be around $", app_price, "."))

      if (functions[[1]]$name == "inclusion_decision"){
        message(
          paste0(
            "* Be aware that getting detailed reponses from ChatGPT ",
            "will substantially increase the prize of the screening relative to the noted approximate prize."
          )
        )
      }

#      if (any(stringr::str_detect(model, "gpt-4")) && max_reps_gpt4 > 1){
#        message("* Consider to reduce reps to 1 for gpt-4 models.")
#      }


      if ("No information" %in% unique(question_dat$abstract)) {
        message(
          paste0(
            "* Consider removing references that has no abstract ",
            "since these can distort the accuracy of the screening"
          )
        )
      }
    }


#    if (!messages && any(stringr::str_detect(model, "gpt-4")) && max_reps_gpt4 > 1){
#
#      warn <- "* Be aware that using gpt-4 models cost at least 10 times more than gpt-3.5 models.\n"
#      price <- paste0("* The approximate price of the current (simple) screening will be around $", app_price, ".")
#
#      if (functions[[1]]$name == "inclusion_decision"){
#        detail_mess <- paste0(
#          "\n* Be aware that getting detailed reponses from ChatGPT ",
#          "will substantially increase the prize of the screening relative to the noted approximate prize."
#        )
#      } else {
#        detail_mess <- NULL
#      }
#
#      reps_mes <- "\n* Consider to reduce reps to 1 for gpt-4 models." else NULL
#      warn_message <- paste0(warn, price, detail_mess, reps_mes)
#
#      message(warn_message)
#
#    }

    # RUNNING QUESTIONS
    furrr_seed <- if (is.null(seed)) TRUE else NULL

    params <- question_dat |>
      dplyr::select(question, model_gpt = model, topp, iterations, req_per_min)

    answer_dat <-
      question_dat |>
      dplyr::mutate(
        res = furrr::future_pmap(
          .l = params,
          .f = ask_gpt,
          ...,
          .options = furrr::furrr_options(seed = furrr_seed),
          .progress = progress
        )
      ) |>
      tidyr::unnest(res) |>
      tibble::new_tibble(class = "chatgpt_tbl")

    n_error <- answer_dat |> dplyr::filter(is.na(decision_binary)) |> nrow()

    if (messages){

      if (n_error == 1) message(paste("* NOTE: Requests failed for 1 title and abstract."))
      if (n_error > 1) message(paste("* NOTE: Requests failed", n_error, "times."))

    }

    if (n_error > 0) error_refs <- answer_dat |> dplyr::filter(is.na(decision_binary))

  }

  if (is_chatgpt_tbl(data) && missing(prompt) && missing(studyid) && missing(title) && missing(abstract)) {

    org_n <- data |> pull(n)

    params <- data |>
      mutate(iterations = 1) |>
      dplyr::select(question, model_gpt = model, topp, iterations, req_per_min)

    question_dat <-
      data |>
      dplyr::select(1:topp)

    furrr_seed <- if (is.null(seed)) TRUE else NULL

    answer_dat <-
      question_dat |>
      dplyr::mutate(
        res = furrr::future_pmap(
          .l = params,
          .f = ask_gpt,
          ...,
          .options = furrr::furrr_options(seed = furrr_seed),
          .progress = progress
        )
      ) |>
      tidyr::unnest(res) |>
      mutate(n = org_n) |>
      tibble::new_tibble(class = "chatgpt_tbl")

    n_error <- answer_dat |> dplyr::filter(is.na(decision_binary)) |> nrow()

    if (n_error > 0) error_refs <- answer_dat |> dplyr::filter(is.na(decision_binary))

  }

  #answer_dat <- tibble::new_tibble(answer_dat, class = "chatgpt_tbl")

  if (token_info){

    price_dat <-
      answer_dat |>
      filter(!is.na(prompt_tokens) | !is.na(completion_tokens)) |>
      summarise(

        input_price_dollar = case_when(
          any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.0015/1000), 4),
          any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.003/1000), 4),
          any(c("gpt-4", "gpt-4-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.03/1000), 4),
          any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.06/1000), 4),
          TRUE ~ NA_real_
        ),

        output_price_dollar = case_when(
          any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.002/1000),
          any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.004/1000),
          any(c("gpt-4", "gpt-4-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.06/1000),
          any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.12/1000),
          TRUE ~ NA_real_
        ),

        price_total_dollar = round(input_price_dollar + output_price_dollar, 4),

        .by = c(model, iterations)

      )

    price <- sum(price_dat$price_total_dollar, na.rm = TRUE)

  }


  sum_dat <-
    answer_dat |>
    summarise(

      incl_p = mean(decision_binary == 1, na.rm = TRUE),

      final_decision_gpt = dplyr::case_when(
        incl_p < incl_cutoff_upper & incl_p >= incl_cutoff_lower ~ "Check",
        incl_p >= incl_cutoff_upper ~ "Include",
        incl_p < incl_cutoff_lower ~ "Exclude",
        TRUE ~ NA_character_
      ),

      final_decision_gpt_num = dplyr::case_when(
        incl_p < incl_cutoff_upper & incl_p >= incl_cutoff_lower ~ 1,
        incl_p >= incl_cutoff_upper ~ 1,
        incl_p < incl_cutoff_lower ~ 0,
        TRUE ~ NA_real_
      ),

      reps = n(),

      n_mis_answers = sum(is.na(decision_binary)),

      .by = c(studyid:topp)

    )

  if ("detailed_description" %in% names(answer_dat)){


    long_answer_dat_sum <-
      answer_dat |>
      mutate(
        incl_p = mean(decision_binary == 1, na.rm = TRUE),

        final_decision_gpt_num = dplyr::case_when(
          incl_p < incl_cutoff_upper & incl_p >= incl_cutoff_lower ~ 1,
          incl_p >= incl_cutoff_upper ~ 1,
          incl_p < incl_cutoff_lower ~ 0,
          TRUE ~ NA_real_
        ),

        n_words_answer = stringr::str_count(detailed_description, '\\w+'),

        .by = c(studyid:topp)

      ) |>
      filter(decision_binary == final_decision_gpt_num) |>
      arrange(promptid, model, topp, iterations, studyid, desc(n_words_answer)) |>
      summarise(
        longest_answer = detailed_description[1],

        .by = c(studyid:topp)

      )

    answer_dat_sum <-
      left_join(sum_dat, long_answer_dat_sum) |>
      suppressMessages() |>
      relocate(longest_answer, .after = final_decision_gpt_num) |>
      tibble::new_tibble(class = "chatgpt_tbl")



  } else {

    answer_dat_sum <- tibble::new_tibble(sum_dat, class = "chatgpt_tbl")

  }

#  # Final data all
#  answer_dat <-
#    answer_dat |>
#    select(-req_per_min) |>
#    rename(top_p = topp)


  # Final data sum
  answer_dat_sum <-
    dplyr::left_join(question_dat, answer_dat_sum) |>
    suppressMessages() |>
    select(-c(iterations, req_per_min)) |>
    rename(top_p = topp) |>
    tibble::new_tibble(class = "chatgpt_tbl")


  if (token_info){

    if (n_error > 0) {
      res <- list(
        price_data = price_dat,
        price_dollar = price,
        answer_data_all = answer_dat,
        answer_data_sum = answer_dat_sum,
        error_data = error_refs,
        arguments_used = arg_list
      )
    } else {
      res <- list(
        price_data = price_dat,
        price_dollar = price,
        answer_data_all = answer_dat,
        answer_data_sum = answer_dat_sum,
        arguments_used = arg_list
      )
    }

  } else {

    if (n_error > 0) {
      res <- list(answer_data_all = answer_dat, answer_data_sum = answer_dat_sum, error_data = error_refs, arguments_used = arg_list)
    } else {
      res <- list(answer_data_all = answer_dat, answer_data_sum = answer_dat_sum, arguments_used = arg_list)
    }

  }

  class(res) <- c("list", "chatgpt")

  res

}



#----------------------------------------------------------------
#
#  Functions
#
#----------------------------------------------------------------

# Body functions

inclusion_decision_description <- paste0(
  "If the study should be included for further review, write '1'.",
  "If the study should be excluded, write '0'.",
  "If there is not enough information to make a clear decision, write '1.1'.",
  "If there is no or only a little information in the title and abstract also write '1.1'",
  "When providing the response only provide the numerical decision."
)

detailed_description_description <- paste0(
  "If the study should be included for further reviewing, give a detailed description of your inclusion decision. ",
  "If the study should be excluded from the review, give a detailed description of your exclusion decision. ",
  "If there is not enough information to make a clear decision, give a detailed description of why you can reach a decision. ",
  "If there is no information in the title and abstract, write 'No information'"
)


incl_function <- list(
  # Function 1
  list(
    name = "inclusion_decision",
    description = inclusion_decision_description,
    parameters = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          items = list(
            type = "string",
            description = "A string of either '1', '0', or '1.1'"
          ),
          description = "List the inclusion decision"
        ),
        detailed_description = list(
          type = "string",
          items = list(
            type = "string",
            description = detailed_description_description
          ),
          description = "List the detailed description of your inclusion decision"
        )
      ),
      required = list("decision_gpt", "detailed_description")
    )
  )
)

incl_function_simple <- list(
  # Function 2
  list(
    name = "inclusion_decision_simple",
    description = inclusion_decision_description,
    parameters = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          items = list(
            type = "string",
            description = "A string of either '1', '0', or '1.1'"
          ),
          description = "List the inclusion decision"
        )
      ),
      required = list("decision_gpt")
    )
  )
)










