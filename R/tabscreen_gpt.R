#' @title Title and abstract screening with GPT API models using function calls via the tools argument
#'
#' @name tabscreen_gpt.tools
#' @aliases tabscreen_gpt.tools tabscreen_gpt
#'
#' @description
#' `r lifecycle::badge("stable")`<br>
#' <br>
#' This function supports the conduct of title and abstract screening with GPT API models in R.
#' Specifically, it allows the user to draw on GPT-3.5, GPT-4, GPT-4o, GPT-4o-mini, and fine-tuned models.
#' The function allows to run title and abstract screening across multiple prompts and with
#' repeated questions to check for consistency across answers. All of which can be done in parallel.
#' The function draws on the newly developed function calling which is called via the
#' tools argument in the request body. This is the main different between [tabscreen_gpt.tools()]
#' and [tabscreen_gpt.original()]. Function calls ensure more reliable and consistent responses to ones
#' requests. See [Vembye, Christensen, Mølgaard, and Schytt. (2025)](https://osf.io/preprints/osf/yrhzm)
#' for guidance on how adequately to conduct title and abstract screening with GPT models.
#'
#' @references Vembye, M. H., Christensen, J., Mølgaard, A. B., & Schytt, F. L. W. (2025)
#'   \emph{GPT API Models Can Function as Highly Reliable Second Screeners of Titles and Abstracts in Systematic Reviews:
#'   A Proof of Concept and Common Guidelines} \url{https://osf.io/preprints/osf/yrhzm}
#'
#'   Thomas, J. et al. (2024).
#'   Responsible AI in Evidence SynthEsis (RAISE): guidance and recommendations.
#'   \url{https://osf.io/cn7x4}
#'
#' Wickham H (2023).
#' \emph{httr2: Perform HTTP Requests and Process the Responses}.
#' \url{https://httr2.r-lib.org}, \url{https://github.com/r-lib/httr2}.
#'
#' @template common-arg
#' @param model Character string with the name of the completion model. Can take
#'   multiple models. Default is the latest `"gpt-4o-mini"`.
#'   Find available model at
#' \url{https://platform.openai.com/docs/models/model-endpoint-compatibility}.
#' @param role Character string indicating the role of the user. Default is `"user"`.
#' @param tools This argument allows this user to apply customized functions.
#' See \url{https://platform.openai.com/docs/api-reference/chat/create#chat-create-tools}.
#' Default is `NULL`. If not specified the default function calls from `AIscreenR` are used.
#' @param tool_choice If a customized function is provided this argument
#' 'controls which (if any) tool is called by the model' (OpenAI). Default is `NULL`.
#' If set to `NULL` when using a customized function, the default is `"auto"`.
#' See \url{https://platform.openai.com/docs/api-reference/chat/create#chat-create-tool_choice}.
#' @param top_p 'An alternative to sampling with temperature, called nucleus sampling,
#'   where the model considers the results of the tokens with top_p probability mass.
#'   So 0.1 means only the tokens comprising the top 10% probability mass are considered.
#'   We generally recommend altering this or temperature but not both.' (OpenAI). Default is 1.
#'   Find documentation at
#' \url{https://platform.openai.com/docs/api-reference/chat/create#chat/create-top_p}.
#' @param time_info Logical indicating whether the run time of each
#'   request/question should be included in the data. Default is `TRUE`.
#' @param token_info Logical indicating whether token information should be included
#'   in the output data. Default is `TRUE`. When `TRUE`, the output object will
#'   include price information of the conducted screening.
#' @template api-key-arg
#' @param max_tries,max_seconds 'Cap the maximum number of attempts with
#'  `max_tries` or the total elapsed time from the first request with
#'  `max_seconds`. If neither option is supplied (the default), [httr2::req_perform()]
#'  will not retry' (Wickham, 2023). The default of `max_tries` is 16.
#' @param is_transient 'A predicate function that takes a single argument
#'  (the response) and returns `TRUE` or `FALSE` specifying whether or not
#'  the response represents a transient error' (Wickham, 2023). This function runs
#'  automatically in the AIscreenR but can be customized by the user if necessary.
#' @param backoff 'A function that takes a single argument (the number of failed
#'   attempts so far) and returns the number of seconds to wait' (Wickham, 2023).
#' @param after 'A function that takes a single argument (the response) and
#'   returns either a number of seconds to wait or `NULL`, which indicates
#'   that a precise wait time is not available that the `backoff` strategy
#'   should be used instead' (Wickham, 2023).
#' @param rpm Numerical value indicating the number of requests per minute (rpm)
#'   available for the specified model. Find more information at
#'   \url{https://platform.openai.com/docs/guides/rate-limits/what-are-the-rate-limits-for-our-api}.
#'   Alternatively, use [rate_limits_per_minute()].
#' @param reps Numerical value indicating the number of times the same
#'   question should be send to the server. This can be useful to test consistency
#'   between answers, and/or can be used to make inclusion judgments based on how many times
#'   a study has been included across a the given number of screenings.
#'   Default is `1` but when using gpt-3.5-turbo models or gpt-4o-mini,
#'   we recommend setting this value to `10` to catch model uncertainty.
#' @param seed_par Numerical value for a seed to ensure that proper,
#'   parallel-safe random numbers are produced.
#' @param progress Logical indicating whether a progress line should be shown when running
#'   the title and abstract screening in parallel. Default is `TRUE`.
#' @param decision_description Logical indicating whether a detailed description should follow
#'   the decision made by GPT. Default is `FALSE`. When conducting large-scale screening,
#'   we generally recommend not using this feature as it will substantially increase the cost of the
#'   screening. We generally recommend using it when encountering disagreements between GPT and
#'   human decisions.
#' @param messages Logical indicating whether to print messages embedded in the function.
#'   Default is `TRUE`.
#' @param incl_cutoff_upper Numerical value indicating the probability threshold
#'   for which a studies should be included. ONLY relevant when the same questions is requested
#'   multiple times (i.e., when any reps > 1). Default is 0.5, indicating that
#'   titles and abstracts should only be included if GPT has included the study more than 50 percent of the times.
#' @param incl_cutoff_lower Numerical value indicating the probability threshold
#'   above which studies should be check by a human. ONLY relevant when the same questions is requested
#'   multiple times (i.e., when any reps > 1). Default is 0.4, meaning
#'   that if you ask GPT the same questions 10 times and it includes the
#'   title and abstract 4 times, we suggest that the study should be check by a human.
#' @param force Logical argument indicating whether to force the function to use more than
#'   10 iterations for gpt-3.5 models and more than 1 iteration for gpt-4 models other than gpt-4o-mini.
#'   This argument is developed to avoid the conduct of wrong and extreme sized screening.
#'   Default is `FALSE`.
#' @param fine_tuned Logical indicating whether a fine-tuned model is used. Default is `FALSE`.
#' @param ... Further argument to pass to the request body.
#'   See \url{https://platform.openai.com/docs/api-reference/chat/create}.
#'
#' @usage tabscreen_gpt.tools(data, prompt, studyid, title, abstract,
#'    model = "gpt-4o-mini", role = "user", tools = NULL, tool_choice = NULL, top_p = 1,
#'    time_info = TRUE, token_info = TRUE, api_key = get_api_key(), max_tries = 16,
#'    max_seconds = NULL, is_transient = gpt_is_transient, backoff = NULL,
#'    after = NULL, rpm = 10000, reps = 1, seed_par = NULL, progress = TRUE,
#'    decision_description = FALSE, messages = TRUE, incl_cutoff_upper = NULL,
#'    incl_cutoff_lower = NULL, force = FALSE, fine_tuned = FALSE, ...)
#'
#'tabscreen_gpt(data, prompt, studyid, title, abstract,
#'    model = "gpt-4o-mini", role = "user", tools = NULL, tool_choice = NULL, top_p = 1,
#'    time_info = TRUE, token_info = TRUE, api_key = get_api_key(), max_tries = 16,
#'    max_seconds = NULL, is_transient = gpt_is_transient, backoff = NULL,
#'    after = NULL, rpm = 10000, reps = 1, seed_par = NULL, progress = TRUE,
#'    decision_description = FALSE, messages = TRUE, incl_cutoff_upper = NULL,
#'    incl_cutoff_lower = NULL, force = FALSE, fine_tuned = FALSE, ...)
#'
#' @return An object of class `'gpt'`. The object is a list containing the following
#' datasets and components:
#' \item{answer_data}{dataset of class `'gpt_tbl'` with all individual answers.}
#' \item{price_dollar}{numerical value indicating the total price (in USD) of the screening.}
#' \item{price_data}{dataset with prices across all gpt models used for screening.}
#' \item{run_date}{string indicating the date when the screening was ran. In some frameworks,
#'    time details are considered important to report (see e.g., Thomas et al., 2024).}
#' \item{...}{some additional attributed values/components, including an attributed list with the arguments used in the function.
#'  These are used in  \code{\link[=screen_errors]{screen_errors()}} to re-screen transient errors.}
#'
#' If the same question is requested multiple times, the object will also contain the
#' following dataset with results aggregated across the iterated requests/questions.
#'
#' \item{answer_data_aggregated}{dataset of class `'gpt_agg_tbl'` with the summarized, probabilistic inclusion decision
#' for each title and abstract across multiple repeated questions.}
#'
#' @note The \code{answer_data} data contains the following *mandatory* variables:
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
#'  ONLY included if the detailed function calling function is used. See 'Examples' below for how to use this function. \cr
#'  \bold{decision_binary}  \tab \code{integer} \tab indicating the binary gpt decision,
#'  that is 1 for inclusion and 0 for exclusion. 1.1 decision are coded equal to 1 in this case. \cr
#'  \bold{prompt_tokens}  \tab \code{integer} \tab indicating the number of prompt tokens sent to the server for the given request. \cr
#'  \bold{completion_tokens}  \tab \code{integer} \tab indicating the number of completion tokens sent to the server for the given request. \cr
#'  \bold{submodel} \tab \code{character} \tab indicating the exact (sub)model used for screening. \cr
#'  \bold{run_time}  \tab \code{numeric} \tab indicating the time it took to obtain a response from the server for the given request. \cr
#'  \bold{run_date}  \tab \code{character} \tab indicating the date the given response was received. \cr
#'  \bold{n} \tab \code{integer} \tab indicating iteration ID. Is only different from 1, when `reps > 1`.  \cr
#' }
#' <br>
#' If any requests failed, the `gpt` object contains an
#' error dataset (`error_data`) containing the same variables as `answer_data`
#' but with failed request references only.
#'
#' <br>
#'
#' When the same question is requested multiple times, the \code{answer_data_aggregated} data contains the following *mandatory* variables:
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
#'  across multiple repeated responses on the same title and abstract. Only included when `decision_description = TRUE`.
#'  See 'Examples' below for how to use this function. \cr
#'  \bold{reps}  \tab \code{integer}  \tab indicating the number of times the same question has been sent to OpenAI's GPT API models. \cr
#'  \bold{n_mis_answers} \tab \code{integer} \tab indicating the number of missing responses. \cr
#'  \bold{submodel} \tab \code{character} \tab indicating the exact (sub)model used for screening. \cr
#' }
#' <br>
#'
#' The \code{price_data} data contains the following variables:
#' \tabular{lll}{
#'  \bold{prompt} \tab \code{character} \tab if multiple prompts are used this variable indicates the given prompt-id. \cr
#'  \bold{model} \tab \code{character} \tab the specific gpt model used. \cr
#'  \bold{iterations} \tab \code{integer} \tab indicating the number of times the same question was requested.  \cr
#'  \bold{input_price_dollar} \tab \code{integer} \tab price for all prompt/input tokens for the correspondent gpt-model. \cr
#'  \bold{output_price_dollar}  \tab \code{integer} \tab price for all completion/output tokens for the correspondent gpt-model. \cr
#'  \bold{total_price_dollar} \tab \code{integer} \tab total price for all tokens for the correspondent gpt-model. \cr
#' }
#'
#' Find current token pricing at \url{https://openai.com/pricing} or [model_prizes].
#'
#' @importFrom stats df
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(future)
#'
#' set_api_key()
#'
#' prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"
#'
#' plan(multisession)
#'
#' tabscreen_gpt(
#'   data = filges2015_dat[1:2,],
#'   prompt = prompt,
#'   studyid = studyid,
#'   title = title,
#'   abstract = abstract
#'   )
#'
#' plan(sequential)
#'
#'  # Get detailed descriptions of the gpt decisions.
#'
#'  plan(multisession)
#'
#'  tabscreen_gpt(
#'    data = filges2015_dat[1:2,],
#'    prompt = prompt,
#'    studyid = studyid,
#'    title = title,
#'    abstract = abstract,
#'    decision_description = TRUE
#'  )
#'
#' plan(sequential)
#'
#'}

# Main function
tabscreen_gpt <- tabscreen_gpt.tools <- function(
  data,
  prompt,
  studyid,
  title,
  abstract,
  model = "gpt-4o-mini",
  role = "user",
  tools = NULL,
  tool_choice = NULL,
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
  seed_par = NULL,
  progress = TRUE,
  decision_description = FALSE,
  messages = TRUE,
  incl_cutoff_upper = NULL,
  incl_cutoff_lower = NULL,
  force = FALSE,
  fine_tuned = FALSE,
  ...
){

  # Handling inherits
  if (is_gpt_tbl(data)) data <- data |> dplyr::select(-c(promptid:n)) |> tibble::as_tibble()
  if (is_gpt_agg_tbl(data)) data <- data |> dplyr::select(-c(promptid:submodel)) |> tibble::as_tibble()

  #......................................
  # Start up - Generic stop messages ----
  #......................................

  # Ensuring that the same model is not called twice by the user
  if (n_distinct(reps) == 1 && n_distinct(model) != length(model)){
    model <- unique(model)
  }

  # Stop if wrong models are called
  if (!fine_tuned){
    if(any(!model %in% model_prizes$model)) stop("Unknown gpt model(s) used - check model name(s).")
  }

  # Ensuring that users do not conduct wrong screening
  if (max(reps) > 10 && !force){
    max_reps_message <- paste("* Are you sure you want to use", max(reps), "iterations? If so, set 'force = TRUE'")
    stop(max_reps_message)
  }

  # Check if the user want to use gpt-4 model with iterations
  if (any(stringr::str_detect(model, "gpt-4")) && max(reps) > 1 && !force){

    gpt4_dat <-
      tibble::tibble(model, reps) |>
      dplyr::filter(!stringr::str_detect(model, "mini|nano"))

      if(nrow(gpt4_dat) > 0){

        gpt4_reps <-
          gpt4_dat |>
          dplyr::filter(stringr::str_detect(model, "gpt-4")) |>
          dplyr::pull(reps) |>
          max()

        if (gpt4_reps > 1){

          max_reps_mes_gpt4 <-
            paste("* Are you sure you want to use", gpt4_reps, "iterations with a gpt-4 model?",
                  "If so, set force = TRUE.")
          stop(max_reps_mes_gpt4)

        }

      }


  }

  # Ensuring that the rpm argument fits to the corresponding model
  if (length(rpm) > 1 && length(model) != length(rpm)){
    stop("model and rpm must be of the same length.")
  }

  # Ensuring that the reps argument fits to the corresponding model
  if (length(reps) > 1 && length(model) != length(reps)){
    stop("model and reps must be of the same length.")
  }


  # Avoiding that equivalent prompts are added to function
  if (!missing(prompt)){
    if (n_distinct(prompt) != length(prompt)) stop("Do not add the same prompt twice.")
  }

  # Ensuring proper use of the incl_cutoff_upper and incl_cutoff_lower arguments
  if (any(reps > 1)) {

    ### Handling the incl_cutoff_upper and incl_cutoff_lower arguments
    # Stop
    if (is.null(incl_cutoff_upper) && !is.null(incl_cutoff_lower)){
      stop("You must specify the incl_cutoff_upper argument.")
    }

    # Ensuring correct use of the incl_cutoff_*()
    if (is.numeric(incl_cutoff_upper) && is.numeric(incl_cutoff_lower) && incl_cutoff_upper < incl_cutoff_lower){
      stop("incl_cutoff_lower must not exceed incl_cutoff_upper")
    }

    if (!is.null(incl_cutoff_upper)){

      if (!is.numeric(incl_cutoff_upper)) stop("incl_cutoff_upper must be a numeric value between 0 and 1.")
      if (incl_cutoff_upper < 0 || incl_cutoff_upper > 1) stop("incl_cutoff_upper only takes values between 0 and 1")

    }

    if (!is.null(incl_cutoff_lower)){

      if (!is.numeric(incl_cutoff_lower)) stop("incl_cutoff_lower must be a numeric value between 0 and 1.")
      if (incl_cutoff_lower < 0 || incl_cutoff_lower > 1) stop("incl_cutoff_lower only takes values between 0 and 1")

    }

  }

  #.........................................
  # Start up - Generic warning messages ----
  #.........................................

  if (missing(title) || missing(abstract) && !is.gpt(data)){
    warning("The function only works properly when given both titles and abstracts.")
  }

  #.......................................................................
  # Collecting all arguments to be used in the screen_errors function ----
  #.......................................................................

  # List tracking the used arguments, i.e., all that are not a track in the
  # return result dataset.
  arg_list <-
    list(
      role = role,
      tools = tools,
      tool_choice = tool_choice,
      time_info = time_info,
      token_info = token_info,
      max_tries = max_tries,
      max_seconds = max_seconds,
      is_transient = is_transient,
      backoff = backoff,
      after = after,
      reps = reps,
      seed_par = seed_par,
      progress = progress,
      messages = messages,
      decision_description = decision_description,
      incl_cutoff_upper = incl_cutoff_upper,
      incl_cutoff_lower = incl_cutoff_lower,
      force = force,
      fine_tuned = fine_tuned,
      ...
    )


  # Assert that max tokens is not below nine when used with the simple function call
  if ("max_completion_tokens" %in% names(arg_list) || "max_tokens" %in% names(arg_list)){
    if (arg_list$max_completion_tokens < 9) {
      stop("Cannot retrieve results from server with tokens below 9.")
    }
  }

  if(decision_description){
    if ("max_completion_tokens" %in% names(arg_list) || "max_tokens" %in% names(arg_list)){
      stop("the max_completion_tokens argument does not work with descriptive screening.")
    }
  }

  # Default values for incl_cutoff_upper and incl_cutoff_lower when 'reps > 1'
  if (any(reps > 1)) {
    if(is.numeric(incl_cutoff_upper) && is.null(incl_cutoff_lower)) incl_cutoff_lower <- incl_cutoff_upper

    if (is.null(incl_cutoff_upper)) incl_cutoff_upper <- 0.5
    if (is.null(incl_cutoff_lower)) incl_cutoff_lower <- incl_cutoff_upper - 0.1
  }

  #.........................
  # Function call setup ----
  #.........................

  # If the users want to add own function call to the function
  # Some further stop messages
  if (!is.null(tools) && !is.list(tools)) stop("The tools function must be of a list.")
  if (is.null(tools) && !is.null(tool_choice)) stop("You must provide a tool or set 'tool_choice = NULL'.")

  # Setting auto if tool_choice is not provided
  if (!is.null(tools) && is.null(tool_choice)) tool_choice <- "auto"

  # Default setting
  if (is.null(tools) && is.null(tool_choice)){

    if (!decision_description){

      tools <- tools_simple
      tool_choice <- "inclusion_decision_simple"

    } else {

      tools <- tools_detailed
      tool_choice <- "inclusion_decision"

    }

  }

  #.......................
  # Data manipulation ----
  #.......................

  # Ensure that the data contains valid study IDs to distinguish between study records
    study_id <- if (missing(studyid)) 1:nrow(data) else data |> pull({{ studyid }})

    dat <-
      data |>
      dplyr::mutate(
        studyid = study_id,
        studyid = factor(studyid, levels = unique(studyid))
      ) |>
      dplyr::relocate(studyid, .before = {{ title }}) |>
      dplyr::relocate({{ abstract }}, .after = {{ title }}) |>
      dplyr::relocate(c(studyid, {{ title }}, {{ abstract }}), .after = last_col())


    # Factors used for slicing data and ensuring correct length of data
    mp_reps <- if (length(reps) > 1) 1 else length(model)
    mp_rpm <- if (length(rpm) > 1) 1 else length(model)

    model_length <- length(model)
    prompt_length <- length(prompt)
    studyid_length <- dplyr::n_distinct(dat$studyid)

    # Creating the question data that will later be passed to the .rep_gpt_engine()
    question_dat <-
      dat |>
      dplyr::mutate(
        # Trying to catch empthy title and abstracts without any text
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
        # removing line shift symbols and creating the main question
        question = stringr::str_replace_all(question_raw, "\n\n", " "),
        question = stringr::str_remove_all(question, "\n")
      ) |>
      dplyr::select(-question_raw) |>
      dplyr::slice(rep(1:dplyr::n(), each = length(top_p))) |>
      dplyr::mutate(
        topp = rep(top_p, studyid_length*prompt_length*model_length)
      ) |>
      dplyr::arrange(promptid, model, topp, iterations, studyid)

    #...................................
    # Approximate price calculation ----
    #...................................

    # Calculating the approximate price using the helper function price_gpt()

    if(!fine_tuned) {

    app_price_dat <- .price_gpt(question_dat)
    app_price <- sum(app_price_dat$total_price_dollar, na.rm = TRUE)

    # Ensuring the user does not waste money on wrong coding
    if (app_price > 15 & !force){
      stop(paste0(
        "Are you sure you want to run this screening? It will cost approximately $", app_price, ".",
        " If so, set 'force = TRUE')"))
    }

    #.......................
    # Startup messages ----
    #.......................

    if (messages){
      message(paste0("* The approximate price of the current (simple) screening will be around $", app_price, "."))
    }

    } else {

      app_price_dat <- NULL
      app_price <- NULL

      if (messages){
        message(paste0("* Cannot approximate the price of the screening since one or more non-standard models are used."))
      }
    }


    if (decision_description){
      message(
        paste0(
          "* Be aware that getting descriptive, detailed responses will substantially increase",
          " the prize of the screening relative to the noted approximate prize."
        )
      )
    }

    abstract_text <- question_dat |> pull({{ abstract }}) |> unique()

    if ("No information" %in% abstract_text) {
      message(
        paste0(
          "* Consider removing references without abstracts ",
          "since these can distort the accuracy of the screening."
        )
      )
    }


    #...................................................
    # RUNNING QUESTIONS - the heart of the function ----
    #...................................................

    furrr_seed <- if (is.null(seed_par)) TRUE else NULL

    params <- question_dat |>
      dplyr::select(question, model_gpt = model, topp, iterations, req_per_min)


    answer_dat <-
      question_dat |>
      dplyr::mutate(
        res = furrr::future_pmap(
          .l = params,
          .f = .rep_gpt_engine,
          role_gpt = role,
          tool = tools,
          t_choice = tool_choice,
          seeds = seed_par,
          time_inf = time_info,
          token_inf = token_info,
          apikey = api_key,
          maxt = max_tries,
          maxs = max_seconds,
          istrans = is_transient,
          ba = backoff,
          af = after,
          ...,
          .options = furrr::furrr_options(seed = furrr_seed),
          .progress = progress
        )
      ) |>
      tidyr::unnest(res) |>
      tibble::new_tibble(class = "gpt_tbl")


    #.....................
    # Catching errors ----
    #.....................

    n_error <- answer_dat |> dplyr::filter(is.na(decision_binary)) |> nrow()

    if (messages){
      if (n_error == 1) message(paste("* NOTE: Requests failed for 1 title and abstract."))
      if (n_error > 1) message(paste("* NOTE: Requests failed", n_error, "times."))
    }

    # Adding error data
    error_dat <- if (n_error > 0) answer_dat |> dplyr::filter(is.na(decision_binary)) else NULL

    #.............................
    # Final price information ----
    #.............................

    # Adding price data
    price_dat <- if (token_info && !fine_tuned) .price_gpt(answer_dat) else NULL
    price <- if (!is.null(price_dat)) sum(price_dat$total_price_dollar, na.rm = TRUE) else NULL

    #.........................................................................
    # Making aggregated data ----
    # Of primary importance when multiple iterations are used, i.e. reps > 1
    #.........................................................................

    # Adding the aggregated data
    if (any(reps > 1)) {

      # Get the aggregated
      answer_dat_sum <-
        .aggregate_res(
          answer_dat,
          incl_cutoff_u = incl_cutoff_upper,
          incl_cutoff_l = incl_cutoff_lower
        )

      # Final data sum
      answer_dat_aggregated <-
        dplyr::left_join(question_dat, answer_dat_sum) |>
        suppressMessages() |>
        dplyr::select(-c(iterations, req_per_min)) |>
        dplyr::rename(top_p = topp) |>
        tibble::new_tibble(class = "gpt_agg_tbl")

      attr(answer_dat_aggregated, "incl_cutoff_upper") <- incl_cutoff_upper
      attr(answer_dat_aggregated, "incl_cutoff_lower") <- incl_cutoff_lower

    } else {

      answer_dat_aggregated <- NULL

    }

    #.........................................
    # Returned output
    #.........................................

    res <- list(
      price_data = price_dat,
      price_dollar = price,
      answer_data = answer_dat,
      answer_data_aggregated = answer_dat_aggregated,
      error_data = error_dat,
      run_date = Sys.Date()
    )

    # If token info is not wanted or fine tuned model used
    if (fine_tuned || !token_info) res[["price_data"]] <- res[["price_dollar"]] <- NULL

    # If no screening errors
    if (n_error == 0) res[["error_data"]] <- NULL

    # Returned output without aggregated results
    if (all(reps == 1)) res[["answer_data_aggregated"]] <- NULL

    # Attributing used arguments to res. Used in screen_errors()
    attr(res, "arg_list") <- arg_list

    # Defining the class of the res object
    class(res) <- c("gpt", class(res))

    res

}
