###############################################################
# Function to send a single request to OpenAI's GPT API models
###############################################################

gpt_engine <-
  function(
    body,
    RPM,
    timeinf,
    tokeninf,
    key,
    max_t,
    max_s,
    is_trans,
    back,
    aft
  ){

    # Logical argument indicating whether detailed screening description is
    # requested
    detailed <- body$tools[[1]]$`function`$name == "inclusion_decision"

    # Indicates how the detailed description variable is handled when the function
    # error and the detailed function is called vs not called.
    detail_desc <- if(detailed) NA_character_ else NULL

    # Max tries and gpt_is_transient not relevant if 'max_t = 0'
    if (max_t == 0) max_t <- is_trans <- NULL

    # Starting time
    tictoc::tic()

    # Request url (might possibly be made more flexible in future applications)
    url <- "https://api.openai.com/v1/chat/completions"

    # Creating the request
    req <-
      httr2::request(url) |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", key) # The key is generated via AIscreenR::get_api_key()
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

    # Check if internet connection is on
    if (curl::has_internet()){

      # Performing the request and getting status code back
      resp <- try(
        suppressMessages(req |> httr2::req_perform()),
        silent = TRUE
      )

      # If succeeded
      if (status_code() == 200){
        # Condition when HTTP response is 200

        # Getting back the response json body (consider try if user experience troubles here)
        resp <- resp |> httr2::resp_body_json()

        # Extracting information from the json body/list
        resp_text <- resp$choices[[1]]$message$tool_calls[[1]]$`function`$arguments
        prompt_tok <- resp$usage$prompt_tokens
        completion_tok <- resp$usage$completion_tokens
        submod <- resp$model

        tib_text <- try(
          tibble::as_tibble(jsonlite::fromJSON(resp_text)),
          silent = TRUE
        )

        # Code what returned data, when JSON format errors appear
        # In some instances the JOSN format returned from the server is flawed
        # This is tried to be handled here.
        if (inherits(tib_text, 'try-error')){
          # Condition if experiences JSON errors

          t_info <- if (timeinf) NA_real_ else NULL
          p_tokens <- c_tokens <- if (tokeninf) NA_real_ else NULL

          res <- tibble::tibble(
            decision_gpt = tib_text[1],
            decision_binary = NA_real_,
            detailed_description = detail_desc,
            prompt_tokens = p_tokens,
            completion_tokens = c_tokens,
            submodel = NA_character_
          )


        } else {
          # Condition when running as expected

          res <-
            tib_text |>
            dplyr::mutate(

              decision_binary = as.numeric(
                dplyr::if_else(stringr::str_detect(decision_gpt, "1"), 1, 0, missing = NA_real_)
              ),

              prompt_tokens = prompt_tok,
              completion_tokens = completion_tok,
              submodel = submod

            )

        }

        if (detailed){
          # Condition constructing data if detailed responses are called
          # This is done to ensure that function works if the function calls for
          # any reason do not work properly (i.e., returns 1 or 0)
          res <-
            res |>
            dplyr::mutate(

              detailed_description = dplyr::if_else(
                is.na(decision_binary), "Error: Something went wrong", detailed_description,
                missing = "Error: Something went wrong"
              )

            ) |>
            dplyr::relocate(detailed_description, .after = decision_binary)
        }


      } else {
        # Condition when HTTP response is NOT 200

        res <- tibble::tibble(
          decision_gpt = error_message(),
          decision_binary = NA_real_,
          detailed_description = detail_desc,
          prompt_tokens = NA_real_,
          completion_tokens = NA_real_,
          submodel = NA_character_
        )

      }

    } else {

      # Condition when not having access to internet

      res <- tibble::tibble(
        decision_gpt = "Error: Could not reach host [check internet connection]",
        decision_binary = NA_real_,
        detailed_description = detail_desc,
        prompt_tokens = NA_real_,
        completion_tokens = NA_real_,
        submodel = NA_character_
      )

    }

    # "Elapsed time since the matching call to tic()"
    time <- tictoc::toc(quiet = TRUE)

    # Adding timing and date info to data
    res <-
      res |>
      dplyr::mutate(
        run_time = round(as.numeric(time$toc - time$tic), 1),
        run_date = as.character(Sys.Date())
      )

    # If the user do not what time and token info, this is removed here
    if (!timeinf) res <- res |> dplyr::select(-run_time)
    if (!tokeninf) res <- res |> dplyr::select(-c(prompt_tokens, completion_tokens))

    # Returning the response result
    res


  }

################################################################
# Function to send repeated requests to OpenAI's GPT API models
################################################################

rep_gpt_engine <- function(
    question, model_gpt, topp, iterations, req_per_min,
    role_gpt,
    tool,
    t_choice,
    seeds,
    time_inf,
    token_inf,
    apikey,
    maxt,
    maxs,
    istrans,
    ba,
    af,
    ...
){

  # Setting tool_choice argument to body

  if (t_choice == "auto"){

    tools_choice = t_choice

  } else {

    tools_choice <- list(
      type = "function",
      "function" = list(
        name = t_choice
      )
    )

  }

  # Creating the body to be passed to gpt_engine()
  # The body is created here, it more readily work with the ... functionality.
  body <- list(
    model = model_gpt,
    messages = list(
      list(
        role = role_gpt,
        content = question
      )
    ),
    tools = tool,
    tool_choice = tools_choice,
    top_p = topp,
    ...
  )

  # Setting the iterations
  if(iterations > 1) iterations <- 1:iterations

  furrr_seed <- if (base::is.null(seeds)) TRUE else NULL

  # Running repeated in requests in parallel, and return tibble
  furrr::future_map_dfr(
    iterations, \(i) gpt_engine(
      body = body,
      RPM = req_per_min,
      timeinf = time_inf,
      tokeninf = token_inf,
      key = apikey,
      max_t = maxt,
      max_s = maxs,
      is_trans = istrans,
      back = ba,
      aft = af
    ),
    .options = furrr::furrr_options(seed = furrr_seed)
  ) |>
    dplyr::mutate(n = iterations) # Given run id to each repetation.


}


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
#' The function draws on the newly developed function calling to better steer the output of the responses called via the
#' tools argument in the request body. This is the main different between this [tabscreen_gpt.tools()]
#' and [tabscreen_gpt.original()]. See [Vembye et al. (2024)](https://osf.io/preprints/osf/yrhzm)
#' for documentation guidance on how to use this screening approach adequately.
#'
#'
#' @references Vembye, M. H., Christensen, J., Mølgaard, A. B., & Schytt, F. L. W. (2024)
#'   \emph{GPT API Models Can Function as Highly Reliable Second Screeners of Titles and Abstracts in Systematic Reviews:
#'   A Proof of Concept and Common Guidelines} \url{https://osf.io/preprints/osf/yrhzm}
#'
#' Wickham H (2023).
#' \emph{httr2: Perform HTTP Requests and Process the Responses}.
#' \url{https://httr2.r-lib.org}, \url{https://github.com/r-lib/httr2}.
#'
#' @template common-arg
#' @param model Character string with the name of the completion model. Can take
#'   multiple models, including gpt-4 models. Default = `"gpt-4"` (i.e., gpt-4-0613). This model has
#'   been shown to outperform the gpt-3.5-turbo models in terms of its ability to detect
#'   relevant studies (Vembye et al., 2024).
#'   Find available model at
#' \url{https://platform.openai.com/docs/models/model-endpoint-compatibility}.
#' @param role Character string indicate the role of the user. Default is `"user"`.
#' @param tools This argument allows this user to customized functions.
#' See \url{https://platform.openai.com/docs/api-reference/chat/create#chat-create-tools}.
#' Default is `NULL`.
#' @param tool_choice If a customized function is provided this argument,
#' 'controls which (if any) tool is called by the model'(OpenAI). Default is `NULL`.
#' If set to `NULL` when using a customized function, then the default is `"auto"`.
#' See \url{https://platform.openai.com/docs/api-reference/chat/create#chat-create-tool_choice}.
#' @param top_p 'An alternative to sampling with temperature, called nucleus sampling,
#'   where the model considers the results of the tokens with top_p probability mass.
#'   So 0.1 means only the tokens comprising the top 10% probability mass are considered.
#'   We generally recommend altering this or temperature but not both.' (OpenAI). Default is 1.
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
#' @param decision_description Logical indicating whether a detailed description should follow
#'   the decision made by GPT. Default is `FALSE`. When conducting large-scale screening,
#'   we generally recommend not using this feature since it will increase the cost of the
#'   screening substantially. We generally recommend using it when encountering disagreements between GPT and
#'   human decisions.
#' @param messages Logical indicating whether to print messages embedded in the function.
#'   Default is `TRUE`.
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
#' @param fine_tuned Logical indicating whether a fine-tuned model is used. Default is `FALSE`.
#' @param ... Further argument to pass to the request body.
#'   See \url{https://platform.openai.com/docs/api-reference/chat/create}.
#'
#' @usage tabscreen_gpt.tools(data, prompt, studyid, title, abstract, model = "gpt-4",
#'    role = "user", tools = NULL, tool_choice = NULL, top_p = 1, time_info = TRUE,
#'    token_info = TRUE, api_key = get_api_key(), max_tries = 16, max_seconds = NULL,
#'    is_transient = gpt_is_transient, backoff = NULL, after = NULL, rpm = 10000,
#'    reps = 1, seed = NULL, progress = TRUE, decision_description = FALSE, messages = TRUE,
#'    incl_cutoff_upper = 0.5, incl_cutoff_lower = incl_cutoff_upper - 0.1, force = FALSE,
#'    fine_tuned = FALSE, ...)
#'
#'tabscreen_gpt(data, prompt, studyid, title, abstract, model = "gpt-4",
#'    role = "user", tools = NULL, tool_choice = NULL, top_p = 1, time_info = TRUE,
#'    token_info = TRUE, api_key = get_api_key(), max_tries = 16, max_seconds = NULL,
#'    is_transient = gpt_is_transient, backoff = NULL, after = NULL, rpm = 10000,
#'    reps = 1, seed = NULL, progress = TRUE, decision_description = FALSE, messages = TRUE,
#'    incl_cutoff_upper = 0.5, incl_cutoff_lower = incl_cutoff_upper - 0.1, force = FALSE,
#'    fine_tuned = FALSE, ...)
#'
#' @return An object of class `'gpt'`. The object is a list containing the following
#' datasets and components:
#' \item{answer_data_final}{dataset of class `'gpt_tbl'` with the summarized, probabilistic inclusion decision
#' for each title and abstract across multiple repeated questions.}
#' \item{answer_data_all}{dataset of class `'gpt_tbl'` with all individual answers.}
#' \item{price_dollar}{numerical value indicating the total price (in USD) of the screening.}
#' \item{price_data}{dataset with prices across all gpt models used for screening.}
#' \item{arguments_used}{list with arguments used. These are past to the [screen_errors()] when used to
#'  catch transient errors.}
#' \item{run_date}{string indicating the date when the screening where ran.}
#'
#' @note The \code{answer_data_final} data contains the following *mandatory* variables:
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
#' The \code{answer_data_all} data contains the following *mandatory* variables:
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
#'  \bold{submodel} \tab \code{character} \tab indicating the exact (sub)model used for screening. \cr
#'  \bold{run_time}  \tab \code{numeric} \tab indicating the time it took to obtain a response from the server for the given request. \cr
#'  \bold{run_date}  \tab \code{character} \tab indicating the date the given response was received. \cr
#'  \bold{n} \tab \code{integer} \tab indicating request ID.  \cr
#' }
#' <br>
#' If any requests failed to reach the server, the `gpt` object contains an
#' error data set (`error_data`) having the same variables as `answer_data_all`
#' but with failed request references only.
#'
#' <br>
#'
#' The \code{price_data} data contains the following variables:
#' \tabular{lll}{
#'  \bold{prompt} \tab \code{character} \tab if multiple prompts are used this variable indicates the given prompt-id. \cr
#'  \bold{model} \tab \code{character} \tab gpt model. \cr
#'  \bold{iterations} \tab \code{integer} \tab indicating the number of times the same question is requested.  \cr
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
#' tabscreen_gpt(
#'   data = filges2015_dat[1:2,],
#'   prompt = prompt,
#'   studyid = studyid,
#'   title = title,
#'   abstract = abstract
#'   )
#' plan(sequential)
#'
#'  # Get detailed descriptions of the gpt decisions by using the
#'  # embedded function calling functions from the package. See example below.
#'
#'  plan(multisession)
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
# inspired by the rma.uni and rma functions are made in metafor instead of making an S3 object
tabscreen_gpt <- tabscreen_gpt.tools <- function(
  data,
  prompt,
  studyid,
  title,
  abstract,
  model = "gpt-4",
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
  seed = NULL,
  progress = TRUE,
  decision_description = FALSE,
  messages = TRUE,
  incl_cutoff_upper = 0.5,
  incl_cutoff_lower = incl_cutoff_upper - 0.1,
  force = FALSE,
  fine_tuned = FALSE,
  ...
){

  ###############################################
  # Start up - Generic stop messages
  ###############################################

  # Ensuring that the same model is not called twice by the user
  if (n_distinct(reps) == 1 && n_distinct(model) != length(model)){
    model <- unique(model)
  }

  # Stop if wrong model are called
  if (!fine_tuned){
    if(!model %in% model_prizes$model) stop("Unknown gpt model(s) used - check model name(s).")
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
      dplyr::filter(!stringr::str_detect(model, "mini"))

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


  # Ensuring correct use of the incl_cutoff_*()
  if(incl_cutoff_upper < incl_cutoff_lower){
    stop("incl_cutoff_lower must not exceed incl_cutoff_upper")
  }

  # Avoiding that equivalent prompts are added to function
  if (!missing(prompt)){
    if (n_distinct(prompt) != length(prompt)) stop("Do not add the same prompt twice.")
  }

  ###############################################
  # Start up - Generic warning messages
  ###############################################

  if (missing(title) || missing(abstract) && !is.gpt(data)){
    warning("The function only works properly when given both titles and abstracts.")
  }

  ###############################################
  # Collecting all arguments to be used in the screen_errors function
  ###############################################

  # List tracking the used arguments
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
      seed = seed,
      progress = progress,
      messages = messages,
      decision_description = decision_description,
      incl_cutoff_upper = incl_cutoff_upper,
      incl_cutoff_lower = incl_cutoff_lower,
      ...
    )


  # Assert that max tokens is not below nine when used with the simple function call.
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


  ###############################################
  # Function call setup
  ###############################################


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


  ###############################################
  # Data manipulation
  ###############################################

  # Ensure that the data contained valid study IDs to distinguish between study records
    study_id <- if (missing(studyid)) 1:nrow(data) else data |> pull({{ studyid }})

    dat <-
      data |>
      dplyr::mutate(
        studyid = study_id
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

    # Creating the question data that will later be passed to the rep_gpt_engine()
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

    ###############################################
    # Approximate price calculation
    ###############################################

    # Calculating the approximate price using the helper function price_gpt()
    app_price_dat <- price_gpt(question_dat)
    app_price <- sum(app_price_dat$total_price_dollar, na.rm = TRUE)

    # Ensuring the user does not waste money on wrong coding
    if (app_price > 15 & !force){
      stop(paste0(
        "Are you sure you want to run this screening? It will cost approximately $", app_price, ".",
        " If so, set 'force = TRUE')"))
    }
    ###############################################
    # Startup messages
    ###############################################

    if (messages){

      message(paste0("* The approximate price of the current (simple) screening will be around $", app_price, "."))

      if (decision_description){
        message(
          paste0(
            "* Be aware that getting descriptive, detailed reponses will substantially increase",
            " the prize of the screening relative to the noted approximate prize."
          )
        )
      }


      abstract_text <- question_dat |> pull({{ abstract }}) |> unique()

      if ("No information" %in% abstract_text) {
        message(
          paste0(
            "* Consider removing references that has no abstract ",
            "since these can distort the accuracy of the screening."
          )
        )
      }
    }

    ###############################################
    # RUNNING QUESTIONS - the heart of the function
    ###############################################

    furrr_seed <- if (is.null(seed)) TRUE else NULL

    params <- question_dat |>
      dplyr::select(question, model_gpt = model, topp, iterations, req_per_min)


    answer_dat <-
      question_dat |>
      dplyr::mutate(
        res = furrr::future_pmap(
          .l = params,
          .f = rep_gpt_engine,
          role_gpt = role,
          tool = tools,
          t_choice = tool_choice,
          seeds = seed,
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


    ###############################################
    # Catching errors
    ###############################################

    n_error <- answer_dat |> dplyr::filter(is.na(decision_binary)) |> nrow()

    if (messages){
      if (n_error == 1) message(paste("* NOTE: Requests failed for 1 title and abstract."))
      if (n_error > 1) message(paste("* NOTE: Requests failed", n_error, "times."))
    }

    if (n_error > 0) error_refs <- answer_dat |> dplyr::filter(is.na(decision_binary))


    ###############################################
    # Final price information
    ###############################################

    if (token_info){

      price_dat <- price_gpt(answer_dat)
      price <- sum(price_dat$total_price_dollar, na.rm = TRUE)

    }

    ###############################################
    # Making final/sum data - of primary importance when multiple iterations are used
    ###############################################

    sum_dat <-
      answer_dat |>
      dplyr::summarise(

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

        reps = dplyr::n(),

        n_mis_answers = sum(is.na(decision_binary)),

        submodel = unique(submodel),

        .by = c(studyid:topp)

      )

    if ("detailed_description" %in% names(answer_dat)){


      long_answer_dat_sum <-
        answer_dat |>
        dplyr::mutate(
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
        dplyr::filter(decision_binary == final_decision_gpt_num) |>
        dplyr::arrange(promptid, model, topp, iterations, studyid, desc(n_words_answer)) |>
        dplyr::summarise(

          longest_answer = detailed_description[1],
          .by = c(studyid:topp)

        )

      answer_dat_sum <-
        left_join(sum_dat, long_answer_dat_sum) |>
        suppressMessages() |>
        dplyr::relocate(longest_answer, .after = final_decision_gpt_num) |>
        tibble::new_tibble(class = "gpt_tbl")



    } else {

      answer_dat_sum <- tibble::new_tibble(sum_dat, class = "gpt_tbl")

    }


    # Final data sum
    answer_dat_final <-
      dplyr::left_join(question_dat, answer_dat_sum) |>
      suppressMessages() |>
      dplyr::select(-c(iterations, req_per_min)) |>
      dplyr::rename(top_p = topp) |>
      tibble::new_tibble(class = "gpt_tbl")


    if (token_info){

      if (n_error > 0) {
        res <- list(
          price_data = price_dat,
          price_dollar = price,
          answer_data_all = answer_dat,
          answer_data_final = answer_dat_final,
          error_data = error_refs,
          arguments_used = arg_list,
          run_date = Sys.Date()
        )
      } else {
        res <- list(
          price_data = price_dat,
          price_dollar = price,
          answer_data_all = answer_dat,
          answer_data_final = answer_dat_final,
          arguments_used = arg_list,
          run_date = Sys.Date()
        )
      }

    } else {

      if (n_error > 0) {
        res <- list(
          answer_data_all = answer_dat,
          answer_data_final = answer_dat_final,
          error_data = error_refs,
          arguments_used = arg_list,
          run_date = Sys.Date()
        )
      } else {
        res <- list(
          answer_data_all = answer_dat,
          answer_data_final = answer_dat_final,
          arguments_used = arg_list,
          run_date = Sys.Date()
        )
      }

    }

    class(res) <- c("list", "gpt")

    res

}
