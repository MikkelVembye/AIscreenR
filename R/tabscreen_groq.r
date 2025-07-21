#' Title and abstract screening with Groq API models using function-like behavior via system messages
#'
#' @description
#' This function supports the conduct of title and abstract screening with Groq API models in R.
#' The function allows you to run title and abstract screening across multiple prompts and with
#' repeated questions to check for consistency across answers. This function implements function-like
#' behavior through system messages since Groq models don't natively support OpenAI's function calling.
#'
#' @references Vembye, M. H., Christensen, J., Mølgaard, A. B., & Schytt, F. L. W. (2024)
#'   \emph{GPT API Models Can Function as Highly Reliable Second Screeners of Titles and Abstracts in Systematic Reviews:
#'   A Proof of Concept and Common Guidelines} \url{https://osf.io/preprints/osf/yrhzm}
#'
#' @template common-arg
#' @param ... Further argument to pass to the request body.
#' @param model Character string with the name of the completion model. Can take
#'   multiple Groq models. Default = `"llama3-70b-8192"`.
#'   Find available models at \url{https://console.groq.com/docs/models}.
#' @param role Character string indicate the role of the user. Default is `"user"`.
#' @param tools List of function definitions for tool calling. Default behavior is set based on `decision_description` parameter.
#'   For detailed responses, the function uses tools that include detailed description capabilities.
#' @param tool_choice Specification for which tool to use. Default behavior is set based on `decision_description` parameter.
#'   For simple responses uses "inclusion_decision_simple", for detailed responses uses "inclusion_decision".
#' @param top_p 'An alternative to sampling with temperature, called nucleus sampling,
#'   where the model considers the results of the tokens with top_p probability mass.
#'   So 0.1 means only the tokens comprising the top 10% probability mass are considered.
#'   We generally recommend altering this or temperature but not both.' (Groq). Default is 1.
#' @param time_info Logical indicating whether the run time of each
#'   request/question should be included in the data. Default = `TRUE`.
#' @param token_info Logical indicating whether the number of prompt and completion tokens
#'   per request should be included in the output data. Default = `TRUE`. When `TRUE`,
#'   the output object will include price information of the conducted screening.
#' @param api_key Numerical value with your personal API key. Find at
#'  \url{https://console.groq.com/keys}. Set with 
#'  `Sys.setenv(GROQ_KEY = "your-api-key")` or use [get_api_key_groq()].
#' @param max_tries,max_seconds 'Cap the maximum number of attempts with
#'  `max_tries` or the total elapsed time from the first request with
#'  `max_seconds`. If neither option is supplied (the default), [httr2::req_perform()]
#'  will not retry'.
#' @param is_transient 'A predicate function that takes a single argument
#'  (the response) and returns `TRUE` or `FALSE` specifying whether or not
#'  the response represents a transient error'.
#' @param backoff 'A function that takes a single argument (the number of failed
#'   attempts so far) and returns the number of seconds to wait'.
#' @param after 'A function that takes a single argument (the response) and
#'   returns either a number of seconds to wait or `NULL`, which indicates
#'   that a precise wait time is not available that the `backoff` strategy
#'   should be used instead'.
#' @param rpm Numerical value indicating the number of requests per minute (rpm)
#'   available for the specified api key.
#' @param reps Numerical value indicating the number of times the same
#'   question should be sent to Groq's API models. This can be useful to test consistency
#'   between answers. Default is `1`.
#' @param seed_par Numerical value for a seed to ensure that proper,
#'   parallel-safe random numbers are produced.
#' @param progress Logical indicating whether a progress line should be shown when running
#'   the title and abstract screening in parallel. Default is `TRUE`.
#' @param decision_description Logical indicating whether to include detailed descriptions
#'   of decisions. Default is `FALSE`.
#' @param messages Logical indicating whether to print messages embedded in the function.
#'   Default is `TRUE`.
#' @param incl_cutoff_upper Numerical value indicating the probability threshold
#'   for which a studies should be included. Default is 0.5, which indicates that
#'   titles and abstracts that Groq's API model has included more than 50 percent of the times
#'   should be included.
#' @param incl_cutoff_lower Numerical value indicating the probability threshold
#'   above which studies should be check by a human. Default is 0.4, which means
#'   that if you ask Groq's API model the same questions 10 times and it includes the
#'   title and abstract 4 times, we suggest that the study should be check by a human.
#' @param force Logical argument indicating whether to force the function to use more than
#'   10 iterations. This argument is developed to avoid the conduct of wrong and extreme sized screening.
#'   Default is `FALSE`.
#'
#' @return An object of class \code{"groq"}. The object is a list containing the following
#' components:
#' \item{answer_data_aggregated}{dataset with the summarized, probabilistic inclusion decision
#' for each title and abstract across multiple repeated questions (only when reps > 1).}
#' \item{answer_data}{dataset with all individual answers.}
#' \item{price_dollar}{numerical value indicating the total price (in USD) of the screening.}
#' \item{price_data}{dataset with prices across all models used for screening.}
#' \item{error_data}{dataset with failed requests (only included if errors occurred).}
#' \item{run_date}{date when the screening was conducted.}
#'
#' @note The \code{answer_data_aggregated} data (only present when reps > 1) contains the following mandatory variables:
#' \tabular{lll}{
#'  \bold{studyid} \tab \code{integer} \tab indicating the study ID of the reference. \cr
#'  \bold{title} \tab \code{character} \tab indicating the title of the reference. \cr
#'  \bold{abstract} \tab \code{character}   \tab indicating the abstract of the reference. \cr
#'  \bold{promptid} \tab \code{integer} \tab indicating the prompt ID. \cr
#'  \bold{prompt} \tab \code{character} \tab indicating the prompt. \cr
#'  \bold{model} \tab \code{character}   \tab indicating the specific model used. \cr
#'  \bold{question} \tab \code{character} \tab indicating the final question sent to Groq's API models. \cr
#'  \bold{top_p} \tab \code{numeric}  \tab indicating the applied top_p. \cr
#'  \bold{incl_p} \tab \code{numeric}  \tab indicating the probability of inclusion calculated across multiple repeated responses on the same title and abstract. \cr
#'  \bold{final_decision_gpt} \tab \code{character} \tab indicating the final decision reached by model - either 'Include', 'Exclude', or 'Check'. \cr
#'  \bold{final_decision_gpt_num}  \tab \code{integer}  \tab indicating the final numeric decision reached by model - either 1 or 0. \cr
#'  \bold{longest_answer}  \tab \code{character} \tab indicating the longest response obtained
#'  across multiple repeated responses on the same title and abstract. Only included if the detailed function
#'  is used. See 'Examples' below for how to use this function. \cr
#'  \bold{reps}  \tab \code{integer}  \tab indicating the number of times the same question has been sent to Groq's API models. \cr
#'  \bold{n_mis_answers} \tab \code{integer} \tab indicating the number of missing responses. \cr
#' }
#' <br>
#' The \code{answer_data} data contains the following mandatory variables:
#' \tabular{lll}{
#'  \bold{studyid} \tab \code{integer} \tab indicating the study ID of the reference. \cr
#'  \bold{title} \tab \code{character} \tab indicating the title of the reference. \cr
#'  \bold{abstract} \tab \code{character}   \tab indicating the abstract of the reference. \cr
#'  \bold{promptid} \tab \code{integer} \tab indicating the prompt ID. \cr
#'  \bold{prompt} \tab \code{character} \tab indicating the prompt. \cr
#'  \bold{model} \tab \code{character}   \tab indicating the specific model used. \cr
#'  \bold{iterations} \tab \code{numeric} \tab indicating the number of times the same question has been sent to Groq's API models. \cr
#'  \bold{question} \tab \code{character} \tab indicating the final question sent to Groq's API models. \cr
#'  \bold{top_p}  \tab \code{numeric} \tab indicating the applied top_p. \cr
#'  \bold{decision_gpt}  \tab \code{character} \tab indicating the raw decision - either \code{"1", "0", "1.1"} for inclusion, exclusion, or uncertainty, respectively. \cr
#'  \bold{detailed_description}  \tab \code{character} \tab indicating detailed description of the given decision made by Groq's API models.
#'  Only included if the detailed function is used. See 'Examples' below for how to use this function. \cr
#'  \bold{decision_binary}  \tab \code{integer} \tab indicating the binary decision,
#'  that is 1 for inclusion and 0 for exclusion. 1.1 decision are coded equal to 1 in this case. \cr
#'  \bold{prompt_tokens}  \tab \code{integer} \tab indicating the number of prompt tokens sent to the server for the given request. \cr
#'  \bold{completion_tokens}  \tab \code{integer} \tab indicating the number of completion tokens sent to the server for the given request. \cr
#'  \bold{run_time}  \tab \code{numeric} \tab indicating the time it took to obtain a response from the server for the given request. \cr
#'  \bold{n} \tab \code{integer} \tab indicating request ID.  \cr
#' }
#' <br>
#' If any requests failed to reach the server, the object contains an
#' error data set (`error_data`) having the same variables as `answer_data`
#' but with failed request references only.
#'
#' <br>
#'
#' The \code{price_data} data contains the following variables:
#' \tabular{lll}{
#'  \bold{model} \tab \code{character} \tab model name. \cr
#'  \bold{input_price_dollar} \tab \code{integer} \tab price for all prompt/input tokens for the correspondent model. \cr
#'  \bold{output_price_dollar}  \tab \code{integer} \tab price for all completion/output tokens for the correspondent model. \cr
#'  \bold{price_total_dollar} \tab \code{integer} \tab total price for all tokens for the correspondent model. \cr
#' }
#'
#' Find current token pricing at \url{https://console.groq.com/docs/pricing}.
#'
#' @importFrom stats df
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Make sure GROQ_KEY is set
#' Sys.setenv(GROQ_KEY = "your-api-key")
#'
#' prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"
#'
#' tabscreen_groq(
#'   data = filges2015_dat[1:2,],
#'   prompt = prompt,
#'   studyid = studyid,
#'   title = title,
#'   abstract = abstract,
#'   model = "llama3-70b-8192",
#'   max_tries = 2
#'   )
#'
#'  # Get detailed descriptions of the decisions by using the
#'  # decision_description option. See example below.
#'  tabscreen_groq(
#'    data = filges2015_dat[1:2,],
#'    prompt = prompt,
#'    studyid = studyid,
#'    title = title,
#'    abstract = abstract,
#'    model = "llama3-70b-8192",
#'    decision_description = TRUE,
#'    max_tries = 2
#'  )
#'}

tabscreen_groq <- function(
  data,
  prompt,
  studyid,
  title,
  abstract,
  ...,
  model = "llama3-70b-8192",
  role = "user",
  tools = NULL,
  tool_choice = NULL,
  top_p = 1,
  time_info = TRUE,
  token_info = TRUE,
  api_key = get_api_key_groq(),
  max_tries = 16,
  max_seconds = NULL,
  is_transient = groq_is_transient,
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
  force = FALSE
  ){
  
  #.......................................
  # Handling inherited objects
  #.......................................
  if (is_groq_tbl(data)) data <- data |> dplyr::select(-c(promptid:n)) |> tibble::as_tibble()
  if (is_groq_agg_tbl(data)) data <- data |> dplyr::select(-c(promptid:n_mis_answers)) |> tibble::as_tibble()
  
  #.......................................
  # Setup functions based on decision_description
  #.......................................
  if(is.null(tools)) {
    tools <- if(decision_description) incl_function else incl_function_simple
  }
  
  if(is.null(tool_choice)) {
    tool_choice <- if(decision_description) list(name = "inclusion_decision") else list(name = "inclusion_decision_simple")
  }
  
  #.......................................
  # Start up - Validation checks
  #.......................................
  
  # Validate model names
  if (any(!is.element(model, groq_model_prices$model))) 
    stop("Unknown Groq model(s) used - check model name(s).")
  
  # Ensuring that users do not conduct wrong screening
  if (max(reps) > 10 && !force){
    max_reps_mes <- paste("* Are you sure you want to use", max(reps), "iterations? If so, set force = TRUE")
    stop(max_reps_mes)
  }

  # Ensuring that the rpm argument fits to the corresponding model
  if (length(rpm) > 1 && length(model) != length(rpm)){
    stop("model and rpm must be of the same length.")
  }

  # Ensuring that the reps argument fits to the corresponding model
  if (length(reps) > 1 && length(model) != length(reps)){
    stop("model and reps must be of the same length.")
  }

  # Default values for incl_cutoff_upper and incl_cutoff_lower when 'reps > 1'
  if (any(reps > 1)) {
    if(is.numeric(incl_cutoff_upper) && is.null(incl_cutoff_lower)) incl_cutoff_lower <- incl_cutoff_upper

    if (is.null(incl_cutoff_upper)) incl_cutoff_upper <- 0.5
    if (is.null(incl_cutoff_lower)) incl_cutoff_lower <- incl_cutoff_upper - 0.1
  }

  # Ensuring proper use of the incl_cutoff_upper and incl_cutoff_lower arguments
  if (is.numeric(incl_cutoff_upper) && is.numeric(incl_cutoff_lower) && incl_cutoff_upper < incl_cutoff_lower){
    stop("incl_cutoff_lower must not exceed incl_cutoff_upper")
  }

  # Avoiding that equivalent prompts are added to function
  if (!missing(prompt)){
    if (n_distinct(prompt) != length(prompt)) stop("Do not add the same prompt twice.")
  }

  # Ensuring that the same model is not called twice by the user
  if (n_distinct(reps) == 1 && n_distinct(model) != length(model)){
    model <- unique(model)
  }

  #.......................................
  # Collecting arguments for error handling
  #.......................................
  arg_list <-
    list(
      role = role,
      tools = tools,
      tool_choice = tool_choice,
      rpm = rpm,
      reps = reps,
      time_info = time_info,
      token_info = token_info,
      max_tries = max_tries,
      max_seconds = max_seconds,
      is_transient = is_transient,
      backoff = backoff,
      after = after,
      seed_par = seed_par,
      progress = progress,
      messages = messages,
      decision_description = decision_description,
      incl_cutoff_upper = incl_cutoff_upper,
      incl_cutoff_lower = incl_cutoff_lower,
      ...
    )

  if ("max_tokens" %in% names(arg_list)){
    if (arg_list$max_tokens < 11) stop("Cannot retrieve results from server with tokens below 11.")
  }

  #.......................................
  # Data manipulation
  #.......................................
  
  # Handle study ID creation
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

  # Factors used for slicing data and ensuring correct length of data
  mp_reps <- if (length(reps) > 1) 1 else length(model)
  mp_rpm <- if (length(rpm) > 1) 1 else length(model)

  model_length <- length(model)
  prompt_length <- length(prompt)
  studyid_length <- dplyr::n_distinct(dat$studyid)

  # Creating the question dataset
  question_dat <-
    dat |>
    dplyr::mutate(
      # Handle missing/empty abstracts or titles
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
      question = iconv(question_raw, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " "), # Transliterate to ASCII, sub unconvertible with space
      question = stringr::str_replace_all(question, "\\s+", " "), # Normalize all whitespace to a single space
      question = trimws(question) # Trim leading/trailing whitespace
    ) |>
    dplyr::select(-question_raw) |>
    dplyr::slice(rep(1:dplyr::n(), each = length(top_p))) |>
    mutate(
      topp = rep(top_p, studyid_length*prompt_length*model_length)
    ) |>
    dplyr::arrange(promptid, model, topp, iterations, studyid)

  #.......................................
  # Approximate price calculation
  #.......................................
  app_price_dat <- .price_groq(question_dat)
  app_price <- sum(app_price_dat$total_price_dollar, na.rm = TRUE)

  #.......................................
  # Startup messages
  #.......................................
  if (messages){
    message(paste0("* The approximate price of the current screening will be around $", app_price, "."))

    if (decision_description){
      message(
        paste0(
          "* Be aware that getting detailed reponses ",
          "will substantially increase the prize of the screening relative to the noted approximate prize."
        )
      )
    }

    if ("No information" %in% unique(question_dat$abstract)) {
      message(
        paste0(
          "* Consider removing references that has no abstract ",
          "since these can distort the accuracy of the screening"
        )
      )
    }
  }

  #.......................................
  # RUNNING QUESTIONS
  #.......................................
  furrr_seed <- if (is.null(seed_par)) TRUE else NULL

  params <- question_dat |>
    dplyr::select(question, model_gpt = model, topp, iterations, req_per_min)

  answer_dat <-
    question_dat |>
    dplyr::mutate(
      res = furrr::future_pmap(
        .l = params,
        .f = .rep_groq_engine,
        role_gpt = role,
        tool = tools,
        t_choice = tool_choice,
        seeds = seed_par,
        time_inf = time_info,
        token_inf = token_info,
        api_key = api_key,
        max_t = max_tries,
        max_s = max_seconds,
        is_trans = is_transient,
        back = backoff,
        aft = after,
        ...,
        .options = furrr::furrr_options(seed = furrr_seed),
        .progress = progress
      )
    ) |>
    tidyr::unnest(res) |>
    dplyr::mutate(run_date = as.character(Sys.Date())) |>
    tibble::new_tibble(class = "groq_tbl")

  #.......................................
  # Catching errors
  #.......................................
  n_error <- answer_dat |> dplyr::filter(is.na(decision_binary)) |> nrow()

  if (messages){
    if (n_error == 1) message(paste("* NOTE: Requests failed for 1 title and abstract."))
    if (n_error > 1) message(paste("* NOTE: Requests failed", n_error, "times."))
  }

  if (n_error > 0) error_refs <- answer_dat |> dplyr::filter(is.na(decision_binary))

  #.......................................
  # Price information calculation
  #.......................................
  price_dat <- if (token_info) .price_groq(answer_dat) else NULL
  price <- if (!is.null(price_dat)) sum(price_dat$total_price_dollar, na.rm = TRUE) else NULL

  #.......................................
  # Making aggregated data (for multiple reps)
  #.......................................
  if (any(reps > 1)) {
    answer_dat_sum <- .aggregate_res_groq(answer_dat, incl_cutoff_upper, incl_cutoff_lower)

    # Final data sum
    answer_dat_aggregated <-
      dplyr::left_join(question_dat, answer_dat_sum) |>
      suppressMessages() |>
      dplyr::select(-c(iterations, req_per_min)) |>
      dplyr::rename(top_p = topp) |>
      tibble::new_tibble(class = "groq_agg_tbl")

    attr(answer_dat_aggregated, "incl_cutoff_upper") <- incl_cutoff_upper
    attr(answer_dat_aggregated, "incl_cutoff_lower") <- incl_cutoff_lower
  } else {
    answer_dat_aggregated <- NULL
  }

  #.......................................
  # Returned output
  #.......................................
  res <- list(
    price_data = price_dat,
    price_dollar = price,
    answer_data = answer_dat,
    answer_data_aggregated = answer_dat_aggregated,
    error_data = if (n_error > 0) error_refs else NULL,
    run_date = Sys.Date()
  )

  # If token info is not wanted
  if (!token_info) res[["price_data"]] <- res[["price_dollar"]] <- NULL

  # If no screening errors
  if (n_error == 0) res[["error_data"]] <- NULL

  # Remove aggregated data if not needed
  if (all(reps == 1)) res[["answer_data_aggregated"]] <- NULL

  # Attributing used arguments to res. Used for error handling
  attr(res, "arg_list") <- arg_list

  # Define class
  class(res) <- c("groq", "list")

  res
}

#.......................................................................
#  Functions
#.......................................................................

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