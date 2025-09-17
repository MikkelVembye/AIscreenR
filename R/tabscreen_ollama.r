#' @encoding UTF-8
#' @title Title and abstract screening with OLLAMA API models using function calls via the tools argument
#'
#' @name tabscreen_ollama
#' @aliases tabscreen_ollama
#'
#' @description
#' This function supports the conduct of title and abstract screening with OLLAMA API models in R.
#' Specifically, it allows the user to draw on locally hosted ollama models (e.g., Llama 3 / 3.1 variants, Mixtral/Mistral, Gemma, DeepSeek and Qwen).
#' For more information on how to install and use OLLAMA, see \url{https://ollama.com/docs/}.
#' Be aware that this function requires that you have OLLAMA installed and running on your local machine.
#' The function allows to run title and abstract screening across multiple prompts and with
#' repeated questions to check for consistency across answers. All of which can be done in parallel.
#' The function draws on the newly developed function calling which is called via the
#' tools argument in the request body. Function calls ensure more reliable and consistent responses to ones
#' requests. See [Vembye, Christensen, Mølgaard, and Schytt. (2025)](https://osf.io/preprints/osf/yrhzm)
#' for guidance on how adequately to conduct title and abstract screening with OLLAMA models.
#'
#' @references Vembye, M. H., Christensen, J., Mølgaard, A. B., & Schytt, F. L. W. (2024)
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
#' @param ... Further argument to pass to the request body.
#'
#' @return An object of class \code{"ollama"}. The object is a list containing the following
#' components:
#' \item{answer_data_aggregated}{dataset with the summarized, probabilistic inclusion decision
#' for each title and abstract across multiple repeated questions (only when reps > 1).}
#' \item{answer_data}{dataset with all individual answers.}
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
#'  \bold{run_time}  \tab \code{numeric} \tab indicating the time it took to obtain a response from the server for the given request. \cr
#'  \bold{n} \tab \code{integer} \tab indicating request ID.  \cr
#' }
#' <br>
#' If any requests failed to reach the server, the object contains an
#' error data set (`error_data`) having the same variables as `answer_data`
#' but with failed request references only.
#'
#' @importFrom stats df
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"
#'
#' plan(multisession)
#'
#' tabscreen_groq(
#'   data = filges2015_dat[1:2,],
#'   prompt = prompt,
#'   studyid = studyid,
#'   title = title,
#'   abstract = abstract,
#'   model = "llama3.2:latest",
#'   max_tries = 2
#'   )
#' plan(sequential)
#'
#'  # Get detailed descriptions of the decisions by using the
#'  # decision_description option.
#' plan(multisession)
#'
#'  tabscreen_groq(
#'    data = filges2015_dat[1:2,],
#'    prompt = prompt,
#'    studyid = studyid,
#'    title = title,
#'    abstract = abstract,
#'    model = "llama3.2:latest",
#'    decision_description = TRUE,
#'    max_tries = 2
#'  )
#' plan(sequential)
#'}


tabscreen_ollama <- function(
  data,
  prompt,
  studyid,
  title,
  abstract,
  ...,
  model = "llama3.2:latest",
  role = "user",
  tools = NULL,
  tool_choice = NULL,
  top_p = 1,
  time_info = TRUE,
  max_tries = 16,
  max_seconds = NULL,
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
  if (is_ollama_tbl(data)) data <- data |> dplyr::select(-c(promptid:n)) |> tibble::as_tibble()
  if (is_ollama_agg_tbl(data)) data <- data |> dplyr::select(-c(promptid:n_mis_answers)) |> tibble::as_tibble()

  #.......................................
  # Setup functions based on decision_description
  #.......................................
  if(is.null(tools)) {
    tools <- if(decision_description) tools_detailed else tools_simple
  }

  # Force the exact function name
  if (is.null(tool_choice)) {
    forced_fn <- if (decision_description) "inclusion_decision" else "inclusion_decision_simple"
    tool_choice <- list(
      type = "function",
      "function" = list(name = forced_fn)
    )
  } else if (is.character(tool_choice) && identical(tool_choice, "required")) {
    forced_fn <- if (decision_description) "inclusion_decision" else "inclusion_decision_simple"
    tool_choice <- list(
      type = "function",
      "function" = list(name = forced_fn)
    )
  }

  #.......................................
  # Start up - Validation checks
  #.......................................

  # Validate model names
  models_url <- "http://127.0.0.1:11434/api/tags"
  # Try to query local Ollama for available models. If not reachable, skip validation with a message.
  available_models <- tryCatch({
    httr2::request(models_url) |>
      httr2::req_method("GET") |>
      httr2::req_user_agent("AIscreenR (local-ollama)") |>
      httr2::req_perform() |>
      httr2::resp_body_json() |>
      (
        function(x) {
          if (!is.null(x$models)) vapply(x$models, function(m) m$name, character(1)) else character(0)
        }
      )()
  }, error = function(e) character(0))
  if (length(available_models) > 0) {
    if (!all(model %in% available_models)) {
      stop(paste(
        "Use one of the available models:",
        paste(available_models, collapse = ", ")
      ))
    }
  } else if (messages) {
    message("* Could not retrieve models from Ollama.")
  }

  # Ensuring that users do not conduct wrong screening
  if (max(reps) > 10 && !force){
    max_reps_mes <- paste("* Are you sure you want to use", max(reps), "iterations? If so, set force = TRUE")
    stop(max_reps_mes)
  }

  # Ensuring that the rpm argument fits to the corresponding model
  if (length(rpm) > 1 && length(model) != length(rpm)){
    stop("model and rpm must be of the same length.")
    }
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
      max_tries = max_tries,
      max_seconds = max_seconds,
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
  # Startup messages
  #.......................................
  if (messages){

    if (decision_description){
      message(
        paste0(
          "* Be aware that getting detailed reponses ",
          "will substantially increase the time of the screening."
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

  # Detailed system that models must follow in order to ensure proper function calling
  forced_fn <- if (decision_description) "inclusion_decision" else "inclusion_decision_simple"
  tool_guard_msg <- paste0(
    "You are a function-calling agent. For each request",
    "you must call the tool '", forced_fn, "' exactly once and only this tool. ",
    "Do not write natural language in the message content. Return only via the tool call. ")

  params <- question_dat |>
    dplyr::select(question, model_gpt = model, topp, iterations, req_per_min)

  answer_dat <-
    question_dat |>
    dplyr::mutate(
      res = furrr::future_pmap(
        .l = params,
        .f = .rep_ollama_engine,
        role_gpt = role,
        tool = tools,
        t_choice = tool_choice,
        system_guard_msg = tool_guard_msg,
        seeds = seed_par,
        time_inf = time_info,
        max_t = max_tries,
        max_s = max_seconds,
        back = backoff,
        aft = after,
        ...,
        .options = furrr::furrr_options(seed = furrr_seed),
        .progress = progress
      )
    ) |>
    tidyr::unnest(res) |>
    dplyr::mutate(run_date = as.character(Sys.Date())) |>
    tibble::new_tibble(class = "ollama_tbl")

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
  # Making aggregated data (for multiple reps)
  #.......................................
  if (any(reps > 1)) {
    answer_dat_sum <- .aggregate_res_ollama(answer_dat, incl_cutoff_upper, incl_cutoff_lower)

    # Final data sum
    answer_dat_aggregated <-
      dplyr::left_join(question_dat, answer_dat_sum) |>
      suppressMessages() |>
      dplyr::select(-c(iterations, req_per_min)) |>
      dplyr::rename(top_p = topp) |>
      tibble::new_tibble(class = "ollama_agg_tbl")

    attr(answer_dat_aggregated, "incl_cutoff_upper") <- incl_cutoff_upper
    attr(answer_dat_aggregated, "incl_cutoff_lower") <- incl_cutoff_lower
  } else {
    answer_dat_aggregated <- NULL
  }

  #.......................................
  # Returned output
  #.......................................
  res <- list(
    answer_data = answer_dat,
    answer_data_aggregated = answer_dat_aggregated,
    error_data = if (n_error > 0) error_refs else NULL,
    run_date = Sys.Date()
  )

  # If no screening errors
  if (n_error == 0) res[["error_data"]] <- NULL

  # Remove aggregated data if not needed
  if (all(reps == 1)) res[["answer_data_aggregated"]] <- NULL

  # Attributing used arguments to res. Used for error handling
  attr(res, "arg_list") <- arg_list

  # Define class
  class(res) <- c("ollama", "list")

  res
}
