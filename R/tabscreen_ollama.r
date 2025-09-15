tabscreen_ollama <- function(
  data,
  prompt,
  studyid,
  title,
  abstract,
  ...,
  model = "llama3.2",
  role = "user",
  tools = NULL,
  tool_choice = NULL,
  top_p = 1,
  time_info = TRUE,
  token_info = TRUE,
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
      invalid_models <- model[!model %in% available_models]
      stop(paste(
        "The following model(s) are not available in your local Ollama instance:",
        paste(invalid_models, collapse = ", ")
      ))
    }
  } else if (messages) {
    message("* Could not retrieve models from Ollama (is it running at 127.0.0.1:11434?). Skipping model validation.")
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
      token_info = token_info,
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
  # Startup messages
  #.......................................
  if (messages){

    if (decision_description){
      message(
        paste0(
          "* Be aware that getting detailed reponses ",
          "will substantially increase the time of the screening relative to the noted approximate prize."
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
        token_inf = token_info,
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
  # Price information calculation
  #.......................................
  price_dat <- if (token_info) .price_gpt(answer_dat) else NULL
  price <- if (!is.null(price_dat)) sum(price_dat$total_price_dollar, na.rm = TRUE) else NULL

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
  class(res) <- c("ollama", "list")

  res
}