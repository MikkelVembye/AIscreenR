###############################################################
# Function to send a single request to claudeS API models
###############################################################

.claude_engine <- function(
    body,
    RPM,
    time_inf,
    token_inf, 
    api_key, 
    max_t,
    max_s, 
    is_trans, 
    back, 
    aft,
    endpoint_url
) {

  # Detect if detailed tool is present based on tools list
  detailed <- FALSE
  if (!is.null(body$tools) && is.list(body$tools)) {
    detailed <- any(vapply(body$tools, function(t) {
      !is.null(t$name) && t$name %in% c("inclusion_decision", "inclusion_decision_binary")
    }, logical(1)))
  }

  # Indicates how the detailed description variable is handled when the function
  # error and the detailed function is called vs not called.
  detail_desc <- if (detailed) NA_character_ else NULL

  # Max tries and gpt_is_transient not relevant if 'max_t = 0'
  if (max_t == 0) max_t <- is_trans <- NULL
  
  # Starting time
  tictoc::tic()

  # Creating the request
  req <-
    httr2::request(endpoint_url) |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("v1/messages") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "anthropic-version" = "2023-06-01",
      "X-Api-Key" = api_key
    ) |>
    httr2::req_body_json(body) |>
    # Automatic retry with backoff and transient detection
    httr2::req_retry(
      max_tries = max_t,
      max_seconds = max_s,
      is_transient = is_trans,
      backoff = back,
      after = aft
    ) |>
    # Rate limiting
    httr2::req_throttle(RPM/60) |>
    httr2::req_user_agent("AIscreenR (http://mikkelvembye.github.io/AIscreenR/)")
  
  # Check if internet connection is on
  if (curl::has_internet()) {
    resp <- try(
      suppressMessages(req |> httr2::req_perform()),
      silent = TRUE
    )
    
    # If request was successful
    if (status_code() == 200) {
      resp <- resp |> httr2::resp_body_json()

      tool_input <- NULL
      if (!is.null(resp$content) && length(resp$content) > 0) {
        tool_use_blocks <- Filter(function(x) identical(x$type, "tool_use"), resp$content)
        if (length(tool_use_blocks) > 0) {
          tool_input <- tool_use_blocks[[1]]$input
        } else if (is.character(resp$content[[1]])) {
          content_text <- resp$content[[1]]
          tool_input <- try(jsonlite::fromJSON(content_text), silent = TRUE)
          if (inherits(tool_input, "try-error")) tool_input <- NULL
        }
      }

      # Anthropic usage uses input_tokens/output_tokens; keep prompt/completion naming
      prompt_tok <- if (!is.null(resp$usage$prompt_tokens)) {
        resp$usage$prompt_tokens
      } else if (!is.null(resp$usage$input_tokens)) {
        resp$usage$input_tokens
      } else {
        NA_real_
      }
      completion_tok <- if (!is.null(resp$usage$completion_tokens)) {
        resp$usage$completion_tokens
      } else if (!is.null(resp$usage$output_tokens)) {
        resp$usage$output_tokens
      } else {
        NA_real_
      }
      submod <- if (!is.null(resp$model)) resp$model else NA_character_

      tib_text <- if (!is.null(tool_input)) {
        try(tibble::as_tibble(tool_input), silent = TRUE)
      } else {
        try(stop("No tool_use blocks and no text content in response."), silent = TRUE)
      }

      # Code what returned data, when JSON format errors appear
      if (inherits(tib_text, "try-error")) {
        p_tokens <- c_tokens <- if (token_inf) NA_real_ else NULL
        res <- tibble::tibble(
          decision_gpt = as.character(tib_text[1]),
          decision_binary = NA_real_,
          detailed_description = detail_desc,
          prompt_tokens = p_tokens,
          completion_tokens = c_tokens,
          submodel = submod
        )
      } else {
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

      if (detailed) {
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
      # If request failed (non-2xx status)
      error_msg <- error_message()
      res <- tibble::tibble(
        decision_gpt = error_msg,
        decision_binary = NA_real_,
        detailed_description = detail_desc,
        prompt_tokens = NA_real_,
        completion_tokens = NA_real_,
        submodel = NA_character_
      )
    }
  } else {
    # No internet
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

  if (!time_inf) res <- res |> dplyr::select(-run_time)
  if (!token_inf && "prompt_tokens" %in% names(res)) res <- res |> dplyr::select(-prompt_tokens)
  if (!token_inf && "completion_tokens" %in% names(res)) res <- res |> dplyr::select(-completion_tokens)
  return(res)
}

################################################################
# Function to send repeated requests to claude's API models
################################################################

.rep_claude_engine <- function(
    question,
    model_gpt, 
    iterations,
    req_per_min, 
    reasoning_effort,
    role_gpt, 
    tool, 
    seeds, 
    time_inf,
    token_inf,
    api_key, 
    max_t, 
    max_s, 
    is_trans, 
    back,
    aft, 
    system_guard_msg = NULL,
    endpoint_url,
    max_tokens = 1024,
    ... 
) {
  # Build messages
  messages <- list(list(role = role_gpt, content = question))

  body <- list(
    model = model_gpt,
    messages = messages,
    max_tokens = max_tokens
  )
  # Pass tools through as provided
  if (!is.null(tool)) {
    body$tools <- tool
  }

  # Check if model supports adaptive thinking
  adaptive_thinking_models <- c("claude-mythos-preview", "claude-opus-4-7", "claude-opus-4-6", "claude-sonnet-4-6")
  supports_adaptive_thinking <- any(grepl(paste(adaptive_thinking_models, collapse = "|"), model_gpt, ignore.case = TRUE))
  
  # Add thinking/reasoning if provided and model supports it
  if (supports_adaptive_thinking && !is.na(reasoning_effort) && !is.null(reasoning_effort) && reasoning_effort != "none") {
    body$thinking <- list(type = "adaptive")
    if (reasoning_effort != "adaptive") {
      body$output_config <- list(effort = reasoning_effort)
    }
  }
  
  # Add any additional arguments to the body
  additional_args <- list(...)
  if (length(additional_args) > 0) {
    body <- c(body, additional_args)
  }
  # Setting the iterations
  if (iterations > 1) iterations <- 1:iterations

  furrr_seed <- if (base::is.null(seeds)) TRUE else NULL

  # Running repeated requests in parallel, and return tibble
  suppressWarnings(
    furrr::future_map_dfr(
      iterations, \(i) .claude_engine(
        body = body,
        RPM = req_per_min,
        time_inf = time_inf,
        token_inf = token_inf,
        api_key = api_key,
        max_t = max_t,
        max_s = max_s,
        is_trans = is_trans,
        back = back,
        aft = aft,
        endpoint_url = endpoint_url
      ),
      .options = furrr::furrr_options(seed = furrr_seed)
    )
  ) |>
    dplyr::mutate(n = iterations)
}
