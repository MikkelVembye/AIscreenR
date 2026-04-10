###############################################################
# Function to send a single request to ollamaS API models
###############################################################

.ollama_engine <- function(
    body,
    time_inf,
    max_t,
    max_s, 
    back, 
    aft,
    endpoint_url 
) {
  detailed <- FALSE
  if (!is.null(body$tool_choice) &&
      is.list(body$tool_choice) &&
      identical(body$tool_choice$type, "function") &&
      !is.null(body$tool_choice$`function`) &&
      body$tool_choice$`function`$name %in% c("inclusion_decision", "inclusion_decision_binary")) {
    detailed <- TRUE
  }

  detail_desc_default <- if (detailed) NA_character_ else NULL

  # Determine expected function name for validation
  expected_fn <- NULL

  # If tool_choice specifies a function, use that for expected function name
  if (!is.null(body$tool_choice) &&
      is.list(body$tool_choice) &&
      identical(body$tool_choice$type, "function") &&
      !is.null(body$tool_choice$`function`) &&
      !is.null(body$tool_choice$`function`$name)) {
    expected_fn <- as.character(body$tool_choice$`function`$name)
  }

  # If no expected function name from tool_choice, but tools are provided, use the first tool's function name for expected function name
  if (is.null(expected_fn) && is.list(body$tools) && length(body$tools) > 0 &&
      !is.null(body$tools[[1]][["function"]]) &&
      !is.null(body$tools[[1]][["function"]][["name"]])) {
    expected_fn <- as.character(body$tools[[1]][["function"]][["name"]])
  }

  # Determine allowed decision values from tool definition if available, otherwise use default
  allowed_decisions <- c("1", "0", "1.1")
  if (is.list(body$tools) && length(body$tools) > 0) {
    tool_enum <- body$tools[[1]][["function"]][["parameters"]][["properties"]][["decision_gpt"]][["enum"]]

    # If enum is defined for the decision_gpt parameter, use those values as allowed decisions
    if (!is.null(tool_enum) && length(tool_enum) > 0) {
      allowed_decisions <- as.character(unlist(tool_enum, use.names = FALSE))
    }
  }

  # Function to validate the decision value returned from the model
  validate_decision <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return("Error: Tool call was missing decision value.")
    }
    decision <- trimws(as.character(x[[1]]))
    if (!nzchar(decision)) {
      return("Error: Tool call returned an empty decision value.")
    }
    if (decision %in% allowed_decisions) {
      return(decision)
    }
    paste0(
      "Error: Tool call returned invalid decision '",
      substr(decision, 1, 80),
      "'. Allowed values are: ",
      paste(allowed_decisions, collapse = ", "),
      "."
    )
  }

  if (max_t == 0) max_t <- NULL
  
  tictoc::tic()
  
  headers <- list(
    content_type = "application/json",
    accept = "application/json",
    user_agent = "AIscreenR"
  )

  # Ensure non-streaming response for JSON parsing
  if (is.null(body$stream)) body$stream <- FALSE

  req <-
    httr2::request(endpoint_url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(!!!headers) |>
    httr2::req_body_json(body) |>
    httr2::req_retry(
      max_tries = max_t,
      max_seconds = max_s,
      backoff = back,
      after = aft
    ) |>
    httr2::req_user_agent("AIscreenR (http://mikkelvembye.github.io/AIscreenR/)")

  resp <- req |> httr2::req_perform() |> httr2::resp_body_json()
  decision_val <- NA_character_
  detailed_desc_val <- detail_desc_default
  tc <- NULL

  # Extract tool calls from the response for validation and parsing
  if (!is.null(resp$message) && !is.null(resp$message$tool_calls)) tc <- resp$message$tool_calls

  # Validate that a tool call is present in the response
  if (!is.null(tc)) {
    call <- if (is.list(tc) && !is.null(tc[[1]])) tc[[1]] else tc
    fn_obj <- call[["function"]]
    if (!is.null(fn_obj)) {
      # Validate that the function called by the model matches the expected function name if available
      actual_fn <- fn_obj[["name"]]
      if (!is.null(expected_fn) && !is.null(actual_fn) && !identical(actual_fn, expected_fn)) {
        decision_val <- paste0(
          "Error: Received wrong function call '", actual_fn,
          "'. Expected '", expected_fn, "'."
        )
      } else {
        # If function name is as expected or no expected function name is defined, proceed to parse arguments
      args_raw <- fn_obj[["arguments"]]
      func_args <- NULL
      # Attempt to parse arguments as list or JSON, and extract the decision value
      if (is.list(args_raw)) {
        func_args <- args_raw
      
      # If arguments are provided as a character string, attempt to parse as JSON
      } else if (is.character(args_raw)) {
        parsed <- try(jsonlite::fromJSON(args_raw), silent = TRUE)
        # If parsing is successful and results in a list, use that as function arguments
        if (!inherits(parsed, "try-error")) func_args <- parsed
      }

      # Extract the decision value from the function arguments using multiple possible keys, and validate the decision value
      if (!is.null(func_args)) {
        decision_candidate <- if (!is.null(func_args$decision_gpt)) {
          func_args$decision_gpt
        } else if (!is.null(func_args$decision)) {
          func_args$decision
        } else {
          NULL
        }
        decision_val <- validate_decision(decision_candidate)

        # If detailed description is expected, attempt to extract it from multiple possible keys
        if (detailed) {
          if (!is.null(func_args$detailed_description)) {
            detailed_desc_val <- as.character(func_args$detailed_description)
          } else if (!is.null(func_args$description)) {
            detailed_desc_val <- as.character(func_args$description)
          } else if (!is.null(func_args$reasoning)) {
            detailed_desc_val <- as.character(func_args$reasoning)
          } else if (!is.null(func_args$explanation)) {
            detailed_desc_val <- as.character(func_args$explanation)
          } else {
            detailed_desc_val <- NA_character_
          }
        }
      } else {
        decision_val <- paste0("Error: Failed to parse tool call arguments. JSON: ", substr(as.character(args_raw), 1, 100))
      }
      }
    } else {
      decision_val <- "Error: Missing function object in tool_call response."
    }
  } else {
    decision_val <- "Error: Model did not return a function call (tool_calls missing)."
  }

  # Map decision values to binary numeric valueS
  decision_bin_val <- dplyr::case_when(
    decision_val %in% c("1", "1.1") ~ 1,
    decision_val == "0" ~ 0,
    TRUE ~ NA_real_
  )

  # Compile results into a tibble, including the decision value, binary mapping, and detailed description if applicable
  res_list <- list(decision_gpt = decision_val, decision_binary = decision_bin_val)
  if (detailed) res_list$detailed_description <- detailed_desc_val
  res <- tibble::as_tibble(res_list) |>
    dplyr::relocate(tidyselect::any_of("detailed_description"), .after = tidyselect::all_of("decision_binary"))

  time <- tictoc::toc(quiet = TRUE)
  run_time_val <- round(as.numeric(time$toc - time$tic), 1)
  if (time_inf) res <- res |> dplyr::mutate(run_time = run_time_val)
  if (!time_inf) res <- res |> dplyr::select(-tidyselect::any_of("run_time"))
  return(res)
}

################################################################
# Function to send repeated requests to ollama's API models
################################################################

.rep_ollama_engine <- function(
    question,
    model_gpt, 
    topp, 
    iterations,
    role_gpt, 
    tool, 
    t_choice, 
    seeds, 
    time_inf,
    max_t, 
    max_s, 
    back,
    aft, 
    system_guard_msg = NULL, 
    endpoint_url,
    ... 
) {
  detailed_for_wrapper <- FALSE
  if (is.list(tool)) {
    detailed_for_wrapper <- any(vapply(tool, function(t) {
      fn <- t[["function"]]
      !is.null(fn) && fn$name %in% c("inclusion_decision", "inclusion_decision_binary")
    }, logical(1)))
  }
  if (!detailed_for_wrapper && is.list(t_choice)) {
    if (!is.null(t_choice$name) && t_choice$name %in% c("inclusion_decision", "inclusion_decision_binary")) detailed_for_wrapper <- TRUE
    if (!is.null(t_choice$type) && identical(t_choice$type, "function") &&
        !is.null(t_choice$`function`) && t_choice$`function`$name %in% c("inclusion_decision", "inclusion_decision_binary")) detailed_for_wrapper <- TRUE
  }

  t_info_wrapper <- if (time_inf) NA_real_ else NULL

  create_error_df <- function(is_detailed) {
    error_list <- list(
      decision_gpt = paste0(
        "Error: Request failed at endpoint '", endpoint_url,
        "'. Check that Ollama is running and api_url points to '/api/chat' ",
        "(e.g., 'http://127.0.0.1:11434/api/chat')."
      ),
      decision_binary = NA_real_
    )
    if (is_detailed) error_list$detailed_description <- NA_character_
    if (time_inf) error_list$run_time <- t_info_wrapper
    df <- tibble::as_tibble(error_list)
    if (is_detailed && !"detailed_description" %in% names(df)) df$detailed_description <- NA_character_
    if (is_detailed) df <- df |> dplyr::relocate(tidyselect::any_of("detailed_description"), .after = tidyselect::all_of("decision_binary"))
    if (!time_inf) df <- df |> dplyr::select(-tidyselect::any_of("run_time"))
    df
  }
  safe_ollama_engine <- suppressWarnings(
    purrr::possibly(
      .ollama_engine,
      otherwise = create_error_df(detailed_for_wrapper)
    )
  )
  
  messages <- if (!is.null(system_guard_msg)) {
    list(
      list(role = "system", content = system_guard_msg),
      list(role = role_gpt, content = question)
    )
  } else {
    list(list(role = role_gpt, content = question))
  }

  api_body <- list(
    model = model_gpt,
    messages = messages,
    top_p = topp,
    stream = FALSE
  )
  if (!is.null(tool)) {
    api_body$tools <- tool
  }
  if (is.null(t_choice) || (is.character(t_choice) && identical(t_choice, "required"))) {
    fn_name <- tryCatch(tool[[1]][["function"]][["name"]], error = function(e) NULL)
    if (!is.null(fn_name)) {
      api_body$tool_choice <- list(type = "function", "function" = list(name = fn_name))
    }
  } else if (is.character(t_choice) && !identical(t_choice, "auto")) {
    api_body$tool_choice <- list(type = "function", "function" = list(name = t_choice))
  } else {
    api_body$tool_choice <- t_choice
  }
  
  additional_args <- list(...)
  if (length(additional_args) > 0) {
    api_body <- c(api_body, additional_args)
  }
  iter_seq <- if(iterations > 1) 1:iterations else 1
  furrr_seed_opt <- if (is.null(seeds)) TRUE else NULL
  
  final_res <-
    furrr::future_map_dfr(
      iter_seq, \(i) {
        result <- safe_ollama_engine(
          body = api_body, 
          time_inf = time_inf,
          max_t = max_t,
          max_s = max_s,
          back = back,
          aft = aft,
          endpoint_url = endpoint_url
        )
        result <- dplyr::mutate(result, n = i)
        return(result)
      },
      .options = furrr::furrr_options(seed = furrr_seed_opt)
    )
  
  final_res
}

################################################################################
# Function used to aggregate responses when repeating the same question is used.
################################################################################

.aggregate_res_ollama <- function(answer_data, incl_cutoff_u, incl_cutoff_l) {
  sum_dat <-
    answer_data |>
    dplyr::summarise(
      # Proportion of inclusion decisions
      incl_p = mean(decision_binary == 1, na.rm = TRUE),
      
      # Map proportion to final decision
      final_decision_gpt = dplyr::case_when(
        incl_p < incl_cutoff_u & incl_p >= incl_cutoff_l ~ "Check",
        incl_p >= incl_cutoff_u ~ "Include",
        incl_p < incl_cutoff_l ~ "Exclude",
        TRUE ~ NA_character_
      ),
      
      # Numeric mapping of final decision
      final_decision_gpt_num = dplyr::case_when(
        incl_p < incl_cutoff_u & incl_p >= incl_cutoff_l ~ 1,
        incl_p >= incl_cutoff_u ~ 1,
        incl_p < incl_cutoff_l ~ 0,
        TRUE ~ NA_real_
      ),
      
      reps = dplyr::n(),
      
      n_mis_answers = sum(is.na(decision_binary)),
      
      .by = c(studyid:topp)
    )
  
  # If detailed description is present, extract the longest answer among those
  if ("detailed_description" %in% names(answer_data)){
    long_answer_dat_sum <-
      answer_data |>
      dplyr::mutate(
        incl_p = mean(decision_binary == 1, na.rm = TRUE),
        
        final_decision_gpt_num = dplyr::case_when(
          incl_p < incl_cutoff_u & incl_p >= incl_cutoff_l ~ 1,
          incl_p >= incl_cutoff_u ~ 1,
          incl_p < incl_cutoff_l ~ 0,
          TRUE ~ NA_real_
        ),
        
        n_words_answer = stringr::str_count(detailed_description, '\\w+'),
        
        .by = c(studyid:topp)
      ) |>
      # Filter to only those matching the final decision
      dplyr::filter(decision_binary == final_decision_gpt_num) |>
      dplyr::arrange(promptid, model, topp, iterations, studyid, dplyr::desc(n_words_answer)) |>
      dplyr::summarise(
        longest_answer = detailed_description[1],
        .by = c(studyid:topp)
      )
    
    # Join longest answer back to summary data
    sum_dat <-
      dplyr::left_join(sum_dat, long_answer_dat_sum, by = c("studyid", "promptid", "prompt", "model", "topp")) |>
      suppressMessages() |>
      dplyr::relocate(longest_answer, .after = final_decision_gpt_num)
  }
  
  sum_dat
}