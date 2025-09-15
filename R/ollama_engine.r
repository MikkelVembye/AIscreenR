###############################################################
# Function to send a single request to ollamaS API models
###############################################################

.ollama_engine <- function(
    body,
    RPM,
    time_inf,
    max_t,
    max_s, 
    is_trans, 
    back, 
    aft 
) {
  detailed <- FALSE
  if (!is.null(body$tools) && length(body$tools) > 0) {
    tool_names <- try(purrr::map_chr(body$tools, function(t) {
      fn <- t[["function"]]
      if (!is.null(fn) && !is.null(fn$name)) fn$name else NA_character_
    }), silent = TRUE)
    if (!inherits(tool_names, "try-error")) {
      detailed <- any(tool_names == "inclusion_decision", na.rm = TRUE)
    }
  }

  detail_desc_default <- if (detailed) NA_character_ else NULL

  if (max_t == 0) max_t <- is_trans <- NULL
  
  tictoc::tic()

  url <- "http://127.0.0.1:11434/api/chat"
  
  headers <- list(
    content_type = "application/json",
    accept = "application/json",
    user_agent = "AIscreenR"
  )

  # Ensure non-streaming response for JSON parsing
  if (is.null(body$stream)) body$stream <- FALSE

  req <-
    httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(!!!headers) |>
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

  resp <- req |> httr2::req_perform() |> httr2::resp_body_json()
    print(resp)
  decision_val <- NA_character_
  detailed_desc_val <- detail_desc_default

  tc <- NULL
  if (!is.null(resp$message) && !is.null(resp$message$tool_calls)) tc <- resp$message$tool_calls

  if (!is.null(tc)) {
    call <- if (is.list(tc) && !is.null(tc[[1]])) tc[[1]] else tc
    fn_obj <- call[["function"]]
    if (!is.null(fn_obj)) {
      args_raw <- fn_obj[["arguments"]]
      func_args <- NULL
      if (is.list(args_raw)) {
        func_args <- args_raw
      } else if (is.character(args_raw)) {
        parsed <- try(jsonlite::fromJSON(args_raw), silent = TRUE)
        if (!inherits(parsed, "try-error")) func_args <- parsed
      }
      if (!is.null(func_args)) {
        if (!is.null(func_args$decision_gpt)) {
          decision_val <- as.character(func_args$decision_gpt)
        } else if (!is.null(func_args$decision)) {
          decision_val <- as.character(func_args$decision)
        } else {
          decision_val <- NA_character_
        }
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
        decision_val <- paste0("Error: Failed to parse tool call arguments. JSON: ", substr(func_arguments_json, 1, 100))
      }
    } else {
      decision_val <- "Error: Unexpected tool_call structure or missing arguments."
    }
  } else {
    content_text <- tryCatch(resp$message$content, error = function(e) "")
    if (nzchar(content_text)) {
      parsed_content <- try(jsonlite::fromJSON(content_text), silent = TRUE)
      if (!inherits(parsed_content, "try-error")) {
        if (!is.null(parsed_content$decision_gpt)) {
          decision_val <- as.character(parsed_content$decision_gpt)
        } else if (!is.null(parsed_content$decision)) {
          decision_val <- as.character(parsed_content$decision)
        } else {
          decision_val <- NA_character_
        }
        if (detailed) {
          dd <- NA_character_
          if (!is.null(parsed_content$detailed_description)) dd <- parsed_content$detailed_description
          else if (!is.null(parsed_content$description)) dd <- parsed_content$description
          else if (!is.null(parsed_content$reasoning)) dd <- parsed_content$reasoning
          else if (!is.null(parsed_content$explanation)) dd <- parsed_content$explanation
          detailed_desc_val <- as.character(dd)
        }
      } else {
        decision_val <- paste0("Error: Failed to parse content as JSON. Content: ", substr(content_text, 1, 100))
      }
    } else {
      decision_val <- "Error: No tool_calls and no content in response."
    }
  }
  
  decision_bin_val <- as.numeric(dplyr::if_else(stringr::str_detect(decision_val, "1"), 1, 0, missing = NA_real_))
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
    req_per_min, 
    role_gpt, 
    tool, 
    t_choice, 
    seeds, 
    time_inf,
    max_t, 
    max_s, 
    is_trans = NULL, 
    back,
    aft, 
    system_guard_msg = NULL, 
    ... 
) {
  detailed_for_wrapper <- FALSE
  if (is.list(tool)) {
    detailed_for_wrapper <- any(vapply(tool, function(t) {
      fn <- t[["function"]]
      !is.null(fn) && identical(fn$name, "inclusion_decision")
    }, logical(1)))
  }
  if (!detailed_for_wrapper && is.list(t_choice)) {
    if (!is.null(t_choice$name) && identical(t_choice$name, "inclusion_decision")) detailed_for_wrapper <- TRUE
    if (!is.null(t_choice$type) && identical(t_choice$type, "function") &&
        !is.null(t_choice$`function`) && identical(t_choice$`function`$name, "inclusion_decision")) detailed_for_wrapper <- TRUE
  }

  t_info_wrapper <- if (time_inf) NA_real_ else NULL

  create_error_df <- function(is_detailed) {
    error_list <- list(
      decision_gpt = "Error [possibly a JSON error from wrapper]",
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
          RPM = req_per_min, 
          time_inf = time_inf,
          max_t = max_t,
          max_s = max_s,
          is_trans = is_trans,
          back = back,
          aft = aft
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