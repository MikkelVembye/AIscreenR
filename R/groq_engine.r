###############################################################
# Function to send a single request to GROQS API models
###############################################################

.groq_engine <- function(
    body,
    RPM,
    time_inf,
    token_inf, 
    api_key, 
    max_t,
    max_s, 
    is_trans, 
    back, 
    aft 
) {
  # Detect if detailed tool is present based on tools list
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

  # Indicates how the detailed description variable is handled when the function
  # error and the detailed function is called vs not called.
  detail_desc_default <- if (detailed) NA_character_ else NULL

  # Max tries and gpt_is_transient not relevant if 'max_t = 0'
  if (max_t == 0) max_t <- is_trans <- NULL
  
  # Starting time
  tictoc::tic()

  # Request url
  url <- "https://api.groq.com/openai/v1/chat/completions"
  
  # Creating the request
  req <-
    httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_key)
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
    resp <- try(suppressMessages(req |> httr2::req_perform()), silent = TRUE)
    
    # If request was successful
    if (status_code() == 200) {
      resp <- resp |> httr2::resp_body_json()
      
      # Initialize default values
      decision_val <- NA_character_
      detailed_desc_val <- detail_desc_default
      decision_bin_val <- NA_real_
      prompt_tok_val <- if (token_inf && !is.null(resp$usage)) resp$usage$prompt_tokens else NA_real_
      completion_tok_val <- if (token_inf && !is.null(resp$usage)) resp$usage$completion_tokens else NA_real_
      
      # Parse response for decision and detailed description
      if (!is.null(resp$choices[[1]]$message$tool_calls)) {
        tool_call <- resp$choices[[1]]$message$tool_calls[[1]]
        if (tool_call$type == "function" && !is.null(tool_call$'function'$arguments)) {
          func_arguments_json <- tool_call$'function'$arguments
          func_args <- try(jsonlite::fromJSON(func_arguments_json), silent = TRUE)
          if (!inherits(func_args, "try-error")) {
            decision_val <- as.character(func_args$decision_gpt)
            # Optional detailed description
            if (detailed && "detailed_description" %in% names(func_args)) {
              detailed_desc_val <- as.character(func_args$detailed_description)
            } else if (detailed) {
              detailed_desc_val <- NA_character_
            }
          } else {
            decision_val <- paste0("Error: Failed to parse tool call arguments. JSON: ", substr(func_arguments_json, 1, 100))
          }
        } else {
          decision_val <- "Error: Unexpected tool_call structure or missing arguments."
        }
      } else if (!is.null(resp$choices[[1]]$message$content)) {
        # Fallback to content parsing if no tool_calls
        content_text <- resp$choices[[1]]$message$content
        parsed_content <- try(jsonlite::fromJSON(content_text), silent = TRUE)
        if (!inherits(parsed_content, "try-error")) {
          # Look for possible keys for decision and detailed description
          if ("decision_gpt" %in% names(parsed_content)) {
            decision_val <- as.character(parsed_content$decision_gpt)
          } else if ("decision" %in% names(parsed_content)) {
            decision_val <- as.character(parsed_content$decision)
          } else {
            decision_val <- "Error: 'decision_gpt' or 'decision' not in content."
          }
          if (detailed) {
            if ("detailed_description" %in% names(parsed_content)) {
              detailed_desc_val <- as.character(parsed_content$detailed_description)
            } else if ("description" %in% names(parsed_content)) {
              detailed_desc_val <- as.character(parsed_content$description)
            } else if ("reasoning" %in% names(parsed_content)) {
              detailed_desc_val <- as.character(parsed_content$reasoning)
            } else if ("explanation" %in% names(parsed_content)) {
              detailed_desc_val <- as.character(parsed_content$explanation)
            } else {
              detailed_desc_val <- NA_character_
            }
          }
        } else {
          # If the model returned non-JSON content
          decision_val <- paste0("Error: Failed to parse content as JSON. Content: ", substr(content_text, 1, 100))
        }
      } else {
        # No usable output found
        decision_val <- "Error: No tool_calls and no content in response."
      }
      
      # Map decisions to binary (detect any '1' in string)
      decision_bin_val <- as.numeric(dplyr::if_else(stringr::str_detect(decision_val, "1"), 1, 0, missing = NA_real_))
      res_list <- list(decision_gpt = decision_val, decision_binary = decision_bin_val)
      if (detailed) res_list$detailed_description <- detailed_desc_val
      if (token_inf) {
        res_list$prompt_tokens <- prompt_tok_val
        res_list$completion_tokens <- completion_tok_val
      }
    res <- tibble::as_tibble(res_list) |>
      dplyr::relocate(tidyselect::any_of("detailed_description"), .after = decision_binary)

    } else {
      # If request failed
      res_list <- list(
        decision_gpt = error_message(),
        decision_binary = NA_real_
      )
      if (detailed) res_list$detailed_description <- detail_desc_default
      if (token_inf) {
        res_list$prompt_tokens <- NA_real_
        res_list$completion_tokens <- NA_real_
      }
    res <- tibble::as_tibble(res_list) |>
      dplyr::relocate(tidyselect::any_of("detailed_description"), .after = decision_binary)
    }
  } else {
    # No internet
    res_list <- list(
      decision_gpt = "Error: Could not reach host [check internet connection]",
      decision_binary = NA_real_
    )
    if (detailed) res_list$detailed_description <- detail_desc_default
    if (token_inf) {
      res_list$prompt_tokens <- NA_real_
      res_list$completion_tokens <- NA_real_
    }
    res <- tibble::as_tibble(res_list) |>
      dplyr::relocate(tidyselect::any_of("detailed_description"), .after = decision_binary)
    }
  time <- tictoc::toc(quiet = TRUE)
  run_time_val <- round(as.numeric(time$toc - time$tic), 1)
  if (time_inf) res <- res |> dplyr::mutate(run_time = run_time_val)
  if (!time_inf && "run_time" %in% names(res)) res <- res |> dplyr::select(-run_time)
  if (!token_inf && "prompt_tokens" %in% names(res)) res <- res |> dplyr::select(-prompt_tokens)
  if (!token_inf && "completion_tokens" %in% names(res)) res <- res |> dplyr::select(-completion_tokens)
  return(res)
}

################################################################
# Function to send repeated requests to GROQ's API models
################################################################

.rep_groq_engine <- function(
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
    token_inf,
    api_key, 
    max_t, 
    max_s, 
    is_trans, 
    back,
    aft, 
    ... 
) {
  # Detect detailed mode from tools or explicit choice
  detailed_for_wrapper <- FALSE
  if (is.list(tool)) {
    detailed_for_wrapper <- any(vapply(tool, function(t) {
      fn <- t[["function"]]
      !is.null(fn) && identical(fn$name, "inclusion_decision")
    }, logical(1)))
  }
  if (!detailed_for_wrapper && is.list(t_choice)) {
    # Also allow explicit selection to trigger detailed mode
    if (!is.null(t_choice$name) && identical(t_choice$name, "inclusion_decision")) detailed_for_wrapper <- TRUE
    if (!is.null(t_choice$type) && identical(t_choice$type, "function") &&
        !is.null(t_choice$`function`) && identical(t_choice$`function`$name, "inclusion_decision")) detailed_for_wrapper <- TRUE
  }

  # Allocate columns
  t_info_wrapper <- if (time_inf) NA_real_ else NULL
  p_tokens_wrapper <- if (token_inf) NA_real_ else NULL
  c_tokens_wrapper <- if (token_inf) NA_real_ else NULL
  
  # Create a function that generates an error tibble
  create_error_df <- function(is_detailed) {
    error_list <- list(
      decision_gpt = "Error [possibly a JSON error from wrapper]",
      decision_binary = NA_real_
    )
    if (is_detailed) error_list$detailed_description <- NA_character_
    if (token_inf) {
      error_list$prompt_tokens <- p_tokens_wrapper
      error_list$completion_tokens <- c_tokens_wrapper
    }
    if (time_inf) error_list$run_time <- t_info_wrapper
      df <- tibble::as_tibble(error_list)
    if (is_detailed && !"detailed_description" %in% names(df)) df$detailed_description <- NA_character_
    if (is_detailed) df <- df |> dplyr::relocate(detailed_description, .after = decision_binary)
    if (!token_inf && "prompt_tokens" %in% names(df)) df <- df |> dplyr::select(-prompt_tokens)
    if (!token_inf && "completion_tokens" %in% names(df)) df <- df |> dplyr::select(-completion_tokens)
    if (!time_inf && "run_time" %in% names(df)) df <- df |> dplyr::select(-run_time)
    df
  }
  # Create a safe version of .groq_engine that returns an error tibble on failure
  safe_groq_engine <- suppressWarnings(
    purrr::possibly(
      .groq_engine,
      otherwise = create_error_df(detailed_for_wrapper)
    )
  )
  
  api_body <- list(
    model = model_gpt,
    messages = list(list(role = role_gpt, content = question)),
    top_p = topp
  )
  # Pass tools through as provided (already Groq/OpenAI compatible)
  if (!is.null(tool)) {
    api_body$tools <- tool
  }
  # Pass tool_choice through if provided; support string or object
  if (!is.null(t_choice)) {
    api_body$tool_choice <- t_choice
  }
  
  # Add any additional arguments to the body
  additional_args <- list(...)
  if (length(additional_args) > 0) {
    api_body <- c(api_body, additional_args)
  }
  iter_seq <- if(iterations > 1) 1:iterations else 1
  furrr_seed_opt <- if (is.null(seeds)) TRUE else NULL
  
  # Running repeated requests in parallel, and return tibble
  final_res <-
    furrr::future_map_dfr(
      iter_seq, \(i) {
        result <- safe_groq_engine(
          body = api_body, 
          RPM = req_per_min, 
          time_inf = time_inf,
          token_inf = token_inf,
          api_key = api_key,
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

.aggregate_res_groq <- function(answer_data, incl_cutoff_u, incl_cutoff_l) {
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