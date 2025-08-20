#' Single request to Groq API
#'
#' @description Internal function that sends a single request to the Groq API
#' @param body Request body list containing model, messages, and other parameters
#' @param RPM Requests per minute limit for throttling
#' @param time_inf Logical indicating whether to include timing information
#' @param token_inf Logical indicating whether to include token information  
#' @param api_key Groq API key
#' @param max_t Maximum number of retry attempts
#' @param max_s Maximum seconds for retry attempts
#' @param is_trans Function to determine if error is transient
#' @param back Backoff strategy for retries
#' @param aft After function for retries
#' @return Tibble with API response results
#' @keywords internal
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
  detailed <- FALSE
  if (!is.null(body$tool_choice)) {
    if (is.list(body$tool_choice) && 
        !is.null(body$tool_choice$type) && body$tool_choice$type == "function" &&
        !is.null(body$tool_choice$'function') && is.list(body$tool_choice$'function') &&
        !is.null(body$tool_choice$'function'$name)) {
      detailed <- body$tool_choice$'function'$name == "inclusion_decision"
    }
  }

  detail_desc_default <- if(detailed) NA_character_ else NULL
  if (max_t == 0) max_t <- is_trans <- NULL
  
  tictoc::tic()
  url <- "https://api.groq.com/openai/v1/chat/completions"
  
  req <-
    httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_key)
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

  if (curl::has_internet()) {
    resp_httr <- try(
      suppressMessages(req |> httr2::req_perform()),
      silent = TRUE
    )
    
    current_status_code <- status_code()

    if (current_status_code == 200) {
      resp_json <- resp_httr |> httr2::resp_body_json()
      
      decision_val <- NA_character_
      detailed_desc_val <- detail_desc_default
      decision_bin_val <- NA_real_
      prompt_tok_val <- if(token_inf && !is.null(resp_json$usage)) resp_json$usage$prompt_tokens else NA_real_
      completion_tok_val <- if(token_inf && !is.null(resp_json$usage)) resp_json$usage$completion_tokens else NA_real_

      if (!is.null(resp_json$choices[[1]]$message$tool_calls)) {
        tool_call <- resp_json$choices[[1]]$message$tool_calls[[1]]
        if (tool_call$type == "function" && !is.null(tool_call$'function'$arguments)) {
          func_arguments_json <- tool_call$'function'$arguments
          func_args <- try(jsonlite::fromJSON(func_arguments_json), silent = TRUE)
          
          if (!inherits(func_args, "try-error")) {
            decision_val <- as.character(func_args$decision_gpt)
            if (detailed && "detailed_description" %in% names(func_args)) {
              detailed_desc_val <- as.character(func_args$detailed_description)
            } else if (detailed) {
              detailed_desc_val <- NA_character_
            }
          } else {
            decision_val <- paste0("Error: Failed to parse tool call arguments. JSON: ", substr(func_arguments_json,1,100))
          }
        } else {
          decision_val <- "Error: Unexpected tool_call structure or missing arguments."
        }
      } else if (!is.null(resp_json$choices[[1]]$message$content)) {
        content_text <- resp_json$choices[[1]]$message$content
        parsed_content <- try(jsonlite::fromJSON(content_text), silent = TRUE)
        
        if (inherits(parsed_content, "try-error")) {
            json_pattern <- "\\{.*\\}"
            json_matches <- regmatches(content_text, regexec(json_pattern, content_text, perl = TRUE))
            if (length(json_matches) > 0 && length(json_matches[[1]]) > 0) {
                extracted_json <- json_matches[[1]][1]
                parsed_content <- try(jsonlite::fromJSON(extracted_json), silent = TRUE)
            }
        }

        if (!inherits(parsed_content, "try-error")) {
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
          decision_val <- paste0("Error: Failed to parse content as JSON. Content: ", substr(content_text,1,100))
        }
      } else {
        decision_val <- "Error: No tool_calls and no content in response."
      }
      
      decision_bin_val <- as.numeric(dplyr::if_else(stringr::str_detect(decision_val, "1"), 1, 0, missing = NA_real_))

      res_list <- list(
        decision_gpt = decision_val,
        decision_binary = decision_bin_val
      )
      if (detailed) res_list$detailed_description <- detailed_desc_val
      if (token_inf) {
        res_list$prompt_tokens <- prompt_tok_val
        res_list$completion_tokens <- completion_tok_val
      }
      res <- tibble::as_tibble(res_list)
      
      if (detailed && "detailed_description" %in% names(res)) {
          res <- res |> dplyr::relocate(detailed_description, .after = decision_binary)
      }

    } else { 
      res_list <- list(
        decision_gpt = status_code_text_GROQ(),
        decision_binary = NA_real_
      )
      if (detailed) res_list$detailed_description <- detail_desc_default
      if (token_inf) {
        res_list$prompt_tokens <- NA_real_
        res_list$completion_tokens <- NA_real_
      }
      res <- tibble::as_tibble(res_list)
    }
  } else { 
    res_list <- list(
      decision_gpt = "Error: Could not reach host [check internet connection]",
      decision_binary = NA_real_
    )
    if (detailed) res_list$detailed_description <- detail_desc_default
    if (token_inf) {
      res_list$prompt_tokens <- NA_real_
      res_list$completion_tokens <- NA_real_
    }
    res <- tibble::as_tibble(res_list)
  }

  time <- tictoc::toc(quiet = TRUE)
  run_time_val <- round(as.numeric(time$toc - time$tic), 1)

  if (time_inf) res <- res |> dplyr::mutate(run_time = run_time_val)

  if (!token_inf && "prompt_tokens" %in% names(res)) res <- res |> dplyr::select(-prompt_tokens)
  if (!token_inf && "completion_tokens" %in% names(res)) res <- res |> dplyr::select(-completion_tokens)
  
  return(res)
}

#' Send repeated requests to Groq API
#'
#' @description Internal function that handles multiple iterations of API requests
#' @param question Question text to send to the API
#' @param model_gpt Groq model name
#' @param topp Top-p parameter for sampling
#' @param iterations Number of iterations to run
#' @param req_per_min Requests per minute limit
#' @param role_gpt Role for the conversation (typically "user")
#' @param tool Tools specification for function calling
#' @param t_choice Tool choice specification
#' @param seeds Random seeds for reproducibility
#' @param time_inf Logical indicating whether to include timing information
#' @param token_inf Logical indicating whether to include token information
#' @param api_key Groq API key
#' @param max_t Maximum number of retry attempts
#' @param max_s Maximum seconds for retry attempts
#' @param is_trans Function to determine if error is transient
#' @param back Backoff strategy for retries
#' @param aft After function for retries
#' @param ... Additional arguments passed to the API body
#' @return Tibble with results from all iterations
#' @keywords internal
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
  detailed_for_wrapper <- if (is.list(t_choice) && !is.null(t_choice$name)) {
    t_choice$name == "inclusion_decision"
  } else if (is.list(t_choice) && !is.null(t_choice$type) && t_choice$type == "function" &&
             !is.null(t_choice$'function') && !is.null(t_choice$'function'$name)) {
    t_choice$'function'$name == "inclusion_decision"
  } else if (is.character(t_choice)) {
    t_choice == "inclusion_decision"
  } else {
    FALSE
  }

  t_info_wrapper <- if (time_inf) NA_real_ else NULL
  p_tokens_wrapper <- if (token_inf) NA_real_ else NULL
  c_tokens_wrapper <- if (token_inf) NA_real_ else NULL
  
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
    error_list$run_date <- as.character(Sys.Date())
    
    df <- tibble::as_tibble(error_list)
    
    # Ensure column order and presence/absence matches .groq_engine output
    if (is_detailed && !"detailed_description" %in% names(df)) df$detailed_description <- NA_character_
    if (is_detailed) df <- df |> dplyr::relocate(detailed_description, .after = decision_binary)

    if (!token_inf && "prompt_tokens" %in% names(df)) df <- df |> dplyr::select(-prompt_tokens)
    if (!token_inf && "completion_tokens" %in% names(df)) df <- df |> dplyr::select(-completion_tokens)
    if (!time_inf && "run_time" %in% names(df)) df <- df |> dplyr::select(-run_time)
    df
  }

  safe_groq_engine <- suppressWarnings(
    purrr::possibly(
      .groq_engine,
      otherwise = create_error_df(detailed_for_wrapper)
    )
  )
  
  api_body <- list(
    model = model_gpt,
    messages = list(
      list(
        role = role_gpt,
        content = question
      )
    ),
    top_p = topp
  )
  
  if (!is.null(tool)) {
    groq_tools_list <- list()
    for (i in seq_along(tool)) {
      t_item <- tool[[i]]
      groq_tools_list[[i]] <- list(
        type = "function",
        `function` = list(
          name = t_item$name,
          description = t_item$description,
          parameters = t_item$parameters
        )
      )
    }
    api_body$tools <- groq_tools_list
  }

  if (!is.null(t_choice)) {
    if (is.character(t_choice) && t_choice %in% c("auto", "none", "required")) {
      api_body$tool_choice <- t_choice
    } else if (is.list(t_choice) && !is.null(t_choice$name)) { # Input from tabscreen_groq
      api_body$tool_choice <- list(
        type = "function",
        `function` = list(name = t_choice$name)
      )
    } else if (is.character(t_choice)) { 
       api_body$tool_choice <- list(
        type = "function",
        `function` = list(name = t_choice)
      )
    } else if (is.list(t_choice) && !is.null(t_choice$type) && t_choice$type == "function") {
        api_body$tool_choice <- t_choice
    }
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
        return(result)
      },
      .options = furrr::furrr_options(seed = furrr_seed_opt)
    ) |>
    dplyr::mutate(n = iter_seq)
  
  final_res
}

#' Aggregate results from multiple iterations
#'
#' @description Internal function that aggregates results across multiple API calls
#' @param answer_data Dataset containing individual API responses
#' @param incl_cutoff_u Upper threshold for inclusion decisions
#' @param incl_cutoff_l Lower threshold for inclusion decisions
#' @return Aggregated dataset with summary statistics
#' @keywords internal
.aggregate_res_groq <- function(answer_data, incl_cutoff_u, incl_cutoff_l) {
  sum_dat <-
    answer_data |>
    dplyr::summarise(
      incl_p = mean(decision_binary == 1, na.rm = TRUE),
      
      final_decision_gpt = dplyr::case_when(
        incl_p < incl_cutoff_u & incl_p >= incl_cutoff_l ~ "Check",
        incl_p >= incl_cutoff_u ~ "Include",
        incl_p < incl_cutoff_l ~ "Exclude",
        TRUE ~ NA_character_
      ),
      
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
      dplyr::filter(decision_binary == final_decision_gpt_num) |>
      dplyr::arrange(promptid, model, topp, iterations, studyid, dplyr::desc(n_words_answer)) |>
      dplyr::summarise(
        longest_answer = detailed_description[1],
        .by = c(studyid:topp)
      )
    
    sum_dat <-
      dplyr::left_join(sum_dat, long_answer_dat_sum, by = c("studyid", "promptid", "prompt", "model", "topp")) |>
      suppressMessages() |>
      dplyr::relocate(longest_answer, .after = final_decision_gpt_num)
  }
  
  sum_dat
}