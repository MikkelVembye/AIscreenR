# Internal function to process a single screening run. This is called by future_pmap.
.process_single_ft_run <- function(current_file_path, current_prompt, current_model, current_rep_num, current_study_id, current_prompt_id, current_vector_store_name, precomputed_supplementary = NULL,
                                   base_url, api_key, top_p, temperature,
                                   decision_description, assistant_name, assistant_description,
                                   assistant_instructions, max_tries, tools_simple, tools_detailed, tools_supplementary, rpm,
                                   sleep_time_val, protocol_file_path,
                                   # Retry parameters
                                   max_seconds_val, is_transient_fn_val, backoff_val, after_val) {  
  start_time_single_run <- Sys.time()
  
  result_single_run <- list(
    decision_gpt = NULL, detailed_description = NULL,
    supplementary = precomputed_supplementary, status = NULL,
    model = current_model, run_date = Sys.Date(), decision_binary = NA, studyid = current_study_id,
    promptid = current_prompt_id, title = basename(current_file_path), prompt = current_prompt,
    iterations = current_rep_num, top_p = top_p, run_time = NA_real_, 
    prompt_tokens = NA_integer_, completion_tokens = NA_integer_ 
  )
  
  tryCatch({
    # Create vector store, upload file, add file to store
    vector_store_resp <- .create_openai_vector_store(base_url, api_key, current_vector_store_name, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val)  
    vector_store_id <- vector_store_resp$id
    file_resp <- .upload_openai_file(base_url, api_key, current_file_path, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val)  
    file_id <- file_resp$id
    .add_file_to_openai_vector_store(base_url, api_key, vector_store_id, file_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val)  

    # Create or get assistant and update it
    include_supp_tool <- is.null(precomputed_supplementary) || is.na(precomputed_supplementary)
    assistant_name_comp <- paste(c(assistant_name, current_model, if (!include_supp_tool) "nosupp" else NULL), collapse = "_")
    adjusted_instructions <- if (!include_supp_tool) {
      paste0(
        assistant_instructions,
        "\n\nIMPORTANT: Supplementary presence is precomputed outside this run. Do NOT call supplementary_check, and do NOT discuss supplementary materials in your responses."
      )
    } else {
      assistant_instructions
    }
    assistant <- .get_or_create_openai_assistant(
      base_url, api_key, assistant_name_comp, assistant_description, adjusted_instructions, current_model,
      decision_description, tools_simple, tools_detailed, tools_supplementary, temperature, top_p, rpm,
      max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val, include_supp_tool = include_supp_tool
    )
    assistant_id <- assistant$id
    .update_openai_assistant_vector_store(base_url, api_key, assistant_id, vector_store_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val)  

    # Create and run thread (inject no-supplementary note if precomputed)
    prompt_for_thread <- if (!include_supp_tool) {
      paste0(current_prompt, "\n\nDo not re-evaluate or discuss supplementary materials; treat supplementary presence as already handled externally.")
    } else {
      current_prompt
    }
    thread_resp <- .create_openai_thread(base_url, api_key, prompt_for_thread, file_id, vector_store_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val, is_protocol = !is.null(protocol_file_path))
    thread_id <- thread_resp$id
    run_resp <- .run_openai_thread(base_url, api_key, thread_id, assistant_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val, decision_description)  
    run_id <- run_resp$id
    
    # Wait for completion
    completion_result <- .wait_for_run_completion(base_url, api_key, thread_id, run_id, run_resp, max_tries, sleep_time_val, max_seconds_val, is_transient_fn_val, backoff_val, after_val, rpm, decision_description)
    
    # Update results from completion
    result_single_run <- utils::modifyList(result_single_run, completion_result$result_updates)
    result_single_run$status <- completion_result$status

    # Always keep the precomputed supplementary if available
    if (!is.null(precomputed_supplementary) && !is.na(precomputed_supplementary)) {
      result_single_run$supplementary <- precomputed_supplementary
    }

    if (completion_result$status == "completed") {
      if (!is.null(completion_result$run_resp$usage)) {
        result_single_run$prompt_tokens <- completion_result$run_resp$usage$prompt_tokens %||% NA_integer_
        result_single_run$completion_tokens <- completion_result$run_resp$usage$completion_tokens %||% NA_integer_
      }
    } else {
      stop(paste("Run failed after", completion_result$retries_count, "tries. Final status:", completion_result$status, "-", completion_result$run_resp$last_error$message %||% "No error message."))
    }

    # Clean up API objects
    try(.delete_openai_file_api(base_url, api_key, file_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val), silent = TRUE)  
    try(.delete_openai_vector_store_api(base_url, api_key, vector_store_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val), silent = TRUE)  
    try(.delete_openai_assistant_api(base_url, api_key, assistant_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val), silent = TRUE)

    result_single_run$run_time <- as.numeric(difftime(Sys.time(), start_time_single_run, units = "secs"))
    
    # Return a success data frame
    return(.create_result_df(result_single_run, decision_description, status_message = "Completed"))

  }, error = function(e) {
    warning(paste0("Error processing ", basename(current_file_path), " (Rep ", current_rep_num, "): ", e$message))
    result_single_run$run_time <- as.numeric(difftime(Sys.time(), start_time_single_run, units = "secs"))
    # Return an error data frame
    return(.create_result_df(result_single_run, decision_description, status_message = paste("Failed:", substr(e$message, 1, 200))))
  })
}

# Wait for a run to complete, handling tool calls
.wait_for_run_completion <- function(base_url, api_key, thread_id, run_id, run_resp, max_tries, sleep_time_val, max_seconds_val, is_transient_fn_val, backoff_val, after_val, rpm, decision_description) {
  retries_count <- 0
  status <- run_resp$status
  result_updates <- list(decision_gpt = NULL, detailed_description = NULL, supplementary = NULL, status = status)
  
  while ((status %in% c("queued", "in_progress", "requires_action")) && retries_count < max_tries) {
    retries_count <- retries_count + 1
    if (status == "requires_action") {
      tool_calls <- run_resp$required_action$submit_tool_outputs$tool_calls
      tool_outputs <- list()
      for (tool_call in tool_calls) {
        if (tool_call$type == "function") {
          function_name <- tool_call$"function"$name
          arguments <- jsonlite::fromJSON(tool_call$"function"$arguments)
          if (function_name == "supplementary_check") {
            result_updates$supplementary <- arguments$supplementary
          } else if (function_name == "inclusion_decision_simple") {
            result_updates$decision_gpt <- arguments$decision_gpt
            result_updates$decision_binary <- if(tolower(arguments$decision_gpt) %in% c("0", "exclude")) 0 else if (tolower(arguments$decision_gpt) %in% c("1", "include")) 1 else 1.1
          } else if (function_name == "inclusion_decision") {
            result_updates$decision_gpt <- arguments$decision_gpt
            result_updates$detailed_description <- arguments$detailed_description
            result_updates$decision_binary <- if(tolower(arguments$decision_gpt) %in% c("0", "exclude")) 0 else if (tolower(arguments$decision_gpt) %in% c("1", "include")) 1 else 1.1
          }
          tool_outputs <- append(tool_outputs, list(list(tool_call_id = tool_call$id, output = jsonlite::toJSON(arguments, auto_unbox = TRUE))))
        }
      }
      run_resp <- .submit_openai_tool_outputs(base_url, api_key, thread_id, run_id, tool_outputs, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val)  
      status <- run_resp$status
      result_updates$status <- status
      next
    }
    Sys.sleep(sleep_time_val)
    run_resp <- .get_openai_run_status(base_url, api_key, thread_id, run_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val)  
    status <- run_resp$status
    result_updates$status <- status
  }
  
  list(status = status, run_resp = run_resp, retries_count = retries_count, result_updates = result_updates)
}

# --- OpenAI API Wrappers ---
.get_or_create_openai_assistant <- function(base_url, api_key, assistant_name, assistant_description, assistant_instructions, model, decision_description, tools_simple, tools_detailed, tools_supplementary, temperature, top_p, rpm, max_tries_val, max_seconds_val, is_transient_fn_val, backoff_val, after_val, include_supp_tool = TRUE) {
  assistants_resp <- httr2::request(base_url) |> httr2::req_url_path_append("assistants") |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries_val, max_seconds = max_seconds_val, is_transient = is_transient_fn_val, backoff = backoff_val, after = after_val) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_perform() |> httr2::resp_body_json()
  existing_assistant <- purrr::keep(assistants_resp$data, ~ .x$name == assistant_name)
  if (length(existing_assistant) > 0) return(existing_assistant[[1]])
  
  assistant_tools <- c(
    list(list(type = "file_search")),
    if (isTRUE(include_supp_tool)) tools_supplementary else list(),
    if (decision_description) tools_detailed else tools_simple
  )
  
  httr2::request(base_url) |> httr2::req_url_path_append("assistants") |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries_val, max_seconds = max_seconds_val, is_transient = is_transient_fn_val, backoff = backoff_val, after = after_val) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_body_json(list(model = model, name = assistant_name, description = assistant_description, instructions = assistant_instructions, tools = assistant_tools, temperature = temperature, top_p = top_p, response_format = "auto")) |> httr2::req_perform() |> httr2::resp_body_json()
}

# Helper to run the supplementary tool once per file
.run_supplementary_only <- function(base_url, api_key, current_file_path, current_model, assistant_name_prefix, assistant_description, tools_supplementary, temperature, top_p, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val, sleep_time_val) {
  # Create short-lived resources
  vs_name <- paste0("SuppOnly_", gsub("[^A-Za-z0-9_.-]", "_", basename(current_file_path)), "_", as.integer(Sys.time()))
  vector_store_resp <- .create_openai_vector_store(base_url, api_key, vs_name, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val)
  vector_store_id <- vector_store_resp$id
  file_resp <- .upload_openai_file(base_url, api_key, current_file_path, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val)
  file_id <- file_resp$id
  .add_file_to_openai_vector_store(base_url, api_key, vector_store_id, file_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val)

  # Assistant with ONLY supplementary tool
  assistant_name <- paste0(assistant_name_prefix, "_supponly_", current_model)
  assistant <- .get_or_create_openai_assistant(
    base_url, api_key, assistant_name, assistant_description,
    "You must ONLY call the supplementary_check function on the attached document and then stop. Do not make any inclusion decision.",
    current_model, FALSE, tools_simple = list(), tools_detailed = list(), tools_supplementary = tools_supplementary,
    temperature = 0, top_p = 1, rpm = rpm,
    max_tries_val = max_tries, max_seconds_val = max_seconds_val,
    is_transient_fn_val = is_transient_fn_val, backoff_val = backoff_val, after_val = after_val,
    include_supp_tool = TRUE
  )
  assistant_id <- assistant$id
  .update_openai_assistant_vector_store(base_url, api_key, assistant_id, vector_store_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val)

  # Thread: ask only for supplementary_check
  thread_resp <- .create_openai_thread(
    base_url, api_key,
    prompt = "Run supplementary_check only for the attached document and return the result via the function call.",
    file_id = file_id, vector_store_id = vector_store_id,
    rpm = rpm, max_tries = max_tries, max_seconds = max_seconds_val,
    is_transient_fn = is_transient_fn_val, backoff_fn = backoff_val, after_fn = after_val,
    is_protocol = FALSE
  )
  thread_id <- thread_resp$id
  run_resp <- .run_openai_thread(base_url, api_key, thread_id, assistant_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val, decision_description = FALSE)
  run_id <- run_resp$id

  completion_result <- .wait_for_run_completion(base_url, api_key, thread_id, run_id, run_resp, max_tries, sleep_time_val, max_seconds_val, is_transient_fn_val, backoff_val, after_val, rpm, decision_description = FALSE)

  # Cleanup
  supp_val <- completion_result$result_updates$supplementary %||% NA_character_
  try(.delete_openai_file_api(base_url, api_key, file_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val), silent = TRUE)
  try(.delete_openai_vector_store_api(base_url, api_key, vector_store_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val), silent = TRUE)
  try(.delete_openai_assistant_api(base_url, api_key, assistant_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val), silent = TRUE)

  # Normalize to yes/no if possible
  to_yes_no <- function(x) {
    if (is.null(x) || is.na(x)) return(NA_character_)
    if (is.logical(x)) return(if (isTRUE(x)) "yes" else "no")
    x_chr <- tolower(as.character(x))
    if (x_chr %in% c("true","yes","1","present","found")) "yes"
    else if (x_chr %in% c("false","no","0","absent","none")) "no"
    else x_chr
  }
  to_yes_no(supp_val)
}

.create_openai_vector_store <- function(base_url, api_key, vector_stores_name, rpm, max_tries, max_seconds, is_transient_fn, backoff_fn, after_fn) {
  httr2::request(base_url) |> httr2::req_url_path_append("vector_stores") |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries, max_seconds = max_seconds, is_transient = is_transient_fn, backoff = backoff_fn, after = after_fn) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_body_json(list(name = vector_stores_name)) |> httr2::req_perform() |> httr2::resp_body_json()
}
.upload_openai_file <- function(base_url, api_key, file_path, rpm, max_tries, max_seconds, is_transient_fn, backoff_fn, after_fn) {
  httr2::request(base_url) |> httr2::req_url_path_append("files") |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries, max_seconds = max_seconds, is_transient = is_transient_fn, backoff = backoff_fn, after = after_fn) |> httr2::req_headers("Content-Type" = "multipart/form-data") |> httr2::req_body_multipart(purpose = "assistants", file = curl::form_file(file_path)) |> httr2::req_perform() |> httr2::resp_body_json()
}
.add_file_to_openai_vector_store <- function(base_url, api_key, vector_store_id, file_id, rpm, max_tries, max_seconds, is_transient_fn, backoff_fn, after_fn) {
  httr2::request(base_url) |> httr2::req_url_path_append(paste0("vector_stores/", vector_store_id, "/files")) |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries, max_seconds = max_seconds, is_transient = is_transient_fn, backoff = backoff_fn, after = after_fn) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_body_json(list(file_id = file_id)) |> httr2::req_perform()
}
.update_openai_assistant_vector_store <- function(base_url, api_key, assistant_id, vector_store_id, rpm, max_tries, max_seconds, is_transient_fn, backoff_fn, after_fn) {
  httr2::request(base_url) |> httr2::req_url_path_append(paste0("assistants/", assistant_id)) |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries, max_seconds = max_seconds, is_transient = is_transient_fn, backoff = backoff_fn, after = after_fn) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_body_json(list(tool_resources = list(file_search = list(vector_store_ids = list(vector_store_id))))) |> httr2::req_perform()
}
.create_openai_thread <- function(base_url, api_key, prompt, file_id, vector_store_id, rpm, max_tries, max_seconds, is_transient_fn, backoff_fn, after_fn, is_protocol = FALSE) {
  enhanced_prompt <- if (is_protocol) .create_protocol_prompt(prompt) else .create_direct_prompt(prompt)
  httr2::request(base_url) |> httr2::req_url_path_append("threads") |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries, max_seconds = max_seconds, is_transient = is_transient_fn, backoff = backoff_fn, after = after_fn) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_body_json(list(messages = list(list(role = "user", content = enhanced_prompt, attachments = list(list(file_id = file_id, tools = list(list(type = "file_search")))))), tool_resources = list(file_search = list(vector_store_ids = list(vector_store_id))))) |> httr2::req_perform() |> httr2::resp_body_json()
}
.run_openai_thread <- function(base_url, api_key, thread_id, assistant_id, rpm, max_tries, max_seconds, is_transient_fn, backoff_fn, after_fn, decision_description) {
  httr2::request(base_url) |> httr2::req_url_path_append(paste0("threads/", thread_id, "/runs")) |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries, max_seconds = max_seconds, is_transient = is_transient_fn, backoff = backoff_fn, after = after_fn) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_body_json(list(assistant_id = assistant_id, tool_choice = "required")) |> httr2::req_perform() |> httr2::resp_body_json()
}
.get_openai_run_status <- function(base_url, api_key, thread_id, run_id, rpm, max_tries, max_seconds, is_transient_fn, backoff_fn, after_fn) {
  httr2::request(base_url) |> httr2::req_url_path_append(paste0("threads/", thread_id, "/runs/", run_id)) |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries, max_seconds = max_seconds, is_transient = is_transient_fn, backoff = backoff_fn, after = after_fn) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_perform() |> httr2::resp_body_json()
}
.submit_openai_tool_outputs <- function(base_url, api_key, thread_id, run_id, tool_outputs, rpm, max_tries, max_seconds, is_transient_fn, backoff_fn, after_fn) {
  httr2::request(base_url) |> httr2::req_url_path_append(paste0("threads/", thread_id, "/runs/", run_id, "/submit_tool_outputs")) |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries, max_seconds = max_seconds, is_transient = is_transient_fn, backoff = backoff_fn, after = after_fn) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_body_json(list(tool_outputs = tool_outputs)) |> httr2::req_perform() |> httr2::resp_body_json()
}
.delete_openai_file_api <- function(base_url, api_key, file_id, rpm, max_tries, max_seconds, is_transient_fn, backoff_fn, after_fn) {
  httr2::request(base_url) |> httr2::req_url_path_append(paste0("files/", file_id)) |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries, max_seconds = max_seconds, is_transient = is_transient_fn, backoff = backoff_fn, after = after_fn) |> httr2::req_method("DELETE") |> httr2::req_perform()
}
.delete_openai_vector_store_api <- function(base_url, api_key, vector_store_id, rpm, max_tries, max_seconds, is_transient_fn, backoff_fn, after_fn) {
  httr2::request(base_url) |> httr2::req_url_path_append(paste0("vector_stores/", vector_store_id)) |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries, max_seconds = max_seconds, is_transient = is_transient_fn, backoff = backoff_fn, after = after_fn) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_method("DELETE") |> httr2::req_perform()
}
.delete_openai_assistant_api <- function(base_url, api_key, assistant_id, rpm, max_tries, max_seconds, is_transient_fn, backoff_fn, after_fn) {
  httr2::request(base_url) |> httr2::req_url_path_append(paste0("assistants/", assistant_id)) |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries, max_seconds = max_seconds, is_transient = is_transient_fn, backoff = backoff_fn, after = after_fn) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_method("DELETE") |> httr2::req_perform()
}