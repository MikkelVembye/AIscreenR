# Internal function to process a single screening run. This is called by future_pmap.
.process_single_ft_run <- function(current_file_path, current_prompt, current_model, current_rep_num, current_study_id, current_prompt_id, current_vector_store_name, # Args from .l
                                   base_url, api_key, top_p, temperature,
                                   decision_description, assistant_name, assistant_description,
                                   assistant_instructions, max_tries, tools_simple, tools_detailed, tools_supplementary, rpm,
                                   sleep_time_val, protocol_file_path,
                                   # Retry parameters
                                   max_seconds_val, is_transient_fn_val, backoff_val, after_val) {  
  
  start_time_single_run <- Sys.time()
  
  result_single_run <- list(
    decision_gpt = NULL, detailed_description = NULL, supplementary = NULL, status = NULL,
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
    assistant <- .get_or_create_openai_assistant(base_url, api_key, paste(assistant_name, current_model, sep="_"), assistant_description, assistant_instructions, current_model, decision_description, tools_simple, tools_detailed, tools_supplementary, temperature, top_p, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val)
    assistant_id <- assistant$id
    .update_openai_assistant_vector_store(base_url, api_key, assistant_id, vector_store_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val)  

    # Create and run thread
    thread_resp <- .create_openai_thread(base_url, api_key, current_prompt, file_id, vector_store_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val, is_protocol = !is.null(protocol_file_path))
    thread_id <- thread_resp$id
    run_resp <- .run_openai_thread(base_url, api_key, thread_id, assistant_id, rpm, max_tries, max_seconds_val, is_transient_fn_val, backoff_val, after_val, decision_description)  
    run_id <- run_resp$id
    
    # Wait for completion
    completion_result <- .wait_for_run_completion(base_url, api_key, thread_id, run_id, run_resp, max_tries, sleep_time_val, max_seconds_val, is_transient_fn_val, backoff_val, after_val, rpm, decision_description)
    
    # Update results from completion
    result_single_run <- utils::modifyList(result_single_run, completion_result$result_updates)
    result_single_run$status <- completion_result$status
    
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
.create_openai_vector_store <- function(base_url, api_key, vector_stores_name, rpm, max_tries, max_seconds, is_transient_fn, backoff_fn, after_fn) {
  httr2::request(base_url) |> httr2::req_url_path_append("vector_stores") |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries, max_seconds = max_seconds, is_transient = is_transient_fn, backoff = backoff_fn, after = after_fn) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_body_json(list(name = vector_stores_name)) |> httr2::req_perform() |> httr2::resp_body_json()
}
.upload_openai_file <- function(base_url, api_key, file_path, rpm, max_tries, max_seconds, is_transient_fn, backoff_fn, after_fn) {
  httr2::request(base_url) |> httr2::req_url_path_append("files") |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries, max_seconds = max_seconds, is_transient = is_transient_fn, backoff = backoff_fn, after = after_fn) |> httr2::req_headers("Content-Type" = "multipart/form-data") |> httr2::req_body_multipart(purpose = "assistants", file = curl::form_file(file_path)) |> httr2::req_perform() |> httr2::resp_body_json()
}
.add_file_to_openai_vector_store <- function(base_url, api_key, vector_store_id, file_id, rpm, max_tries, max_seconds, is_transient_fn, backoff_fn, after_fn) {
  httr2::request(base_url) |> httr2::req_url_path_append(paste0("vector_stores/", vector_store_id, "/files")) |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries, max_seconds = max_seconds, is_transient = is_transient_fn, backoff = backoff_fn, after = after_fn) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_body_json(list(file_id = file_id)) |> httr2::req_perform()
}
.get_or_create_openai_assistant <- function(base_url, api_key, assistant_name, assistant_description, assistant_instructions, model, decision_description, tools_simple, tools_detailed, tools_supplementary, temperature, top_p, rpm, max_tries_val, max_seconds_val, is_transient_fn_val, backoff_val, after_val) {
  assistants_resp <- httr2::request(base_url) |> httr2::req_url_path_append("assistants") |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries_val, max_seconds = max_seconds_val, is_transient = is_transient_fn_val, backoff = backoff_val, after = after_val) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_perform() |> httr2::resp_body_json()
  existing_assistant <- purrr::keep(assistants_resp$data, ~ .x$name == assistant_name)
  if (length(existing_assistant) > 0) return(existing_assistant[[1]])
  
  assistant_tools <- if (decision_description) c(list(list(type = "file_search")), tools_supplementary, tools_detailed) else c(list(list(type = "file_search")), tools_supplementary, tools_simple)
  
  httr2::request(base_url) |> httr2::req_url_path_append("assistants") |> httr2::req_auth_bearer_token(token = api_key) |> httr2::req_throttle(rpm/60) |> httr2::req_retry(max_tries = max_tries_val, max_seconds = max_seconds_val, is_transient = is_transient_fn_val, backoff = backoff_val, after = after_val) |> httr2::req_headers("Content-Type" = "application/json", "OpenAI-Beta" = "assistants=v2") |> httr2::req_body_json(list(model = model, name = assistant_name, description = assistant_description, instructions = assistant_instructions, tools = assistant_tools, temperature = temperature, top_p = top_p, response_format = "auto")) |> httr2::req_perform() |> httr2::resp_body_json()
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