#' Full-text screening with OpenAI API models
#'
#' @description
#' This function supports the conduct of full-text screening with OpenAI API models in R.
#' It uses the OpenAI Assistants API to process local documents (e.g., PDFs, text files).
#' The function allows you to run screening across multiple prompts and with
#' repeated questions to check for consistency across answers. It uses native function calling
#' to structure the model's output.
#'
#' @param file_path A character vector of file paths or directories to be screened. Directories will be processed recursively, with files in sub-directories being combined into a single document for screening.
#' @param prompt A character string containing the screening prompt. Required if `protocol_file_path` is not provided.
#' @param protocol_file_path Path to a file (.txt, .md, .pdf, .docx) containing the screening protocol.
#' @param api_key Your OpenAI API key. Defaults to `get_api_key()`.
#' @param vector_stores_name A name for the OpenAI vector store to be created for the screening session.
#' @param model Character string with the name of the completion model. Can take
#'   multiple OpenAI models. Default = `"gpt-4o-mini"`.
#'   Find available models at \url{https://platform.openai.com/docs/models}.
#' @param top_p An alternative to sampling with temperature, called nucleus sampling,
#'   where the model considers the results of the tokens with top_p probability mass.
#'   So 0.1 means only the tokens comprising the top 10% probability mass are considered.
#'   OpenAI recommends altering this or temperature but not both. Default is 1.
#' @param temperature Controls randomness: lowering results in less random completions.
#'   As the temperature approaches zero, the model will become deterministic and repetitive. Default is 0.7.
#' @param decision_description Logical indicating whether to include detailed descriptions
#'   of decisions. Default is `FALSE`.
#' @param assistant_name,assistant_description,assistant_instructions Configuration for the OpenAI assistant.
#' @param messages Logical indicating whether to print messages embedded in the function.
#'   Default is `TRUE`.
#' @param reps Numerical value indicating the number of times the same
#'   question should be sent to OpenAI's API models. This can be useful to test consistency
#'   between answers. Default is `1`.
#' @param max_tries,max_seconds Cap the maximum number of attempts with
#'  `max_tries` or the total elapsed time from the first request with
#'  `max_seconds`. If neither option is supplied, it will not retry.
#' @param is_transient A predicate function that takes a single argument
#'  (the response) and returns `TRUE` or `FALSE` specifying whether or not
#'  the response represents a transient error.
#' @param backoff A function that takes a single argument (the number of failed
#'   attempts so far) and returns the number of seconds to wait.
#' @param after A function that takes a single argument (the response) and
#'   returns either a number of seconds to wait or `NULL`, which indicates
#'   that the `backoff` strategy should be used instead.
#' @param time_info Logical indicating whether the run time of each
#'   request/question should be included in the data. Default = `TRUE`.
#' @param token_info Logical indicating whether the number of prompt and completion tokens
#'   per request should be included in the output data. Default = `TRUE`.
#' @param rpm Numerical value indicating the number of requests per minute (rpm)
#'   available for the specified api key.
#' @param seed_par Numerical value for a seed to ensure that proper,
#'   parallel-safe random numbers are produced.
#' @param progress Logical indicating whether a progress bar should be shown when running
#'   the screening in parallel. Default is `TRUE`.
#' @param incl_cutoff_upper Numerical value indicating the probability threshold
#'   for which a study should be included. Default is 0.5.
#' @param incl_cutoff_lower Numerical value indicating the probability threshold
#'   above which studies should be checked by a human. Default is 0.4.
#' @param force Logical argument indicating whether to force the function to use more than
#'   10 iterations or certain models. Default is `FALSE`.
#' @param sleep_time Time in seconds to wait between checking run status. Default is 8.
#' @param ... Further arguments to pass to the request body.
#'
#' @return An object of class `gpt_ftscreen`. The object is a list containing the following
#' components:
#' \item{answer_data}{A data frame with all individual answers from the model.}
#' \item{answer_data_aggregated}{A data frame with the summarized, probabilistic inclusion decision for each file across multiple repeated questions (only when reps > 1).}
#' \item{error_data}{A data frame with failed requests (only included if errors occurred).}
#' \item{run_date}{The date when the screening was conducted.}
#' \item{n_files, n_prompts, n_models, n_combinations, n_runs}{Counts of inputs and processing runs.}
#'
#' @note The `answer_data_aggregated` data (only present when reps > 1) contains the following variables:
#' \tabular{lll}{
#'  \bold{title} \tab \code{character} \tab The filename of the screened document. \cr
#'  \bold{model} \tab \code{character} \tab The specific model used. \cr
#'  \bold{promptid} \tab \code{integer} \tab The prompt ID. \cr
#'  \bold{prompt} \tab \code{character} \tab The prompt used for screening. \cr
#'  \bold{incl_p} \tab \code{numeric} \tab The probability of inclusion across repeated responses. \cr
#'  \bold{final_decision_gpt} \tab \code{character} \tab The final decision: 'Include', 'Exclude', or 'Check'. \cr
#'  \bold{final_decision_gpt_num} \tab \code{integer} \tab The final numeric decision: 1 for include/check, 0 for exclude. \cr
#'  \bold{reps} \tab \code{integer} \tab The number of repetitions for the question. \cr
#'  \bold{n_mis_answers} \tab \code{integer} \tab The number of missing responses. \cr
#'  \bold{supplementary} \tab \code{character} \tab Indicates if supplementary material was detected ('yes'/'no'). \cr
#'  \bold{longest_answer} \tab \code{character} \tab The longest detailed description from responses (if `decision_description = TRUE`). \cr
#' }
#' <br>
#' The `answer_data` data contains the following variables:
#' \tabular{lll}{
#'  \bold{studyid} \tab \code{integer} \tab The study ID of the file. \cr
#'  \bold{title} \tab \code{character} \tab The filename of the screened document. \cr
#'  \bold{promptid} \tab \code{integer} \tab The prompt ID. \cr
#'  \bold{prompt} \tab \code{character} \tab The prompt used for screening. \cr
#'  \bold{model} \tab \code{character} \tab The specific model used. \tr
#'  \bold{iterations} \tab \code{numeric} \tab The repetition number for the question. \cr
#'  \bold{decision_gpt} \tab \code{character} \tab The raw decision from the model ('1', '0', or '1.1'). \cr
#'  \bold{detailed_description} \tab \code{character} \tab A detailed description of the decision (if `decision_description = TRUE`). \cr
#'  \bold{supplementary} \tab \code{character} \tab Indicates if supplementary material was detected ('yes'/'no'). \cr
#'  \bold{decision_binary} \tab \code{integer} \tab The binary decision (1 for inclusion/uncertainty, 0 for exclusion). \cr
#'  \bold{run_time} \tab \code{numeric} \tab The time taken for the request. \cr
#'  \bold{prompt_tokens} \tab \code{integer} \tab The number of prompt tokens used. \cr
#'  \bold{completion_tokens} \tab \code{integer} \tab The number of completion tokens used. \cr
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' set_api_key()
#' 
#' file_path <- "path/to/your/full_text_files"
#' protocol_file <- "path/to/your/protocol_file.txt"
#' 
#' # --- Run screening using the protocol file ---
#' result_protocol <- ftscreen(
#'   file_path = file_path,
#'   protocol_file_path = protocol_file,
#'   vector_stores_name = "TestFTScreenProtocol",
#'   model = "gpt-4o-mini",
#'   decision_description = TRUE,
#'   reps = 1,
#'   assistant_instructions = "
#'   You are a helpful agent that reviews files to determine their relevance based on specific protocols.
#'
#'   CRITICAL: You must follow this EXACT two-step process:
#'
#'   STEP 1: SUPPLEMENTARY CHECK ONLY
#'   - Use the supplementary_check function to identify if the text contains references to
#'     supplementary materials, appendices, or additional information.
#'   - This step is ONLY about identifying supplementary content - do NOT make any inclusion decisions here.
#'
#'   STEP 2: PROTOCOL EVALUATION AND INCLUSION DECISION
#'   - Carefully review the provided protocol criteria.
#'   - Evaluate the study against ALL relevant criteria in the protocol.
#'   - Base your decision solely on whether the study meets the protocol criteria.
#'
#'   IMPORTANT REMINDERS:
#'   - The supplementary check is separate from the inclusion decision.
#'   - If the study meets the protocol criteria, then it should be included in further studies.
#'   - Be explicit about whether the study should be included or excluded based on the protocol evaluation.
#'   - Provide detailed reasoning for your decision when detailed_description is enabled"
#' )
#'
#' print(result_protocol$answer_data)
#'
#'
#' # --- Run screening using traditional prompts ---
#' 
#' prompts <- c("
#' Does this study focus on an intervention aimed at improving children's language, 
#' reading/literacy, or mathematical skills?
#' ",
#' "Is this study written in English?",
#' "Does this study involve children aged 3 to 4 years old?"
#' )
#' 
#' result_prompts <- ftscreen(
#'   file_path = file_path,
#'   prompt = prompts,
#'   vector_stores_name = "TestFTScreenPrompts",
#'   model = "gpt-4o-mini",
#'   decision_description = TRUE,
#'   reps = 1
#' )
#'
#' print(result_prompts$answer_data)
#' }


ftscreen <- function(
    file_path,
    prompt = NULL,
    protocol_file_path = NULL,
    api_key = get_api_key(),
    vector_stores_name,
    model = "gpt-4o-mini",
    top_p = 1,
    temperature = 0.7,
    decision_description = FALSE,
    assistant_name = "file assistant screening",
    assistant_description = "An assistant to review a file and decide if it should be included or excluded in further studies based on a protocol or prompt.",
    assistant_instructions = "You are an assistant that helps in reviewing files to determine their relevance based on specific protocols or prompts. IMPORTANT: You must ALWAYS follow this exact sequence: 1. FIRST, check if the text contains any references to supplementary materials, appendices, or additional information using the supplementary_check function. 2. THEN, determine whether the study should be included or excluded based on the protocol or prompt provided. Be explicit about whether the study should be included or excluded in further studies based on the evaluation criteria. If the study meets the protocol criteria, then it should be included in further studies.",
    messages = TRUE,
    reps = 1,
    max_tries = 16,
    time_info = TRUE,
    token_info = TRUE,
    max_seconds = NULL,
    is_transient = gpt_is_transient,
    backoff = NULL,
    after = NULL,
    rpm = 10000,
    seed_par = NULL,
    progress = TRUE,
    incl_cutoff_upper = 0.5,
    incl_cutoff_lower = 0.4,
    force = FALSE,
    sleep_time = 8,
    ...
) {
  # Validate protocol_file_path and prompt parameters
  if (is.null(prompt) && is.null(protocol_file_path)) {
    stop("Error: Either 'prompt' or 'protocol_file_path' must be provided.")
  }
  
  if (!is.null(prompt) && !is.null(protocol_file_path)) {
    stop("Error: Please provide either 'prompt' or 'protocol_file_path', not both.")
  }
  
  # Read protocol file if provided
  if (!is.null(protocol_file_path)) {
    prompt <- .read_protocol_file(protocol_file_path)
  }

  # Process file paths and get temporary files list
  file_processing_result <- .process_file_paths(file_path)
  file_path <- file_processing_result$file_paths
  temp_files_created <- file_processing_result$temp_files
  
  # Set up cleanup on exit
  on.exit(.cleanup_temp_files(temp_files_created), add = TRUE)

  arg_list <- list(
    file_path = file_path,
    prompt = prompt,
    protocol_file_path = protocol_file_path,
    vector_stores_name = vector_stores_name,
    model = model,
    top_p = top_p,
    temperature = temperature,
    decision_description = decision_description,
    assistant_name = assistant_name,
    assistant_description = assistant_description,
    assistant_instructions = assistant_instructions,
    messages = messages,
    reps = reps,
    max_tries = max_tries,
    time_info = time_info,
    token_info = token_info,
    max_seconds = max_seconds,
    is_transient = is_transient,
    backoff = backoff,
    after = after,
    rpm = rpm,
    seed_par = seed_par,
    progress = progress,
    incl_cutoff_upper = incl_cutoff_upper,
    incl_cutoff_lower = incl_cutoff_lower,
    force = force,
    sleep_time = sleep_time,
    ...
  )

  # Argument and Parameter Checks
  .validate_ftscreen_args(arg_list)

  # Startup messages
  if (messages){
    price_estimate <- .price_ftscreen(
      file_paths = file_path,
      prompts = prompt,
      models = model,
      reps = reps,
      decision_description = decision_description
    )
    message(paste0("* The estimated cost for this screening is: $", price_estimate))
  }
  if (decision_description && messages){ 
    message(paste0("* Be aware that getting descriptive, detailed responses will substantially increase the price of the screening."))
  }
    
  # API Call Preparation
  base_url <- "https://api.openai.com/v1"
  actual_is_transient_for_retry <- if (is.function(is_transient)) is_transient else if (is.logical(is_transient) && is_transient) gpt_is_transient else NULL

  # Prepare tasks for furrr::future_pmap.
  combinations <- expand.grid(file_path = file_path, prompt = prompt, model = model, stringsAsFactors = FALSE) |>
    dplyr::mutate(
      file_idx = as.numeric(factor(file_path, levels = unique(file_path))),
      prompt_idx = as.numeric(factor(prompt, levels = unique(prompt)))
    )

  pmap_params <- expand.grid(combination_idx = seq_len(nrow(combinations)), rep_num = 1:reps, stringsAsFactors = FALSE) |>
    dplyr::mutate(
      current_file_path = combinations$file_path[combination_idx],
      current_prompt = combinations$prompt[combination_idx],
      current_model = combinations$model[combination_idx],
      current_study_id = combinations$file_idx[combination_idx],
      current_prompt_id = combinations$prompt_idx[combination_idx],
      current_rep_num = rep_num,
      safe_basename = gsub("[^a-zA-Z0-9_.-]", "_", basename(current_file_path)),
      current_vector_store_name = paste0(vector_stores_name, "_", safe_basename, "_comb", combination_idx, "_rep", rep_num)
    ) |>
    dplyr::select(current_file_path, current_prompt, current_model, current_rep_num, current_study_id, current_prompt_id, current_vector_store_name)

  # Main Processing Loop (future_pmap)
  results_list <- furrr::future_pmap(
    .l = pmap_params,
    .f = .process_single_ft_run,
    # Pass fixed arguments to the worker function .f
    base_url = base_url, api_key = api_key, top_p = top_p, temperature = temperature,
    decision_description = decision_description, assistant_name = assistant_name,
    assistant_description = assistant_description, assistant_instructions = assistant_instructions,
    max_tries = max_tries, tools_simple = tools_simple, tools_detailed = tools_detailed,
    tools_supplementary = tools_supplementary, rpm = rpm, sleep_time_val = sleep_time,
    protocol_file_path = protocol_file_path,
    # Retry parameters
    max_seconds_val = max_seconds, is_transient_fn_val = actual_is_transient_for_retry,
    backoff_val = backoff, after_val = after,
    .options = furrr::furrr_options(seed = if(is.null(seed_par)) TRUE else seed_par),
    .progress = progress
  )

  # Result Combination and Error Handling
  combined_answer_data <- do.call(rbind, results_list)

  if (!time_info && "run_time" %in% names(combined_answer_data)) combined_answer_data$run_time <- NULL
  if (!token_info) {
    if ("prompt_tokens" %in% names(combined_answer_data)) combined_answer_data$prompt_tokens <- NULL
    if ("completion_tokens" %in% names(combined_answer_data)) combined_answer_data$completion_tokens <- NULL
  }

  n_error <- if (!is.null(combined_answer_data) && nrow(combined_answer_data) > 0 && "decision_binary" %in% names(combined_answer_data)) {
    sum(is.na(combined_answer_data$decision_binary))
  } else { 0 }

  if (messages && n_error > 0) {
    message(paste("* NOTE: Requests failed for", n_error, "file processing run(s) (decision_binary is NA)."))
  }
  
  error_data_df <- if (n_error > 0 && !is.null(combined_answer_data)) combined_answer_data[is.na(combined_answer_data$decision_binary), ] else NULL

  # Aggregated data
  answer_dat_aggregated <- if (any(reps > 1) || length(prompt) > 1 || length(model) > 1) {
    .aggregate_ft_res(combined_answer_data, incl_cutoff_u = incl_cutoff_upper, incl_cutoff_l = incl_cutoff_lower)
  } else { NULL }

  # Return Object Construction
  return_obj <- list(
    answer_data = combined_answer_data,
    answer_data_aggregated = answer_dat_aggregated,
    error_data = error_data_df,
    run_date = Sys.Date(),
    n_files = length(file_path),
    n_prompts = length(prompt),
    n_models = length(model),
    n_combinations = nrow(combinations),
    n_runs = nrow(pmap_params),
    price_dollar = NULL, # Placeholder
    price_data = NULL    # Placeholder
  )
  
  attr(return_obj, "arg_list") <- arg_list
  if (reps > 1) {
    attr(return_obj, "incl_cutoff_upper") <- incl_cutoff_upper
    attr(return_obj, "incl_cutoff_lower") <- incl_cutoff_lower
  }
  class(return_obj) <- c("gpt_ftscreen", "gpt", class(return_obj))
  return(return_obj)
}