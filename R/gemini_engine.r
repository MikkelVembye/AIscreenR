###############################################################
# Function to send a single request to Gemini's API models
###############################################################

.gemini_engine <-
  function(
    body,
    RPM,
    timeinf,
    tokeninf,
    key,
    max_t,
    max_s,
    is_trans,
    back,
    aft,
    endpoint_url
  ){

    # Declarations are nested under tools -> functionDeclarations.
    detailed <- body$tools[[1]]$functionDeclarations[[1]]$name == "inclusion_decision"

    # Indicates how the detailed description variable is handled when the function
    # error and the detailed function is called vs not called.
    detail_desc <- if(detailed) NA_character_ else NULL

    # Max tries and gpt_is_transient not relevant if 'max_t = 0'
    if (max_t == 0) max_t <- is_trans <- NULL

    # Starting time
    tictoc::tic()

    # Creating the request
    req <-
      httr2::request(endpoint_url) |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        "Content-Type" = "application/json",
        # Gemini API expects x-goog-api-key instead of Bearer auth.
        "x-goog-api-key" = key
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

    # Check if internet connection is on
    if (curl::has_internet()){

      # Performing the request and getting status code back
      resp <- try(
        suppressMessages(req |> httr2::req_perform()),
        silent = TRUE
      )

      # If succeeded
      if (!inherits(resp, "try-error") && httr2::resp_status(resp) == 200){
        # Condition when HTTP response is 200

        # Parse Gemini response JSON.
        resp <- resp |> httr2::resp_body_json()

        # Gemini token metadata fields.
        prompt_tok <- resp$usageMetadata$promptTokenCount
        completion_tok <- resp$usageMetadata$candidatesTokenCount
        submod <- resp$modelVersion

        # Gemini function call arguments are returned under candidates -> content -> parts -> functionCall$args.
        function_call <- NULL
        if (length(resp$candidates) > 0 && !is.null(resp$candidates[[1]]$content$parts)) {
          parts <- resp$candidates[[1]]$content$parts
          function_calls <- Filter(
            function(part) {
              !is.null(part$functionCall) &&
                part$functionCall$name %in% c(
                  "inclusion_decision_simple",
                  "inclusion_decision",
                  "inclusion_decision_simple_binary",
                  "inclusion_decision_binary"
                )
            },
            parts
          )
          if (length(function_calls) > 0) {
            function_call <- function_calls[[1]]$functionCall
          }
        }

        tib_text <- try(
          tibble::as_tibble(function_call$args),
          silent = TRUE
        )

        # Code what returned data, when JSON format errors appear
        # In some instances the JOSN format returned from the server is flawed
        # This is tried to be handled here.
        if (inherits(tib_text, 'try-error')){
          # Condition if experiences JSON errors

          t_info <- if (timeinf) NA_real_ else NULL
          p_tokens <- c_tokens <- if (tokeninf) NA_real_ else NULL

          res <- tibble::tibble(
            decision_gpt = tib_text[1],
            decision_binary = NA_real_,
            detailed_description = detail_desc,
            prompt_tokens = p_tokens,
            completion_tokens = c_tokens,
            submodel = NA_character_
          )


        } else {
          # Condition when running as expected

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

        if (detailed){
          # Condition constructing data if detailed responses are called
          # This is done to ensure that function works if the function calls for
          # any reason do not work properly (i.e., returns 1 or 0)
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
        # Condition when HTTP response is NOT 200

        res <- tibble::tibble(
          decision_gpt = error_message(),
          decision_binary = NA_real_,
          detailed_description = detail_desc,
          prompt_tokens = NA_real_,
          completion_tokens = NA_real_,
          submodel = NA_character_
        )

      }

    } else {

      # Condition when not having access to internet

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

    # If the user do not what time and token info, this is removed here
    if (!timeinf) res <- res |> dplyr::select(-run_time)
    if (!tokeninf) res <- res |> dplyr::select(-c(prompt_tokens, completion_tokens))

    # Returning the response result
    res

  }

###############################################################
# Function to send repeated requests to Gemini API models
###############################################################

.rep_gemini_engine <- function(
    question, model_gpt, topp, iterations, req_per_min,
    role_gpt,
    tool,
    t_choice,
    seeds,
    time_inf,
    token_inf,
    apikey,
    maxt,
    maxs,
    istrans,
    ba,
    af,
    endpoint_url,
    reasoning_effort,
    ...
){

  # Gemini tool-choice mapping per functionCallingConfig modes:
  # - NONE: Prohibit function calls
  # - AUTO: Model decides between function call or natural language (default)
  # - ANY: Model always predicts a function call
  # - VALIDATED: (when combined with other tools; defaults here)
  tool_config <- NULL
  if (!is.null(t_choice)) {
    if (is.character(t_choice) && length(t_choice) == 1) {
      if (identical(t_choice, "none")) {
        tool_config <- list(functionCallingConfig = list(mode = "NONE"))
      } else if (identical(t_choice, "auto")) {
        # AUTO: model chooses between function call or natural language
        tool_config <- list(functionCallingConfig = list(mode = "AUTO"))
      } else if (t_choice %in% c("any", "required")) {
        # ANY: model always predicts a function call
        tool_config <- list(functionCallingConfig = list(mode = "ANY"))
      } else {
        # Specific function name: constrain to that function with ANY mode
        tool_config <- list(
          functionCallingConfig = list(
            mode = "ANY",
            allowedFunctionNames = list(t_choice)
          )
        )
      }
    } else if (is.list(t_choice) && !is.null(t_choice$`function`$name)) {
      # List with function spec: constrain to that function with ANY mode
      tool_config <- list(
        functionCallingConfig = list(
          mode = "ANY",
          allowedFunctionNames = list(t_choice$`function`$name)
        )
      )
    }
  }

  # Gemini body: contents + tools(functionDeclarations) + optional toolConfig/generationConfig.
  generation_config <- NULL
  if (!is.null(topp) || !is.null(reasoning_effort)) {
    thinking_config <- NULL
    if (!is.null(reasoning_effort)) {
      # Gemini 2.5 models use thinkingBudget; Gemini 3+ uses thinkingLevel.
      is_gemini_25 <- grepl("gemini-2\\.5", model_gpt, ignore.case = TRUE)

      if (is_gemini_25) {
        budget <- reasoning_effort

        if (is.character(reasoning_effort) && length(reasoning_effort) == 1) {
          # Minimal level-to-budget mapping for 2.5 series.
          budget <- dplyr::case_when(
            identical(reasoning_effort, "minimal") ~ 0, # Minimal effort corresponds to 0 thinking budget.
            identical(reasoning_effort, "low") ~ 1024, # Low effort corresponds to 1024 tokens.
            identical(reasoning_effort, "medium") ~ 8192, # Medium effort corresponds to 8192 tokens.
            identical(reasoning_effort, "high") ~ 24576, # High effort corresponds to 24576 tokens.
            TRUE ~ -1
          )
        }

        # 2.5 Pro cannot disable thinking, so we set to minimal to 128 instead.
        if (grepl("gemini-2\\.5-.*pro", model_gpt, ignore.case = TRUE) && identical(budget, 0)) {
          budget <- 128
        }

        thinking_config <- list(thinkingBudget = as.integer(budget))
      } else {
        # Gemini 3+ uses thinkingLevel with discrete values.
        thinking_config <- list(thinkingLevel = reasoning_effort)
      }
    }

    generation_config <- list(
      topP = topp,
      thinkingConfig = thinking_config
    )
  }

  body <- list(
    contents = list(
      list(
        role = role_gpt,
        parts = list(
          list(text = question)
        )
      )
    ),
    tools = if (is.null(tool)) NULL else list(list(functionDeclarations = tool)),
    toolConfig = tool_config,
    generationConfig = generation_config,
    ...
  )

  # Construct full Gemini API endpoint URL with model and :generateContent action
  full_endpoint_url <- paste0(endpoint_url, "/v1beta/models/", model_gpt, ":generateContent")

  # Setting the iterations
  if(iterations > 1) iterations <- 1:iterations

  furrr_seed <- if (base::is.null(seeds)) TRUE else NULL

  # Running repeated in requests in parallel, and return tibble
  suppressWarnings(
    furrr::future_map_dfr(
      iterations, \(i) .gemini_engine(
        body = body,
        RPM = req_per_min,
        timeinf = time_inf,
        tokeninf = token_inf,
        key = apikey,
        max_t = maxt,
        max_s = maxs,
        is_trans = istrans,
        back = ba,
        aft = af,
        endpoint_url = full_endpoint_url
      ),
      .options = furrr::furrr_options(seed = furrr_seed)
    )
  ) |>
    dplyr::mutate(n = iterations) # Given run id to each repetation.

}