###############################################################
# Function to send a single request to OpenAI's GPT API models
###############################################################

gpt_engine <-
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
    aft
){

  # Logical argument indicating whether detailed screening description is
  # requested
  detailed <- body$tools[[1]]$`function`$name == "inclusion_decision"

  # Indicates how the detailed description variable is handled when the function
  # error and the detailed function is called vs not called.
  detail_desc <- if(detailed) NA_character_ else NULL

  # Move out of this function
  #if (RPM > 10000) RPM <- 10000

  # Max tries and gpt_is_transient not relevant if 'max_t = 0'
  if (max_t == 0) max_t <- is_trans <- NULL

  # Starting time
  tictoc::tic()

  # Request url (might possibly be made more flexible in future applications)
  url <- "https://api.openai.com/v1/chat/completions"

  # Creating the request
  req <-
    httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", key) # The key is generated via AIscreenR::get_api_key()
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
    if (status_code() == 200){
      # Condition when HTTP response is 200

      # Getting back the response json body (consider try if user experience troubles here)
      resp <- resp |> httr2::resp_body_json()

      # Extracting information from the json body/list
      resp_text <- resp$choices[[1]]$message$tool_calls[[1]]$`function`$arguments
      prompt_tok <- resp$usage$prompt_tokens
      completion_tok <- resp$usage$completion_tokens
      submod <- resp$model

      tib_text <- try(
        tibble::as_tibble(jsonlite::fromJSON(resp_text)),
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
              is.na(decision_binary), "Error: Something went wrong [Try again]", detailed_description,
              missing = "Error: Something went wrong [Try again]"
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

################################################################
# Function to send repeated requests to OpenAI's GPT API models
################################################################

rep_gpt_engine <- function(
    question,
    model_gpt,
    topp,
    role_gpt,
    tool,
    t_choice,
    iterations,
    req_per_min,
    seeds,
    time_inf,
    token_inf,
    apikey,
    maxt,
    maxs,
    istrans,
    ba,
    af,
    ...
){

  # Setting tool_choice argument to body
  tools_choice <- list(
    type = "function",
    "function" = list(
      name = t_choice
    )
  )


  body <- list(
    model = model_gpt,
    messages = list(
      list(
        role = role_gpt,
        content = question
      )
    ),
    tools = tool,
    tool_choice = tools_choice,
    top_p = topp,
    ...
  )

  if(iterations > 1) iterations <- 1:iterations

  furrr_seed <- if (base::is.null(seeds)) TRUE else NULL


  furrr::future_map_dfr(
    iterations, \(i) gpt_engine(
      body = body,
      RPM = req_per_min,
      timeinf = time_inf,
      tokeninf = token_inf,
      key = apikey,
      max_t = maxt,
      max_s = maxs,
      is_trans = istrans,
      back = ba,
      aft = af
      ),
    .options = furrr::furrr_options(seed = furrr_seed)
  ) |>
  dplyr::mutate(n = iterations)


}

# Main function
tabscreen2_gpt.tools <- function(
    #data,
    #prompt,
    #studyid,
    #title,
    #abstract,
    #arrange_var = studyid,
    model = "gpt-4o",
    #role = "user",
    #tools = tools_simple,
    #tool_choice = "inclusion_decision_simple",
    #top_p = 1,
    #time_info = TRUE,
    #token_info = TRUE,
    #api_key = get_api_key(),
    #max_tries = 16,
    #max_seconds = NULL,
    #is_transient = gpt_is_transient,
    #backoff = NULL,
    #after = NULL,
    #rpm = 10000,
    #reps = 1,
    #seed = NULL,
    #progress = TRUE,
    #messages = TRUE,
    #incl_cutoff_upper = 0.5,
    #incl_cutoff_lower = incl_cutoff_upper - 0.1,
    #force = FALSE,
    fine_tuned = FALSE,
    ...
){

  # Stop if wrong model are called
  if (!fine_tuned){
    if(!model %in% model_prizes$model) stop("Unknown gpt model(s) used - check model name(s).")
  }


  # Stop warnings
  # Ensuring that users do not conduct wrong screening
  if (max(reps) > 10 && !force){
    max_reps_mes <- paste("* Are you sure you want to use", max(reps), "iterations? If so, set force = TRUE")
    stop(max_reps_mes)
  }
#
#
#  if (any(stringr::str_detect(model, "gpt-4")) && max(reps) > 1 && !force){
#
#    gpt4_reps <- tibble::tibble(model, reps) |> filter(stringr::str_detect(model, "gpt-4")) |> pull(reps) |> max()
#
#    if (gpt4_reps > 1){
#
#      max_reps_mes_gpt4 <-
#        paste("* Are you sure you want to use", gpt4_reps, "iterations with gpt-4?",
#              "If so, set force = TRUE")
#      stop(max_reps_mes_gpt4)
#
#    }
#
#  }
#
#  if (length(rpm) > 1 && length(model) != length(rpm)){
#    stop("model and rpm must be of the same length.")
#  }
#
#  if (length(reps) > 1 && length(model) != length(reps)){
#    stop("model and reps must be of the same length.")
#  }
#
#
#  #if (any(!is.element(model, c(
#  #  "gpt-3.5-turbo", "gpt-3.5-turbo-0613", "gpt-3.5-turbo-1106",
#  #  "gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613",
#  #  "gpt-4", "gpt-4-0613",
#  #  "gpt-4-32k", "gpt-4-32k-0613", "gpt-4-1106-preview", "gpt-4-vision-preview"
#  #)))) stop("Unknown gpt model(s) used - check model name(s).")
#
#
#  if(incl_cutoff_upper < incl_cutoff_lower){
#    stop("incl_cutoff_lower must not exceed incl_cutoff_upper")
#  }
#
#  if (!missing(prompt)){
#    if (n_distinct(prompt) != length(prompt)) stop("Do not add the same prompt twice.")
#  }
#
#  if (n_distinct(reps) == 1 && n_distinct(model) != length(model)){
#    model <- unique(model)
#  }
#
#  # Collecting all arguments to be used in the screen_errors function
#  arg_list <-
#    list(
#      role = role,
#      tools = tools,
#      tool_choice = tool_choice,
#      time_info = time_info,
#      token_info = token_info,
#      max_tries = max_tries,
#      max_seconds = max_seconds,
#      is_transient = is_transient,
#      backoff = backoff,
#      after = after,
#      seed = seed,
#      progress = progress,
#      messages = messages,
#      incl_cutoff_upper = incl_cutoff_upper,
#      incl_cutoff_lower = incl_cutoff_lower,
#      ...
#    )
#
#  if ("max_tokens" %in% names(arg_list)){
#    if (arg_list$max_tokens < 9) stop("Cannot retrieve results from server with tokens below 11.")
#  }
#
#
#  ###############################################
#  # Data manipulation
#  ###############################################
#
#
#
#    if (missing(studyid)){
#
#      dat <-
#        data |>
#        dplyr::mutate(
#          studyid = 1:nrow(data)
#        ) |>
#        dplyr::relocate(studyid, .before = {{ title }}) |>
#        dplyr::relocate({{ abstract }}, .after = {{ title }}) |>
#        dplyr::relocate(c(studyid, {{ title }}, {{ abstract }}), .after = last_col())
#
#
#    } else {
#
#      dat <-
#        data |>
#        dplyr::mutate(
#          studyid = {{ studyid }}
#        ) |>
#        dplyr::relocate(studyid, .before = {{ title }}) |>
#        dplyr::relocate({{ abstract }}, .after = {{ title }}) |>
#        dplyr::relocate(c(studyid, {{ title }}, {{ abstract }}), .after = last_col())
#
#    }
#
#
#    mp_reps <- if (length(reps) > 1) 1 else length(model)
#    mp_rpm <- if (length(rpm) > 1) 1 else length(model)
#
#    model_length <- length(model)
#    prompt_length <- length(prompt)
#    studyid_length <- dplyr::n_distinct(dat$studyid)
#
#    question_dat <-
#      dat |>
#      dplyr::mutate(
#        dplyr::across(c({{ title }}, {{ abstract }}), ~ dplyr::if_else(
#          is.na(.x) | .x == "" | .x == " " | .x == "NA", "No information", .x, missing = "No information")
#        )
#      ) |>
#      dplyr::slice(rep(1:nrow(dat), prompt_length)) |>
#      dplyr::mutate(
#        promptid = rep(1:prompt_length, each = studyid_length),
#        prompt = rep(prompt, each = studyid_length)
#      ) |>
#      dplyr::slice(rep(1:dplyr::n(), each = model_length)) |>
#      dplyr::mutate(
#        model = rep(model, studyid_length*prompt_length),
#        iterations = rep(reps, studyid_length*prompt_length*mp_reps),
#        req_per_min = rep(rpm, studyid_length*prompt_length*mp_rpm),
#        question_raw = paste0(
#          prompt,
#          " Now, evaluate the following title and abstract for",
#          " Study ", studyid, ":",
#          " -Title: ", {{ title }},
#          " -Abstract: ", {{ abstract }}
#        ),
#        question = stringr::str_replace_all(question_raw, "\n\n", " "),
#        question = stringr::str_remove_all(question, "\n")
#      ) |>
#      dplyr::select(-question_raw) |>
#      dplyr::slice(rep(1:dplyr::n(), each = length(top_p))) |>
#      mutate(
#        topp = rep(top_p, studyid_length*prompt_length*model_length)
#      ) |>
#      dplyr::arrange(promptid, model, topp, iterations, studyid)
#
#    # For checks of whether multiple reps are used with gpt4 models
#    if (any(stringr::str_detect(model, "gpt-4"))){
#      max_reps_gpt4 <-
#        question_dat |>
#        filter(stringr::str_detect(model, "gpt-4")) |>
#        summarise(
#          max_reps = max(iterations, na.rm = TRUE)
#        ) |>
#        pull(max_reps)
#    }
#
#    # Approximate prize
#    app_price_dat <-
#      question_dat |>
#      mutate(
#        prompt_tokens = round(stringr::str_count(question, '\\w+') * 1.6),
#        completion_tokens = 11 # Average number of completion tokens for the inclusion_decision_simple function
#      ) |>
#      filter(!is.na(prompt_tokens) | !is.na(completion_tokens)) |>
#      dplyr::rowwise() |>
#      mutate(
#
#        input_price = case_when(
#          any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ round(prompt_tokens * (0.0015/1000) * iterations, 4),
#          any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ round(prompt_tokens * (0.003/1000) * iterations, 4),
#          any(c("gpt-4", "gpt-4-0613") %in% model) ~ round(prompt_tokens * (0.03/1000) * iterations, 4),
#          any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ round(prompt_tokens * (0.06/1000) * iterations, 4),
#          TRUE ~ NA_real_
#        ),
#
#        output_price = case_when(
#          any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ completion_tokens * (0.002/1000) * iterations,
#          any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ completion_tokens * (0.004/1000) * iterations,
#          any(c("gpt-4", "gpt-4-0613") %in% model) ~ completion_tokens * (0.06/1000) * iterations,
#          any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ completion_tokens * (0.12/1000) * iterations,
#          TRUE ~ NA_real_
#        )
#
#      ) |>
#      ungroup() |>
#      summarise(
#
#        iterations = unique(iterations),
#        input_price_dollar = sum(input_price, na.rm = TRUE),
#        output_price_dollar = sum(output_price, na.rm = TRUE),
#        total_price_dollar = round(input_price_dollar + output_price_dollar, 4),
#
#        .by = c(model, iterations)
#
#      )
#
#    app_price <- sum(app_price_dat$total_price_dollar, na.rm = TRUE)
#
#
#    if (messages){
#
#      message(paste0("* The approximate price of the current (simple) screening will be around $", app_price, "."))
#
#      if (tools[[1]]$`function`$name == "inclusion_decision"){
#        message(
#          paste0(
#            "* Be aware that getting detailed reponses from ChatGPT ",
#            "will substantially increase the prize of the screening relative to the noted approximate prize."
#          )
#        )
#      }
#
#
#      if ("No information" %in% unique(question_dat$abstract)) {
#        message(
#          paste0(
#            "* Consider removing references that has no abstract ",
#            "since these can distort the accuracy of the screening"
#          )
#        )
#      }
#    }
#
#
#    # RUNNING QUESTIONS
#    furrr_seed <- if (is.null(seed)) TRUE else NULL
#
#    params <- question_dat |>
#      dplyr::select(question, model_gpt = model, topp, iterations, req_per_min)
#
#    answer_dat <-
#      question_dat |>
#      dplyr::mutate(
#        res = furrr::future_pmap(
#          .l = params,
#          .f = ask_gpt,
#          ...,
#          .options = furrr::furrr_options(seed = furrr_seed),
#          .progress = progress
#        )
#      ) |>
#      tidyr::unnest(res) |>
#      tibble::new_tibble(class = "chatgpt_tbl")
#
#    n_error <- answer_dat |> dplyr::filter(is.na(decision_binary)) |> nrow()
#
#    if (messages){
#
#      if (n_error == 1) message(paste("* NOTE: Requests failed for 1 title and abstract."))
#      if (n_error > 1) message(paste("* NOTE: Requests failed", n_error, "times."))
#
#    }
#
#    if (n_error > 0) error_refs <- answer_dat |> dplyr::filter(is.na(decision_binary))
#
#
#
#
#  #answer_dat <- tibble::new_tibble(answer_dat, class = "chatgpt_tbl")
#
#  if (token_info){
#
#    price_dat <-
#      answer_dat |>
#      filter(!is.na(prompt_tokens) | !is.na(completion_tokens)) |>
#      summarise(
#
#        input_price_dollar = case_when(
#          any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.0015/1000), 4),
#          any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.003/1000), 4),
#          any(c("gpt-4", "gpt-4-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.03/1000), 4),
#          any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.06/1000), 4),
#          TRUE ~ NA_real_
#        ),
#
#        output_price_dollar = case_when(
#          any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.002/1000),
#          any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.004/1000),
#          any(c("gpt-4", "gpt-4-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.06/1000),
#          any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.12/1000),
#          TRUE ~ NA_real_
#        ),
#
#        price_total_dollar = round(input_price_dollar + output_price_dollar, 4),
#
#        .by = c(model, iterations)
#
#      )
#
#    price <- sum(price_dat$price_total_dollar, na.rm = TRUE)
#
#  }
#
#
#  sum_dat <-
#    answer_dat |>
#    summarise(
#
#      incl_p = mean(decision_binary == 1, na.rm = TRUE),
#
#      final_decision_gpt = dplyr::case_when(
#        incl_p < incl_cutoff_upper & incl_p >= incl_cutoff_lower ~ "Check",
#        incl_p >= incl_cutoff_upper ~ "Include",
#        incl_p < incl_cutoff_lower ~ "Exclude",
#        TRUE ~ NA_character_
#      ),
#
#      final_decision_gpt_num = dplyr::case_when(
#        incl_p < incl_cutoff_upper & incl_p >= incl_cutoff_lower ~ 1,
#        incl_p >= incl_cutoff_upper ~ 1,
#        incl_p < incl_cutoff_lower ~ 0,
#        TRUE ~ NA_real_
#      ),
#
#      reps = n(),
#
#      n_mis_answers = sum(is.na(decision_binary)),
#
#      submodel = unique(submodel),
#
#      .by = c(studyid:topp)
#
#    )
#
#  if ("detailed_description" %in% names(answer_dat)){
#
#
#    long_answer_dat_sum <-
#      answer_dat |>
#      mutate(
#        incl_p = mean(decision_binary == 1, na.rm = TRUE),
#
#        final_decision_gpt_num = dplyr::case_when(
#          incl_p < incl_cutoff_upper & incl_p >= incl_cutoff_lower ~ 1,
#          incl_p >= incl_cutoff_upper ~ 1,
#          incl_p < incl_cutoff_lower ~ 0,
#          TRUE ~ NA_real_
#        ),
#
#        n_words_answer = stringr::str_count(detailed_description, '\\w+'),
#
#        .by = c(studyid:topp)
#
#      ) |>
#      filter(decision_binary == final_decision_gpt_num) |>
#      arrange(promptid, model, topp, iterations, studyid, desc(n_words_answer)) |>
#      summarise(
#        longest_answer = detailed_description[1],
#
#        .by = c(studyid:topp)
#
#      )
#
#    answer_dat_sum <-
#      left_join(sum_dat, long_answer_dat_sum) |>
#      suppressMessages() |>
#      relocate(longest_answer, .after = final_decision_gpt_num) |>
#      tibble::new_tibble(class = "chatgpt_tbl")
#
#
#
#  } else {
#
#    answer_dat_sum <- tibble::new_tibble(sum_dat, class = "chatgpt_tbl")
#
#  }
#
#  #  # Final data all
#  #  answer_dat <-
#  #    answer_dat |>
#  #    select(-req_per_min) |>
#  #    rename(top_p = topp)
#
#
#  # Final data sum
#  answer_dat_sum <-
#    dplyr::left_join(question_dat, answer_dat_sum) |>
#    suppressMessages() |>
#    select(-c(iterations, req_per_min)) |>
#    rename(top_p = topp) |>
#    tibble::new_tibble(class = "chatgpt_tbl")
#
#
#  if (token_info){
#
#    if (n_error > 0) {
#      res <- list(
#        price_data = price_dat,
#        price_dollar = price,
#        answer_data_all = answer_dat,
#        answer_data_sum = answer_dat_sum,
#        error_data = error_refs,
#        arguments_used = arg_list
#      )
#    } else {
#      res <- list(
#        price_data = price_dat,
#        price_dollar = price,
#        answer_data_all = answer_dat,
#        answer_data_sum = answer_dat_sum,
#        arguments_used = arg_list
#      )
#    }
#
#  } else {
#
#    if (n_error > 0) {
#      res <- list(answer_data_all = answer_dat, answer_data_sum = answer_dat_sum, error_data = error_refs, arguments_used = arg_list)
#    } else {
#      res <- list(answer_data_all = answer_dat, answer_data_sum = answer_dat_sum, arguments_used = arg_list)
#    }
#
#  }
#
#  class(res) <- c("list", "gpt")
#
#  res

}








