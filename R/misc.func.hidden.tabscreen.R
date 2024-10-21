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

################################################################
# Function to send repeated requests to OpenAI's GPT API models
################################################################

rep_gpt_engine <- function(
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
    ...
){

  # Setting tool_choice argument to body

  if (t_choice == "auto"){

    tools_choice = t_choice

  } else {

    tools_choice <- list(
      type = "function",
      "function" = list(
        name = t_choice
      )
    )

  }

  # Creating the body to be passed to gpt_engine()
  # The body is created here, it more readily work with the ... functionality.
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

  # Setting the iterations
  if(iterations > 1) iterations <- 1:iterations

  furrr_seed <- if (base::is.null(seeds)) TRUE else NULL

  # Running repeated in requests in parallel, and return tibble
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
    dplyr::mutate(n = iterations) # Given run id to each repetation.


}

################################################################################
# Function used to aggregate responses when repeating the same question is used.
################################################################################

aggregate_res <-
  function(data, incl_cutoff_u, incl_cutoff_l) {

    sum_dat <-
      data |>
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

        submodel = unique(submodel),

        .by = c(studyid:topp)

      )

    if ("detailed_description" %in% names(data)){


      long_answer_dat_sum <-
        data |>
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
        dplyr::arrange(promptid, model, topp, iterations, studyid, desc(n_words_answer)) |>
        dplyr::summarise(

          longest_answer = detailed_description[1],
          .by = c(studyid:topp)

        )

      answer_dat_sum <-
        left_join(sum_dat, long_answer_dat_sum) |>
        suppressMessages() |>
        dplyr::relocate(longest_answer, .after = final_decision_gpt_num) |>
        tibble::new_tibble(class = "gpt_tbl")



    } else {

      answer_dat_sum <- tibble::new_tibble(sum_dat, class = "gpt_tbl")

    }

    answer_dat_sum


  }


