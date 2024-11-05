#' @title Re-screen failed requests.
#'
#' @description
#' `r lifecycle::badge("deprecated")`<br>
#' <br>
#'
#' This function supports re-screening of all failed title and abstract requests
#' screened with [tabscreen_gpt.original()]. This function has been deprecated because
#' OpenAI has deprecated the function_call and and functions argument that was used
#' in [tabscreen_gpt.original()].
#'
#' @references Wickham H (2023).
#' \emph{httr2: Perform HTTP Requests and Process the Responses}.
#' https://httr2.r-lib.org, https://github.com/r-lib/httr2.
#'
#' @param object An object of class `'chatgpt'`.
#' @param ... Further argument to pass to the request body.
#'  See \url{https://platform.openai.com/docs/api-reference/chat/create}.
#'  If used in the original screening (e.g., with [tabscreen_gpt.original()]), the argument(s)
#'  must be specified again here.
#' @param api_key Numerical value with your personal API key.
#' @param max_tries,max_seconds 'Cap the maximum number of attempts with
#'  `max_tries` or the total elapsed time from the first request with
#'  `max_seconds`. If neither option is supplied (the default), [httr2::req_perform()]
#'  will not retry' (Wickham, 2023). Default `max_tries` is 4. If missing, the value of `max_seconds`
#'  from the original screening (e.g., conducted with [tabscreen_gpt.original()]) will be used.
#' @param is_transient 'A predicate function that takes a single argument
#'  (the response) and returns `TRUE` or `FALSE` specifying whether or not
#'  the response represents a transient error' (Wickham, 2023). If missing, the `is_transient`
#'  function from the original screening (e.g., conducted with [tabscreen_gpt.original()]) will be used.
#' @param backoff 'A function that takes a single argument (the number of failed
#'   attempts so far) and returns the number of seconds to wait' (Wickham, 2023).
#'   If missing, the `backoff`value from the original screening (e.g., conducted
#'   with [tabscreen_gpt.original()]) will be used.
#' @param after 'A function that takes a single argument (the response) and
#'   returns either a number of seconds to wait or `NULL`, which indicates
#'   that a precise wait time is not available that the `backoff` strategy
#'   should be used instead' (Wickham, 2023). If missing, the `after` value
#'   from the original screening (e.g., conducted with [tabscreen_gpt.original()]) will be used.
#'
#' @return Object of class `'chatgpt'` similar to the object returned by [tabscreen_gpt.original()].
#' See documentation value for [tabscreen_gpt.original()].
#'
#' @seealso [tabscreen_gpt.original()]
#'
#' @importFrom stats df
#' @import dplyr
#'
#'
#' @examples
#'
#' \dontrun{
#' prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"
#'
#' obj_with_error <-
#'   tabscreen_gpt(
#'     data = filges2015_dat[1:2,],
#'     prompt = prompt,
#'     studyid = studyid,
#'     title = title,
#'     abstract = abstract,
#'     model = c("gpt-3.5-turbo-0613", "gpt-3.5-turbo-16k-0613"),
#'     max_tries = 1,
#'     reps = 10
#'     )
#'
#' obj_rescreened <-
#'   obj_with_error |>
#'   screen_error()
#'
#' # Alternatively re-set max_tries if errors still appear
#' obj_rescreened <-
#'   obj_with_error |>
#'   screen_error(max_tries = 16)
#'}
#'
#' @export


screen_errors.chatgpt <- function(
    object,
    ...,
    api_key = get_api_key(),
    max_tries = 4,
    max_seconds,
    is_transient,
    backoff,
    after
){


  lifecycle::deprecate_warn("0.1.0", "screen_errors.chatgpt()", "screen_errors.gpt()")

  if (!"error_data" %in% names(object)){

    res <- object

  } else {

    if (nrow(object$error_data) < nrow(object$answer_data_all)){

      prop_failed_resp <- nrow(object$error_data)/nrow(object$answer_data_all)
      if(prop_failed_resp > .1) message("* Consider to increase max_tries to reduce error requests.")

    }

    role <- object$arguments_used$role
    functions <- object$arguments_used$functions
    function_call_name <- object$arguments_used$function_call_name
    time_info <- object$arguments_used$time_info
    token_info <- object$arguments_used$token_info
    #max_tries <- if (missing(max_tries)) object$arguments_used$max_tries else max_tries
    max_seconds <- if (missing(max_seconds)) object$arguments_used$max_seconds else max_seconds
    is_transient <- if (missing(is_transient)) object$arguments_used$is_transient else is_transient
    backoff <- if (missing(backoff)) object$arguments_used$backoff else backoff
    after <- if (missing(after)) object$arguments_used$after else after
    seed_par <- object$arguments_used$seed_par
    progress <- object$arguments_used$progress
    messages <- object$arguments_used$messages
    incl_cutoff_upper <- object$arguments_used$incl_cutoff_upper
    incl_cutoff_lower <- object$arguments_used$incl_cutoff_lower

    arg_list <-
      list(
        role = role,
        functions = functions,
        function_call_name = function_call_name,
        time_info = time_info,
        token_info = token_info,
        max_tries = max_tries,
        max_seconds = max_seconds,
        is_transient = is_transient,
        backoff = backoff,
        after = after,
        seed_par = seed_par,
        progress = progress,
        messages = messages,
        incl_cutoff_upper = incl_cutoff_upper,
        incl_cutoff_lower = incl_cutoff_lower,
        ...
      )

    #if ("max_tokens" %in% names(arg_list)){
    #  if (arg_list$max_tokens < 11) stop("Cannot retrieve results from server with tokens below 11.")
    #}

    # Further argument from ... [ADD]
    #    if ("max_tokens" %in% names(object$arguments_used)){
    #      max_tokens <- object$arguments_used$max_tokens
    #
    #      if (max_tokens < 11) stop("Cannot retrieve results from server with tokens below 11.")
    #
    #      arg_list$max_tokens <- max_tokens
    #
    #    } else {
    #      max_tokens <- NULL
    #    }

    # Consider !is.na(n) if errors appear
    error_dat <- object$error_data

    if(any(stringr::str_detect(error_dat$decision_gpt, "400"))){

      # IDs for studies with HTTP 400 error
      studyid_filter <-
        error_dat |>
        filter(stringr::str_detect(decision_gpt, "400")) |>
        pull(studyid) |>
        unique()

      # Error data
      error_400_dat <-
        error_dat |>
        filter(stringr::str_detect(decision_gpt, "400")) |>
        mutate(
          question = base::gsub("<.*?>", "", question),
          question = stringr::str_remove_all(question, "[:punct:]|[:symbol:]"),
          question = stringr::str_remove_all(question, "\\&")
        )


      error_other_dat <-
        error_dat |>
        filter(!stringr::str_detect(decision_gpt, "400")) |>
        mutate(
          question = if_else(studyid %in% studyid_filter, base::gsub("<.*?>", "", question), question),
          question = if_else(studyid %in% studyid_filter, stringr::str_remove_all(question, "[:punct:]|[:symbol:]"), question),
          question = if_else(studyid %in% studyid_filter, stringr::str_remove_all(question, "\\&"), question)
        )

      error_dat <-
        bind_rows(error_400_dat, error_other_dat) |>
        arrange(promptid, model, topp, iterations, studyid, n)

      # Aligning all and sum data set
      object$answer_data_all <-
        object$answer_data_all |>
        mutate(
          question = if_else(studyid %in% studyid_filter, base::gsub("<.*?>", "", question), question),
          question = if_else(studyid %in% studyid_filter, stringr::str_remove_all(question, "[:punct:]|[:symbol:]"), question),
          question = if_else(studyid %in% studyid_filter, stringr::str_remove_all(question, "\\&"), question)
        )

      object$answer_data_sum <-
        object$answer_data_sum |>
        mutate(
          question = if_else(studyid %in% studyid_filter, base::gsub("<.*?>", "", question), question),
          question = if_else(studyid %in% studyid_filter, stringr::str_remove_all(question, "[:punct:]|[:symbol:]"), question),
          question = if_else(studyid %in% studyid_filter, stringr::str_remove_all(question, "\\&"), question)
        )

    }


    error_recoved_dat <-
      tabscreen_gpt.original(
        data = error_dat,
        api_key = api_key,
        role = role,
        functions = functions,
        function_call_name = function_call_name,
        time_info = time_info,
        token_info = token_info,
        max_tries = max_tries,
        max_seconds = max_seconds,
        is_transient = is_transient,
        backoff = backoff,
        after = after,
        seed_par = seed_par,
        progress = progress,
        messages = messages,
        incl_cutoff_upper = incl_cutoff_upper,
        incl_cutoff_lower = incl_cutoff_lower,
        ...

      )$answer_data_all

    # Amalgamate succes and error data
    if (nrow(object$error_data) < nrow(object$answer_data_all)){

      succes_dat <-
        object$answer_data_all |>
        filter(!is.na(decision_binary))

      answer_dat <-
        bind_rows(succes_dat, error_recoved_dat) |>
        arrange(promptid, model, topp, iterations, studyid, n)

    } else {

      answer_dat <- error_recoved_dat

    }

    n_error <- answer_dat |> dplyr::filter(is.na(decision_binary)) |> nrow()

    if (messages){

      if (n_error == 1) message(paste("* NOTE: Re-screening requests failed for 1 title and abstract."))
      if (n_error > 1) message(paste("* NOTE: Re-screening requests failed", n_error, "times."))

    }

    if (n_error > 0) error_refs <- answer_dat |> dplyr::filter(is.na(decision_binary))

    if (token_info){

      price_dat <-
        answer_dat |>
        filter(!is.na(prompt_tokens) | !is.na(completion_tokens)) |>
        summarise(

          input_price_dollar = case_when(
            any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.0015/1000), 4),
            any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.003/1000), 4),
            any(c("gpt-4", "gpt-4-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.03/1000), 4),
            any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.06/1000), 4),
            TRUE ~ NA_real_
          ),

          output_price_dollar = case_when(
            any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.002/1000),
            any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.004/1000),
            any(c("gpt-4", "gpt-4-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.06/1000),
            any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.12/1000),
            TRUE ~ NA_real_
          ),

          price_total_dollar = round(input_price_dollar + output_price_dollar, 4),

          .by = c(model, iterations)

        )

      price <- sum(price_dat$price_total_dollar, na.rm = TRUE)

    }


    sum_dat <-
      answer_dat |>
      summarise(

        incl_p = mean(decision_binary == 1, na.rm = TRUE),

        final_decision_gpt = dplyr::case_when(
          incl_p < incl_cutoff_upper & incl_p >= incl_cutoff_lower ~ "Check",
          incl_p >= incl_cutoff_upper ~ "Include",
          incl_p < incl_cutoff_lower ~ "Exclude",
          TRUE ~ NA_character_
        ),

        final_decision_gpt_num = dplyr::case_when(
          incl_p < incl_cutoff_upper & incl_p >= incl_cutoff_lower ~ 1,
          incl_p >= incl_cutoff_upper ~ 1,
          incl_p < incl_cutoff_lower ~ 0,
          TRUE ~ NA_real_
        ),

        reps = n(),

        n_mis_answers = sum(is.na(decision_binary)),

        .by = c(studyid:topp)

      )

    if ("detailed_description" %in% names(answer_dat)){


      long_answer_dat_sum <-
        answer_dat |>
        mutate(
          incl_p = mean(decision_binary == 1, na.rm = TRUE),

          final_decision_gpt_num = dplyr::case_when(
            incl_p < incl_cutoff_upper & incl_p >= incl_cutoff_lower ~ 1,
            incl_p >= incl_cutoff_upper ~ 1,
            incl_p < incl_cutoff_lower ~ 0,
            TRUE ~ NA_real_
          ),

          n_words_answer = stringr::str_count(detailed_description, '\\w+'),

          .by = c(studyid:topp)

        ) |>
        filter(decision_binary == final_decision_gpt_num) |>
        arrange(promptid, model, topp, iterations, studyid, desc(n_words_answer)) |>
        summarise(
          longest_answer = detailed_description[1],

          .by = c(studyid:topp)

        )

      answer_dat_sum <-
        left_join(sum_dat, long_answer_dat_sum) |>
        suppressMessages() |>
        relocate(longest_answer, .after = final_decision_gpt_num) |>
        tibble::new_tibble(class = "chatgpt_tbl")



    } else {

      answer_dat_sum <- tibble::new_tibble(sum_dat, class = "chatgpt_tbl")

    }


    answer_dat_sum <-
      answer_dat_sum |>
      select(-c(iterations, req_per_min)) |>
      rename(top_p = topp)

    answer_data_sum_org <-
      object$answer_data_sum |>
      select(1:top_p, reps)

    # Final data sum
    # by = join_by(studyid, title, abstract, promptid, prompt, model, question, top_p, reps)
    answer_dat_sum <-
      left_join(answer_data_sum_org, answer_dat_sum) |>
      suppressMessages() |>
      relocate(reps, .before = n_mis_answers) |>
      tibble::new_tibble(class = "chatgpt_tbl")


    if (token_info){

      if (n_error > 0) {
        res <- list(
          price_data = price_dat,
          price_dollar = price,
          answer_data_all = answer_dat,
          answer_data_sum = answer_dat_sum,
          error_data = error_refs,
          arguments_used = arg_list
        )
      } else {
        res <- list(
          price_data = price_dat,
          price_dollar = price,
          answer_data_all = answer_dat,
          answer_data_sum = answer_dat_sum,
          arguments_used = arg_list
        )
      }

    } else {

      if (n_error > 0) {
        res <- list(answer_data_all = answer_dat, answer_data_sum = answer_dat_sum, error_data = error_refs, arguments_used = arg_list)
      } else {
        res <- list(answer_data_all = answer_dat, answer_data_sum = answer_dat_sum, arguments_used = arg_list)
      }

    }

    class(res) <- c("list", "chatgpt")

  }

  res

}
