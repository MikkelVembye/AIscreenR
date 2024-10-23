#' @title Re-screen failed requests.
#'
#' @description
#' `r lifecycle::badge("experimental")`<br>
#' <br>
#'
#' This function supports re-screening of all failed title and abstract requests
#' screened with [tabscreen_gpt()]/[tabscreen_gpt.tools()].
#'
#' @references Wickham H (2023).
#' \emph{httr2: Perform HTTP Requests and Process the Responses}.
#' https://httr2.r-lib.org, https://github.com/r-lib/httr2.
#'
#' @param object An object of class `'gpt'`.
#' @param api_key Numerical value with your personal API key.
#' @param max_tries,max_seconds 'Cap the maximum number of attempts with
#'  `max_tries` or the total elapsed time from the first request with
#'  `max_seconds`. If neither option is supplied (the default), [req_perform()]
#'  will not retry' (Wickham, 2023). Default `max_tries` is 16. If missing, the value of `max_seconds`
#'  from the original screening conducted with [tabscreen_gpt()] will be used.
#' @param is_transient 'A predicate function that takes a single argument
#'  (the response) and returns `TRUE` or `FALSE` specifying whether or not
#'  the response represents a transient error' (Wickham, 2023). If missing, the `is_transient`
#'  function from the original screening conducted with [tabscreen_gpt() will be used.
#' @param backoff 'A function that takes a single argument (the number of failed
#'   attempts so far) and returns the number of seconds to wait' (Wickham, 2023).
#'   If missing, the `backoff`value from the original screening conducted with [tabscreen_gpt()] will be used.
#' @param after 'A function that takes a single argument (the response) and
#'   returns either a number of seconds to wait or `NULL`, which indicates
#'   that a precise wait time is not available that the `backoff` strategy
#'   should be used instead' (Wickham, 2023). If missing, the `after` value
#'   from the original screening conducted with [tabscreen_gpt()] will be used.
#' @param ... Further argument to pass to the request body. See \url{https://platform.openai.com/docs/api-reference/chat/create}.
#'  If used in the original screening in [tabscreen_gpt()], the argument(s)
#'  must be specified again here.
#'
#' @return An object of class `'gpt'` similar to the object returned by [tabscreen_gpt()].
#' See documentation for [tabscreen_gpt()].
#'
#' @seealso [tabscreen_gpt()], [tabscreen_gpt.tools()]
#'
#' @importFrom stats df
#' @import dplyr
#'
#' @examples
#'
#' \dontrun{
#' prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"
#'
#' obj_with_error <-
#'   tabscreen_gpt(
#'     data = filges2015_dat[1:10,],
#'     prompt = prompt,
#'     studyid = studyid,
#'     title = title,
#'     abstract = abstract,
#'     model = "gpt-4o"
#'     )
#'
#' obj_rescreened <-
#'   obj_with_error |>
#'   screen_error()
#'
#'}
#'
#' @export


screen_errors.gpt <- function(
    object,
    api_key = get_api_key(),
    max_tries = 16,
    max_seconds,
    is_transient,
    backoff,
    after,
    ...
){


  if (!"error_data" %in% names(object)){

    res <- object

  } else {


    # Add data and reps
    answer_data <- object$answer_data
    error_dat <- error_data <- object$error_data
    reps <- object$arguments_used$reps

    if (nrow(error_data) < nrow(answer_data)){

      prop_failed_resp <- nrow(error_data)/nrow(answer_data)
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
    seed <- object$arguments_used$seed
    progress <- object$arguments_used$progress
    messages <- object$arguments_used$messages
    incl_cutoff_upper <- object$arguments_used$incl_cutoff_upper
    incl_cutoff_lower <- object$arguments_used$incl_cutoff_lower
    force <- object$arguments_used$force
    fine_tuned <- object$arguments_used$fine_tuned

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
        reps = reps,
        seed = seed,
        progress = progress,
        messages = messages,
        incl_cutoff_upper = incl_cutoff_upper,
        incl_cutoff_lower = incl_cutoff_lower,
        force = force,
        fine_tuned = fine_tuned,
        ...
      )


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


    }

    # Pulling original iteration IDs to ensure correct order of the rescreened data
    org_n <- error_dat |> dplyr::pull(n)

    params <- error_dat |>
      dplyr::mutate(iterations = 1) |>
      dplyr::select(question, model_gpt = model, topp, iterations, req_per_min)

    question_dat <-
      error_dat |>
      dplyr::select(1:topp)

    furrr_seed <- if (is.null(seed)) TRUE else NULL

    error_dat_recovered <-
      question_dat |>
      dplyr::mutate(
        res = furrr::future_pmap(
          .l = params,
          .f = .rep_gpt_engine,
          role_gpt = role,
          tool = tools,
          t_choice = tool_choice,
          seeds = seed,
          time_inf = time_info,
          token_inf = token_info,
          apikey = api_key,
          maxt = max_tries,
          maxs = max_seconds,
          istrans = is_transient,
          ba = backoff,
          af = after,
          ...,
          .options = furrr::furrr_options(seed = furrr_seed),
          .progress = progress
        )
      ) |>
      tidyr::unnest(res) |>
      tibble::new_tibble(class = "gpt_tbl") |>
      mutate(n = org_n)

    # Amalgamate succes and error data
    if (nrow(error_data) < nrow(answer_data)){

      succes_dat <-
        answer_data |>
        dplyr::filter(!is.na(decision_binary))

      answer_dat <-
        dplyr::bind_rows(succes_dat, error_dat_recovered) |>
        dplyr::arrange(promptid, model, topp, iterations, studyid, n)

    } else {

      answer_dat <- error_dat_recovered

    }

    #.....................
    # Catching errors ----
    #.....................

    n_error <- answer_dat |> dplyr::filter(is.na(decision_binary)) |> nrow()

    if (messages){
      if (n_error == 1) message(paste("* NOTE: Requests failed for 1 title and abstract."))
      if (n_error > 1) message(paste("* NOTE: Requests failed", n_error, "times."))
    }

    # Adding error data
    error_dat <- if (n_error > 0) answer_dat |> dplyr::filter(is.na(decision_binary)) else NULL

    #.............................
    # Final price information ----
    #.............................

    # Adding price data
    price_dat <- if (token_info) price_gpt(answer_dat) else NULL
    price <- if (!is.null(price_dat)) sum(price_dat$total_price_dollar, na.rm = TRUE) else NULL

    #.........................................................................
    # Making aggregated data ----
    # Of primary importance when multiple iterations are used, i.e. reps > 1
    #.........................................................................

    # Adding the aggregated data
    if (any(reps > 1)) {

      # Get the aggregated
      answer_dat_sum <-
        .aggregate_res(
          answer_dat,
          incl_cutoff_u = incl_cutoff_upper,
          incl_cutoff_l = incl_cutoff_lower
        )

      # Final data sum
      answer_dat_aggregated <-
        dplyr::left_join(question_dat, answer_dat_sum) |>
        suppressMessages() |>
        dplyr::select(-c(iterations, req_per_min)) |>
        dplyr::rename(top_p = topp) |>
        tibble::new_tibble(class = "gpt_agg_tbl")

    } else {

      answer_dat_aggregated <- NULL

    }

    #.........................................
    # Returned output
    #.........................................

    res <- list(
      price_data = price_dat,
      price_dollar = price,
      answer_data = answer_dat,
      answer_data_aggregated = answer_dat_aggregated,
      error_data = error_dat,
      arguments_used = arg_list,
      run_date = Sys.Date()
    )

    # If token info is not wanted
    if (!token_info) res[["price_data"]] <- res[["price_dollar"]] <- NULL

    # If no screening errors
    if (n_error == 0) res[["error_data"]] <- NULL

    # Returned output without aggregated results
    if (all(reps == 1)) res[["answer_data_aggregated"]] <- NULL

    # Defining the class of the res object
    class(res) <- c("gpt", class(res))

  }

  res

}
