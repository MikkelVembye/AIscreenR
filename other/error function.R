#  if (n_error > 0){
#
#    succes_dat <- answer_dat |>
#      dplyr::filter(!is.na(decision_binary))
#
#    # _mod = modified
#    failed_dat <- answer_dat |>
#      dplyr::filter(is.na(decision_binary))
#
#    params_mod <-
#      failed_dat |>
#      mutate(iterations = 1) |>
#      select(question, model_gpt = model, topp, iterations, req_per_min)
#
#    error_dat <-
#      failed_dat |>
#      dplyr::select(1:topp, id = n) |>
#      dplyr::mutate(
#        res = furrr::future_pmap(
#          .l = params_mod,
#          .f = ask_gpt,
#          ...,
#          .options = furrr::furrr_options(seed = furrr_seed),
#          .progress = progress
#        )
#      ) |>
#      tidyr::unnest(res) |>
#      mutate(n = id) |>
#      select(-id) |>
#      relocate(n, .after = last_col())
#
#    answer_dat <-
#      dplyr::bind_rows(
#        succes_dat,
#        error_dat
#      ) |>
#      dplyr::arrange(promptid, model, topp, {{ arrange_var }})
#
#    still_error <- answer_dat |> dplyr::filter(is.na(decision_binary)) |> nrow()
#
#    if (messages){
#      if (still_error == 1) message(paste("* NOTE: Requests falied for 1 title and abstract."))
#      if (still_error > 1) message(paste("* NOTE: Requests falied for", still_error, "titles and abstracts."))
#    }
#
#    if (still_error > 0) error_refs <- answer_dat |> dplyr::filter(is.na(decision_binary))
#
#  }
#
#  n_error_refs <- answer_dat |> dplyr::filter(is.na(decision_binary)) |> nrow()
