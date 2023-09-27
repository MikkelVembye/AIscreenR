
#' @title Approximate price estimation for title and abstract screening using ChatGPT models
#'
#' @description
#' `r lifecycle::badge("experimental")`<br>
#' <br>
#' This function supports the approximation of the price of the total title and abstract
#' screening with ChatGPT. The function only provide approximately accurate pricing
#' estimates for screenings drawing on the function calling function `incl_function_simple` (Default)
#' since the detailed descriptions will increase the completion tokens with an unknown amount
#' which furthermore is rather model sensitive with gpt-4 models yielding longer
#' detailed responses relative to gpt-3.5-turbo models.
#'
#'
#' @template common-arg
#' @param model Character string with the name of the completion model. Can take
#'   multiple models, including gpt-4 models. Default = `"gpt-3.5-turbo-0613"`.
#'   Find available model at
#'   \url{https://platform.openai.com/docs/models/model-endpoint-compatibility}.
#' @param reps Numerical value indicating the number of times the same
#'   question should be sent to ChatGPT. This can be useful to test consistency
#'   between answers. Default is `1` but when using 3.5 models, we recommend setting this
#'   value to `10`.
#' @param top_p 'An alternative to sampling with temperature, called nucleus sampling,
#'   where the model considers the results of the tokens with top_p probability mass.
#'   So 0.1 means only the tokens comprising the top 10% probability mass are considered.
#'   We generally recommend altering this or temperature but not both.' (OPEN-AI). Default is 1.
#'   Find documentation at
#' \url{https://platform.openai.com/docs/api-reference/chat/create#chat/create-top_p}.
#'
#' @param token_word_ratio The multiplier used to approximate the number of tokens per word.
#'   Default is `1.6` which we empirically have found to be the average number of tokens per word.
#'
#' @return An object of class \code{"gpt_price"}. The object is a list containing the following
#' components:
#' \item{price}{numerical value indicating the total approximate price (in USD) of the screening across all gpt-models expected to be used for the screening.}
#' \item{price_data}{dataset with prices across all gpt models expected to be used for screening.}
#'
#' @importFrom stats df
#' @import dplyr
#' @export
#'
#' @examples
#' prompt <- "This is a prompt"
#'
#' app_price <- approximate_price_gpt(
#'   data = FFT_dat[1:2,],
#'   prompt = prompt,
#'   studyid = studyid,
#'   title = title,
#'   abstract = abstract,
#'   model = c("gpt-3.5-turbo-0613", "gpt-4"),
#'   reps = c(10, 1)
#' )
#'
#' app_price
#' app_price$price_dollar
#' app_price$price_data


approximate_price_gpt <-
  function(
    data,
    prompt,
    studyid,
    title,
    abstract,
    model = "gpt-3.5-turbo-0613",
    reps = 1,
    top_p = 1,
    token_word_ratio = 1.6
  ){

    if (length(reps) > 1 && length(model) != length(reps)){
      stop("model and reps must be of the same length.")
    }

    ###############################################
    # Data manipulation
    ###############################################


    if (missing(studyid)){

      dat <-
        data |>
        dplyr::mutate(
          studyid = 1:nrow(data)
        ) |>
        dplyr::relocate(studyid, .before = {{ title }})


    } else {

      dat <-
        data |>
        dplyr::mutate(
          studyid = {{ studyid }}
        ) |>
        dplyr::relocate(studyid, .before = {{ title }})

    }

    mp_reps <- if (length(reps) > 1) 1 else length(model)

    question_dat <-
      dat |>
      dplyr::mutate(
        dplyr::across(c({{ title }}, {{ abstract }}), ~ dplyr::if_else(
          is.na(.x) | .x == "" | .x == " ", "No information", .x, missing = "No information")
        )
      ) |>
      dplyr::slice(rep(1:nrow(dat), length(prompt))) |>
      dplyr::mutate(
        promptid = rep(paste("Prompt", 1:length(prompt)), each = dplyr::n_distinct(studyid)),
        prompt = rep(prompt, each = dplyr::n_distinct(studyid))
      ) |>
      dplyr::slice(rep(1:dplyr::n(), each = length(model))) |>
      dplyr::mutate(
        model = rep(model, dplyr::n_distinct(studyid)*dplyr::n_distinct(prompt)),
        iterations = rep(reps, dplyr::n_distinct(studyid)*dplyr::n_distinct(prompt)*mp_reps),
        #req_per_min = rep(rpm, dplyr::n_distinct(studyid)*dplyr::n_distinct(prompt)*mp_rpm),
        question_raw = paste0(
          prompt,
          " Now, evaluate the following title and abstract for",
          " Study ", studyid, ":",
          " -Title: ", {{ title }},
          " -Abstract: ", {{ abstract }}
        ),
        question = stringr::str_replace_all(question_raw, "\n\n", " "),
        question = stringr::str_remove_all(question, "\n")
      ) |>
      dplyr::select(-question_raw) |>
      dplyr::slice(rep(1:dplyr::n(), each = length(top_p))) |>
      mutate(
        topp = rep(top_p, n_distinct(studyid)*n_distinct(prompt)*n_distinct(model))
      )




    price_dat <-
      question_dat |>
      mutate(
        prompt_tokens = round(stringr::str_count(question, '\\w+') * 1.6),
        completion_tokens = 11 # Average number of completion tokens for the inclusion_decision_simple function

      ) |>
      filter(!is.na(prompt_tokens) | !is.na(completion_tokens)) |>
      dplyr::rowwise() |>
      mutate(

        input_price = case_when(
          any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ round(prompt_tokens * (0.0015/1000) * iterations, 4),
          any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ round(prompt_tokens * (0.003/1000) * iterations, 4),
          any(c("gpt-4", "gpt-4-0613") %in% model) ~ round(prompt_tokens * (0.03/1000) * iterations, 4),
          any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ round(prompt_tokens * (0.06/1000) * iterations, 4),
          TRUE ~ NA_real_
        ),

        output_price = case_when(
          any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ completion_tokens * (0.002/1000) * iterations,
          any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ completion_tokens * (0.004/1000) * iterations,
          any(c("gpt-4", "gpt-4-0613") %in% model) ~ completion_tokens * (0.06/1000) * iterations,
          any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ completion_tokens * (0.12/1000) * iterations,
          TRUE ~ NA_real_
        )

      ) |>
      ungroup() |>
      summarise(

        iterations = unique(iterations),
        input_price_dollar = sum(input_price, na.rm = TRUE),
        output_price_dollar = sum(output_price, na.rm = TRUE),
        total_price_dollor = round(input_price_dollar + output_price_dollar, 4),

        .by = model

      )


    price <- sum(price_dat$total_price_dollor, na.rm = TRUE)

    res <- list(price_data = price_dat, price_dollar = price)
    class(res) <- c("list", "gpt_price")

    res

}
